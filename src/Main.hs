{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Taskwarrior.Annotation as Annot
import qualified Taskwarrior.IO         as Task
import           Taskwarrior.Task       (Task)
import qualified Taskwarrior.Task       as Task

import           Control.Monad.Except   (ExceptT, MonadError)
import qualified Control.Monad.Except   as Except
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, ReaderT)
import qualified Control.Monad.Reader   as Reader

import           Data.Default           (Default, def)
import           Data.Ini.Config        (IniParser)
import qualified Data.Ini.Config        as Ini
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import           Data.Time              (UTCTime, getCurrentTime)
import qualified Data.UUID              as UUID

import           Options.Applicative    (Parser)
import qualified Options.Applicative    as Opt

import qualified System.Directory       as Dir
import           System.FilePath        ((<.>), (</>))
import qualified System.IO              as IO
import qualified System.Process         as Process

-- CONFIG: Allow configuration via ~/.hasknoterc file --

data Config = Config
    { confEditor    :: Text
    , confViewer    :: Text
    , confLocation  :: Text
    , confExtension :: Text
    , confPrefix    :: Text
    } deriving (Show)

instance Default Config where
    def = Config "vi" "cat" "~/.task/notes" ".txt" "[hasknote]"

configParser :: IniParser Config
configParser =
    Ini.section "MAIN" $ do
        confEditor <- Ini.fieldDef "editor" (confEditor def)
        confViewer <- Ini.fieldDef "viewer" (confViewer def)
        confLocation <- Ini.fieldDef "location" (confLocation def)
        confExtension <- Ini.fieldDef "extension" (confExtension def)
        confPrefix <- Ini.fieldDef "prefix" (confPrefix def)
        return Config{..}

getConfig :: IO Config
getConfig = do
    confFilePath <- expandPath "~/.hasknoterc"
    confFileExists <- Dir.doesFileExist confFilePath
    if confFileExists
        then do
            confFile <- TIO.readFile confFilePath
            let parsedFile = Ini.parseIniFile ("[MAIN]\n" <> confFile) configParser
            case parsedFile of
                Right config ->
                    return config

                Left err ->
                    error $ "Failed to parse config file: " <> err
        else return def

-- COMMAND LINE ARGUMENTS --

type UseStdin = Bool

data Command
    = Edit UseStdin
    | View
    | Remove
  deriving (Show, Eq)

data Arguments = Arguments
    { argTaskId  :: Text
    , argCommand :: Command
    } deriving (Show)

argParser :: Parser Arguments
argParser =
    Arguments <$> taskIdArg <*> cmdArg
  where
    taskIdArg =
        Opt.argument Opt.str (Opt.metavar "TASK_ID")

    cmdArg =
        Opt.hsubparser $ mconcat
            [ Opt.command "edit"
                (Opt.info
                    (fmap Edit $
                        Opt.switch $ mconcat
                            [ Opt.long "stdin"
                            , Opt.help "Read note from stdin instead of opening an editor"
                            ])
                    (Opt.progDesc "Create or edit a note for a task"))
            , Opt.command "view"
                (Opt.info
                    (pure View)
                    (Opt.progDesc "View the note for a task if it exists"))
            , Opt.command "remove"
                (Opt.info
                    (pure Remove)
                    (Opt.progDesc "Remove the note for a task if it exists"))
            ]

parseArgs :: IO Arguments
parseArgs =
    Opt.customExecParser p opts
  where
    opts = Opt.info (Opt.helper <*> argParser) Opt.idm
    p = Opt.prefs Opt.disambiguate

-- WIRING --

data Env
    = Env Config Arguments
  deriving (Show)

getEnv :: IO Env
getEnv =
    Env <$> getConfig <*> parseArgs

newtype App a = App { unApp :: ReaderT Env (ExceptT Text IO) a }
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadError Text, MonadIO)

runApp :: Env -> App a -> IO (Either Text a)
runApp env app =
    Except.runExceptT $ Reader.runReaderT (unApp app) env

-- MAIN --

main :: IO ()
main = do
    env@(Env Config{..} Arguments{..}) <- getEnv
    liftIO $ expandPath (T.unpack confLocation) >>= Dir.createDirectoryIfMissing True
    tasks <- Task.getTasks [argTaskId]
    result <- case tasks of
        [] ->
            error $ "No task with ID '" <> T.unpack argTaskId <> "'"

        [task] ->
            runApp env $
                case argCommand of
                    Edit useStdin ->
                        edit useStdin task

                    View ->
                        view task

                    Remove ->
                        remove task

        _ ->
            error "Found multiple tasks. Are you sure you gave me a task ID?"
    case result of
        Right _ ->
            return ()

        Left err ->
            putStrLn $ T.unpack err

edit :: Bool -> Task -> App ()
edit useStdin task = do
    (Env Config{..} _) <- Reader.ask
    notePath <- getNotePath task
    if useStdin
       then liftIO $ do
           stdin <- getStdin
           TIO.writeFile notePath stdin
       else do
           let editorCmd = T.unpack confEditor
           liftIO $ Process.callProcess editorCmd [notePath]
    noteCreated <- liftIO $ Dir.doesFileExist notePath
    if noteCreated
        then do
            let taskNoAnnot = removeAnnotation confPrefix task
            liftIO $ do
                noteLines <- T.lines <$> TIO.readFile notePath
                entryTime <- getCurrentTime
                let
                    firstLine =
                        case noteLines of
                            [] ->
                                confPrefix

                            line:_ ->
                                confPrefix <> " " <> line
                    newTask = addAnnotation firstLine entryTime taskNoAnnot
                Task.saveTasks [ newTask ]
        else liftIO $ putStrLn "No note created."


view :: Task -> App ()
view task =
    ifNoteExists task $ do
        (Env Config{..} _) <- Reader.ask
        let viewCmd = T.unpack confViewer
        notePath <- getNotePath task
        liftIO $ Process.callProcess viewCmd [notePath]

remove :: Task -> App ()
remove task =
    ifNoteExists task $ do
        (Env Config{..} _) <- Reader.ask
        notePath <- getNotePath task
        liftIO $ do
            Process.callProcess "rm" [notePath]
            Task.saveTasks [ removeAnnotation confPrefix task ]

-- HELPERS --

ifNoteExists :: Task -> App () -> App ()
ifNoteExists task app = do
    notePath <- getNotePath task
    noteExists <- liftIO $ Dir.doesFileExist notePath
    if noteExists
        then app
        else liftIO $ putStrLn "There is no note for that task."

getNotePath :: Task -> App FilePath
getNotePath task = do
    (Env config _) <- Reader.ask
    let noteDir = confLocation config
        noteExt = confExtension config
        taskUuid = UUID.toText (Task.uuid task)
        notePath = T.unpack noteDir </> T.unpack taskUuid <.> T.unpack noteExt
    liftIO $ expandPath notePath

removeAnnotation :: Text -> Task -> Task
removeAnnotation prefix task =
    task {
        Task.annotations =
            filter
                ((/=prefix) . T.take (T.length prefix) . Annot.description)
                (Task.annotations task)
    }

addAnnotation :: Text -> UTCTime -> Task -> Task
addAnnotation description entryTime task =
    task {
        Task.annotations =
            Annot.Annotation entryTime description : Task.annotations task
    }

expandPath :: FilePath -> IO FilePath
expandPath path =
    case path of
      '~':rest -> do
          homeDir <- Dir.getHomeDirectory
          return (homeDir </> dropWhile (=='/') rest)
      p -> return p

getStdin :: IO Text
getStdin =
    T.strip <$> loop ""
  where
    loop acc = do
        done <- IO.isEOF
        if done
           then return acc
           else TIO.getLine >>= (\s -> loop (acc <> "\n" <> s))
