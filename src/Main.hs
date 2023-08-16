{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Taskwarrior.Annotation    as Annot
import qualified Taskwarrior.IO            as Task
import           Taskwarrior.Task          (Task)
import qualified Taskwarrior.Task          as Task

import           Control.Monad.Except      (ExceptT, MonadError)
import qualified Control.Monad.Except      as Except
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.Reader      (MonadReader, ReaderT)
import qualified Control.Monad.Reader      as Reader

import qualified Data.Aeson                as Aeson
import qualified Data.Bifunctor            as BF
import qualified Data.ByteString.Lazy.UTF8 as LBS
import           Data.Default              (Default, def)
import           Data.Ini.Config           (IniParser)
import qualified Data.Ini.Config           as Ini
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.IO              as TIO
import qualified Data.Set                  as Set
import           Data.Time                 (UTCTime, getCurrentTime)
import qualified Data.UUID                 as UUID

import           Options.Applicative       (Parser)
import qualified Options.Applicative       as Opt

import qualified System.Directory          as Dir
import           System.FilePath           ((<.>), (</>))
import qualified System.IO                 as IO
import qualified System.Process            as Process

-- CONFIG: Allow configuration via ~/.hasknoterc file --

data Config = Config
    { confEditor           :: String
    , confViewer           :: String
    , confLocation         :: FilePath
    , confExtension        :: String
    , confPrefix           :: Text
    , confSkipHooksOnQuery :: Bool
    } deriving (Show)

instance Default Config where
    def = Config "vi" "cat" "~/.task/notes" ".txt" "[hasknote]" True

configParser :: IniParser Config
configParser =
    Ini.section "MAIN" $ do
        confEditor <- Ini.fieldDefOf "editor" Ini.string (confEditor def)
        confViewer <- Ini.fieldDefOf "viewer" Ini.string (confViewer def)
        confLocation <- Ini.fieldDefOf "location" Ini.string (confLocation def)
        confExtension <- Ini.fieldDefOf "extension" Ini.string (confExtension def)
        confPrefix <- Ini.fieldDef "prefix" (confPrefix def)
        confSkipHooksOnQuery <- Ini.fieldFlagDef "skipHooksOnQuery" (confSkipHooksOnQuery def)
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

getTask :: Text -> App (Maybe Task)
getTask taskId = do
    (Env Config{confSkipHooksOnQuery} _) <- Reader.ask
    let withHooks = [ "rc.hooks=0" | confSkipHooksOnQuery ]
        proc =
            (Process.proc "task" (withHooks <> [T.unpack taskId, "export"]))
                { Process.std_out = Process.CreatePipe
                , Process.std_err = Process.CreatePipe
                }
    taskData <- liftIO $ Process.readCreateProcess proc ""
    tasks <- Except.liftEither $ BF.first T.pack $ Aeson.eitherDecode $ LBS.fromString taskData
    case tasks of
        []  -> return Nothing
        [t] -> return $ Just t
        _ -> Except.throwError "Found multiple tasks. Are you sure you gave me a task ID?"

-- MAIN --

main :: IO ()
main = do
    env@(Env Config{..} Arguments{..}) <- getEnv
    expandPath confLocation >>= Dir.createDirectoryIfMissing True
    out <- runApp env $ do
        mbTask <- getTask argTaskId
        case mbTask of
            Nothing ->
                Except.throwError $ "No task with ID '" <> argTaskId <> "'."

            Just task ->
                case argCommand of
                    Edit useStdin ->
                        edit useStdin task

                    View ->
                        view task

                    Remove ->
                        remove task
    case out of
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
       else liftIO $ Process.callProcess confEditor [notePath]
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
        notePath <- getNotePath task
        liftIO $ Process.callProcess confViewer [notePath]

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
        taskUuid = UUID.toString (Task.uuid task)
        notePath = noteDir </> taskUuid <.> noteExt
    liftIO $ expandPath notePath

removeAnnotation :: Text -> Task -> Task
removeAnnotation prefix task =
    task {
        Task.annotations =
            Set.filter
                ((/=prefix) . T.take (T.length prefix) . Annot.description)
                (Task.annotations task)
    }

addAnnotation :: Text -> UTCTime -> Task -> Task
addAnnotation description entryTime task =
    task {
        Task.annotations =
            Set.insert (Annot.Annotation entryTime description) (Task.annotations task)
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
