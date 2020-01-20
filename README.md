hasknote
========

A Haskell clone of [tasknote](https://github.com/mikebobroski/tasknote) with extra features and nicer configuration. It allows the user to associate a text file with a [Taskwarrior](https://taskwarrior.org/) task. When a note is added, the task will be annotated with the first line of the note. This annotation will be updated with each call to `hasknote edit`.

## Installation

### Building with Nix

Using the [Nix package manager](https://nixos.org/nix/):

1. Clone the project and `cd` into its directory.
2. Run `nix build`
3. The compiled executable will be located at `./result/bin/hasknote`.

### Building with Cabal

Using Haskell's [Cabal](https://www.haskell.org/cabal/):

1. Clone the project and `cd` into its directory.
2. Run `cabal --enable-nix v1-configure && cabal --enable-nix v1-build`.
3. The compiled executable will be located at `./dist/build/hasknote/hasknote`.

## Usage

```
Usage: hasknote TASK_ID COMMAND

Available options:
  -h,--help                Show this help text

Available commands:
  edit [--stdin]           Create or edit a note for a task, optionally reading
                           from stdin instead of opening an editor
  view                     View the note for a task if it exists
  remove                   Remove the note for a task if it exists
```

## Configuration

You can configure hasknote via a `~/.hasknoterc` file. Options are:

- `editor` - The text editor you edit your note in. Default: `editor = vi`.
- `viewer` - The program used to view your note. Default: `viewer = cat`.
- `location` - The directory where your notes will be stored. If the directory does not exist, it will be created. Default: `location = ~/.task/notes`.
- `extension` - The file extension for your notes. Default: `extension = .txt`.
- `prefix` - The prefix of hasknote's task annotations, used by hasknote to detect and remove the annotations later. Default: `prefix = [hasknote]`.
- `skipHooksOnQuery` - Whether to run the `task` command with `rc.hooks=0` when querying task data. Increases the speed of `hasknote`, especially if you have hooks that take a while. This option does not affect the `task` command when a task is being modified, in which case hooks are always run. Default: `skipHooksOnQuery = True`.
