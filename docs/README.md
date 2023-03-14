# hoyo

[![BSD-3 license](https://img.shields.io/github/license/fpringle/hoyo)](https://github.com/fpringle/hoyo/blob/main/LICENSE)
[![Test workflow](https://github.com/fpringle/hoyo/actions/workflows/tests.yml/badge.svg)](https://github.com/fpringle/hoyo/actions/workflows/tests.yml)
[![GitHub Pages deployment](https://github.com/fpringle/hoyo/actions/workflows/pages/pages-build-deployment/badge.svg)](https://github.com/fpringle/hoyo/actions/workflows/pages/pages-build-deployment)

hoyo is a command-line utility that lets the user save directories as bookmarks (similar to in the browser) and easily `cd` to them.

# Contents

* [Installation](#installation)
* [Usage](#usage)
    * [Command-line options](#command-line-options)
    * [Examples](#examples)
    * [Configuration](#configuration)
        * [Default command](#default-command)
        * [Default bookmarks](#default-bookmarks)
* [Contributing](#contributing)

# Installation

## Download `hoyo-cli` and add to path

You can download binary directly [from GitHub releases](https://github.com/fpringle/hoyo/releases).

After downloading binary, make it executable and copy it under convenient location, for example:

```shell
chmod +x hoyo-cli
mv hoyo-cli ~/.local/bin/hoyo-cli
```

## Download shell config script and source

Download [this bash script](scripts/hoyo.sh).

Add the following line to your `.bashrc`:

```bash
source path/to/hoyo.sh
```

# Usage

## Command-line options

Running `hoyo --help` or `hoyo help` will display all the global default options,
as well as the available commands. For command-specific options, run
`hoyo <cmd> --help` or `hoyo help <cmd>`.

```
Set directory bookmarks for quick "cd" behaviour

Usage: hoyo [COMMAND] [-C|--config-file <file>] [-B|--bookmarks-file <file>]
            [--fail] [--nofail] [--time] [--notime] [--enable-clear]
            [--disable-clear] [--enable-reset] [--disable-reset]
            [--backup-before-clear] [--no-backup-before-clear]

  For more help on a particular sub-command, run `hoyo <cmd> --help`.

Available options:
  --version                Display version information and exit
  -C,--config-file <file>  Override the default config file
  -B,--bookmarks-file <file>
                           Override the default bookmarks file
  --fail                   Fail on error
  --nofail                 Disable fail on error
  --time                   Display bookmark creation times
  --notime                 Hide bookmark creation times
  --enable-clear           Enable the 'clear' command
  --disable-clear          Disable the 'clear' command
  --enable-reset           Enable the 'config reset' command
  --disable-reset          Disable the 'config reset' command
  --backup-before-clear    Backup the bookmarks file before running `hoyo clear`
  --no-backup-before-clear Don't backup the bookmarks file before running `hoyo
                           clear`
  -h,--help                Show this help text

Available commands:
  add                      Add a bookmark
  move                     Change directory using a bookmark
  list                     List existing bookmarks
  clear                    Clear all bookmarks
  delete                   Delete a bookmark
  refresh                  Re-calculate bookmark indices
  config                   View/manage hoyo config
  check                    Verify validity of config and bookmarks
  help                     Print a help message for the entire program or a
                           specific command

Bugs:
  If something went wrong unexpectedly or you think there's a problem with hoyo,
  let me know! To report an issue, create a bug report at
  https://github.com/fpringle/hoyo/issues

Online documentation:
  To read the web documentation, visit https://github.com/fpringle/hoyo#readme
```

## Examples

### List the current bookmarks

```shell
$ hoyo list
1. /home/Documents doc
2. /home/Music/Albums
```

### `cd` to a bookmark

```shell
$ hoyo move doc
$ pwd
/home/Documents
$ hoyo move 2
$ pwd
/home/Music/Albums
```

### Add a new bookmark

```shell
$ hoyo add /home h
$ hoyo list
1. /home/Documents doc
2. /home/Music/Albums
3. /home h
$ hoyo move h
$ pwd
/home
```

### Delete a bookmark

```shell
$ hoyo delete 2
$ hoyo list
1. /home/Documents doc
3. /home h
```

### Reset bookmark indices

```shell
$ hoyo refresh
$ hoyo list
1. /home/Documents doc
2. /home h
```

### View config

```shell
$ hoyo config print
fail_on_error = false
enable_clearing = true
backup_before_clear = false
display_creation_time = false
enable_reset = true
```

### Modify config

```shell
$ hoyo config set display_creation_time true
$ hoyo list
1. 12/26/22 16:55:13    /home/Documents doc
2. 12/27/22 12:01:55    /home h
```

### Reset config

```shell
$ hoyo config reset
$ hoyo config print
fail_on_error = false
enable_clearing = false
backup_before_clear = false
display_creation_time = false
enable_reset = false
```

### Clear all bookmarks

```shell
$ hoyo clear --enable-clear
$ hoyo list
[ no output ]
```

### Validate your config file

```shell
$ hoyo check
Config is good
Bookmarks file is good
```

## Configuration

The default config file is at `$XDG_CONFIG_HOME/hoyo/config.toml`, which is normally
`~/.config/hoyo/config.toml`. To use a different config file, see [Command-line options](#command-line-options).

```toml
# a sample hoyo config file, with explanations

# whether to backup the bookmarks file before running the `hoyo clear` command
backup_before_clear     = false

# whether to display bookmark creation time when running the `hoyo list` command
display_creation_time   = false

# set to False to disable the `hoyo clear` command as a safety mechanism
enable_clearing         = true

# set to False to disable the `hoyo config reset` command as a safety mechanism
enable_reset            = false

# whether to fail if we hit a non-fatal error e.g.  adding a bookmark that already exists
fail_on_error           = false

# optionally set a default command - see section "Default command"
default_command         = "list"

# optionally set a list of default bookmarks - see section "Default bookmarks"
[[default_bookmark]]
  directory             = "/home/me"

[[default_bookmark]]
  directory             = "/home/me/coding/haskell"
  name                  = "hask"
```

### Default bookmarks

You can optionally set a list of default bookmarks. When running `hoyo` for
the first time, or running `hoyo clear` to delete all the existing bookmarks,
the bookmarks list will be populated by the bookmarks in this list. A default
bookmark can have these fields:

- `directory`: required. The directory of the bookmark.
- `name`: optional. Give the bookmark a nickname for easier cd.

### Default command

If the `default_command` option is set, then running `hoyo` with no arguments
will actually run the default command.

For example, if `default_command = "list"`:

```shell
$ hoyo      # no arguments
1. /home/me
2. /home/me/coding/haskell      (hask)
```

If the `default_command` option is no set, then running `hoyo` with no arguments
is equivalent to running `hoyo --help`.

# Contributing

Please submit any bug reports or feature requests on the
[issues](https://github.com/fpringle/hoyo/issues) page.

When submitting pull requests, make sure you've done a few things first:

- If adding a new feature, make sure to add relevant tests.

- Install [pre-commit](https://pre-commit.com/) and run `pre-commit install`
inside the root directory of the repository. This sets up pre-commit hooks to
run useful scripts (in the [scripts](scripts/) directory) like linting, keeping
documentation up to date, and running tests.

- Update [CHANGELOG.md](CHANGELOG.md) under the
[Unreleased](CHANGELOG.md#unreleased) section with a short description of your changes.
