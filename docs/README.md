# hoyo

[![BSD-3 license](https://img.shields.io/github/license/fpringle/hoyo)](https://github.com/fpringle/hoyo/blob/main/LICENSE)
[![Test workflow](https://github.com/fpringle/hoyo/actions/workflows/tests.yml/badge.svg)](https://github.com/fpringle/hoyo/actions/workflows/tests.yml)

hoyo is a command-line utility that lets the user save directories as bookmarks (similar to in the browser) and easily `cd` to them.

# Installation

## Download `hoyo-cli` and add to path

You can download binary directly [from GitHub releases](https://github.com/fpringle/hoyo/releases).

After downloading binary, make it executable and copy it under convenient location, for example:

```
chmod +x hoyo-cli
mv hoyo-cli ~/.local/bin/hoyo-cli
```

## Download shell config script and source

Download [this bash script](scripts/hoyo.sh).

Add the following line to your `.bashrc`:

```
source path/to/hoyo.sh
```

# Usage

```
Set directory bookmarks for quick "cd"-like behaviour

Usage: hoyo [COMMAND] [-c|--config <file>] [-b|--bookmarks <file>] [--fail]
            [--nofail] [--time] [--notime] [--enable-clear] [--disable-clear]
            [--enable-reset] [--disable-reset]

  For more help on a particular sub-command, run `hoyo <cmd> --help`.

Available options:
  -c,--config <file>       Override the default config file
  -b,--bookmarks <file>    Override the default bookmarks file
  --fail                   Fail on error
  --nofail                 Disable fail on error
  --time                   Display bookmark creation times
  --notime                 Hide bookmark creation times
  --enable-clear           Enable the 'clear' command
  --disable-clear          Disable the 'clear' command
  --enable-reset           Enable the 'config reset' command
  --disable-reset          Disable the 'config reset' command
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
```

## Examples

### List the current bookmarks

```
$ hoyo list
1. /home/Documents doc
2. /home/Music/Albums
```

### `cd` to a bookmark

```
$ hoyo move doc
$ pwd
/home/Documents
$ hoyo move 2
$ pwd
/home/Music/Albums
```

### Add a new bookmark

```
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

```
$ hoyo delete 2
$ hoyo list
1. /home/Documents doc
3. /home h
```

### Reset bookmark indices

```
$ hoyo refresh
$ hoyo list
1. /home/Documents doc
2. /home h
```

### View config

```
$ hoyo config print
fail_on_error = false
enable_clearing = true
backup_before_clear = false
display_creation_time = false
enable_reset = true
```

### Modify config

```
$ hoyo config set display_creation_time true
$ hoyo list
1. 12/26/22 16:55:13    /home/Documents doc
2. 12/27/22 12:01:55    /home h
```

### Reset config

```
$ hoyo config reset
$ hoyo config print
fail_on_error = false
enable_clearing = false
backup_before_clear = false
display_creation_time = false
enable_reset = false
```

### Clear all bookmarks

```
$ hoyo clear --enable-clear
$ hoyo list
[ no output ]
```

### Validate your config file

```
$ hoyo check
Config is good
Bookmarks file is good
```
