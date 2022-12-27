# hoyo

hoyo is a command-line utility that lets the user save directories as bookmarks (similar to in the browser) and easily `cd` to them.

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

```man
hoyo [--version] COMMAND [-c|--config FILE] [-b|--bookmarks FILE] 
            [--fail] [--nofail] [--time] [--notime] [--enable-clear] 
            [--disable-clear] [--enable-reset] [--disable-reset]

  Set directory bookmarks for quick "cd"-like behaviour

Available options:
  -c,--config FILE         Override the default config file
  -b,--bookmarks FILE      Override the default bookmarks file
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
```

## Examples

### List the current bookmarks

```bash
$ hoyo list
1. /home/Documents doc
2. /home/Music/Albums
```

### `cd` to a bookmark

```bash
$ hoyo move doc
$ pwd
/home/Documents
$ hoyo move 2
$ pwd
/home/Music/Albums
```

### Add a new bookmark

```bash
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

```bash
$ hoyo delete 2
$ hoyo list
1. /home/Documents doc
3. /home h
```

### Reset bookmark indices

```bash
$ hoyo refresh
$ hoyo list
1. /home/Documents doc
2. /home h
```

### View config

```bash
$ hoyo config print
fail_on_error = false
enable_clearing = true
backup_before_clear = false
display_creation_time = false
enable_reset = true
```

### Modify config

```bash
$ hoyo config set display_creation_time true
$ hoyo list
1. 12/26/22 16:55:13    /home/Documents doc
2. 12/27/22 12:01:55    /home h
```

### Reset config

```bash
$ hoyo config reset
$ hoyo config print
fail_on_error = false
enable_clearing = false
backup_before_clear = false
display_creation_time = false
enable_reset = false
```

### Clear all bookmarks

```bash
$ hoyo clear --enable-clear
$ hoyo list
[ no output ]
```
