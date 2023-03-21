# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## [Unreleased]

### Added

- Prompt the user for confirmation before performing a destructive action, but only if:
        1. We're not going to backup first, AND
        2. The input handle is a terminal.

- Catch IO exceptions in TOML file encoding/decoding functions; wrap them in `HoyoException`.

### Fixed

- Add a newline to pager output.

### Changed

- Got rid of all the unnecessary `Internal/*` modules.
- Switch to using a custom Exception type instead of just `Text`.
- Format the search term in error messages.

### Removed

## [0.5.1.1] - 2023-03-20

### Fixed
- Updated nix files with new dependencies.

### Changed

## [0.5.1.0] - 2023-03-20

### Added

- When the output is longer than a page, and the output is a TTY, then pass output to a pager.
    Otherwise, print normally.
- Added `--json` flag to `hoyo list` and `hoyo config print`.

## [0.5.0.0] - 2023-03-14

### Added

- Integration with Nix.
- Help message footer - links to online docs and bug tracker.
- `hoyo help <command>` command - does the same thing as `hoyo --help` and `hoyo <command> --help`.

### Fixed

- Show help text when the user runs `hoyo` but doesn't have a default command set.

### Changed

- Undid some of the obsessive usage of `T.Text`. For example, got rid of `TFilePath`.

### Removed

- Get rid of the "catch-all" `move` command
    (see [here](https://clig.dev/#subcommands:~:text=Don%E2%80%99t%20have%20a%20catch%2Dall%20subcommand.)
    for more information).

## [0.4.0.0] - 2023-02-10

### Added

- Tests for parsing command-line options (round-trip property).
- Prints to stderr are now colorised red.
- Added command-line flags to override the "backup_before_clear" config option.

### Changed

- Moved CLI modules "Complete" and "Parse" into the main library.
    This lets us test the parsing and completion code.
- Changed global option flags:
    - [-c|--config]     -> [-C|--config-file]
    - [-b|--bookmarks]  -> [-B|--bookmarks-file]
- Changed the default_command config value from Text to Command.
    This makes it much cleaner to parse the default command from the
    config and to run it.

    However this incurs a cyclic dependency (we have to import the optparse parser
    to decode the defaul command from TOML), which we resolve using
    [Parse.hs-boot](./src/HoYo/Internal/Parse.hs-boot).
- Moved all the data definitions in [Command.hs](./src/HoYo/Internal/Command.hs)
    to [Types.hs](./src/HoYo/Internal/Types.hs)

### Removed

- `testDirectoryUnique`: allows user to have multiple bookmarks to the same directory.
- `ExecResult`: no longer needed now that we parse the default command directly.

## [0.3.0.0] - 2023-01-27

### Added

- `Arbitrary` instances for test suite.
- Added property tests for functions in Bookmarks.hs and Utils.hs.
- Integrate [headroom](https://github.com/vaclavsvejcar/headroom).
- Some util functions for testing in HoYo/Test/HoYo.hs.
- Add more bookmark, utils and env tests.
- Better pretty printing in `hoyo config print` command.
- Stricter config type safety using GADTs parameterised by `ConfigValueType`.
- Arbitrary values for `ConfigValue`.

### Fixed

- Make sure that readInt and readBool parse the entire string.
- Add "default_command" to list of config keys in `configKeyCompleter`

### Changed

- Refactored uniqueness tests in `hoyo add` to their own functions.
- `Config` now uses `ConfigValue`s instead of raw `Bool`/`Text` etc.

### Removed

- All the auxiliary pretty-printing functions that operated on Toml values.

## [0.2.3.1] - 2023-01-21

### Changed

- Update the year in `versionString`
- Update README.md

## [0.2.3.0] - 2023-01-21

### Added

- add haddock module header fields to all exported modules.
- let the user set a default command. If the user has set
    `default_command = "list"` in their `hoyo.toml` config file,
    then running `hoyo` is equivalent to running `hoyo list`.
    Otherwise, running `hoyo` will show the help screen.

### Changed

- `getEnvAndRunCommand` now returns `ExecResult` instead of `()`.
- `Config` has a new field `_defaultCommand` which corresponds to the option
    `default_command` in hoyo.toml.

## [0.2.2.1] - 2023-01-20

### Changed

- Fix a bug in Main.hs that would print the Options object before running the appropriate command.

## [0.2.2.0] - 2023-01-19

### Added

- Show instances for the various command and option types. Mostly for debugging.
- Make `hoyo move` the default command. In other words, if there is an existing bookmark
    with the nickname `docs`, then running `hoyo docs` is the equivalent to running
    `hoyo move docs`. Likewise if the bookmark index is used.

## [0.2.1.0] - 2022-12-31

### Added

- Added a new command `config add-default`: add a new default bookmark to the config.

### Changed

- Split Main.exe into different modules for parsing and completion.
- Made the parser metavariables a bit nicer in the help text, e.g. <file>
instead of FILE.

## [0.2.0.0] - 2022-12-30

### Added

- Added an array table "default_bookmark" to config: these bookmarks
will be added to the bookmark list on init/clear.
- Added utility formatting functions for Bookmarks.
- Added custom Bash completions.
- `check` command: verify the config and bookmark files.
- Added withFiles function.

### Fixed

- Add checks in `addMove` to verify bookmark names are valid.

### Changed

- Changed dependency from lens-simple to microlens.
- Rename "bookmarks" to "bookmark" in bookmarks.toml.
- Use Text instead of String everywhere.
- Move `versionString` to its own module.

### Removed

## [0.1.1.0] - 2022-12-28

### Changed

- Fixed some dependency bounds in hoyo.cabal.

### Removed

- Todo.md.

## [0.1.0.2] - 2022-12-27

### Added

- Start of a test suite.
- Pre-commit hook to run tests.
- GitHub Action to run tests.
- Pre-commit hook for linting (scripts/lint.sh).

### Changed

- Instead of building documentation through a GitHub action
(this is slow since the runner needs to re-download Cabal, GHC etc every time),
build docs using a pre-commit hook.

### Removed

- Removed Todo.md - it can stay out of version control, and CHANGELOG.md can document WIPs.
- Removed scripts/(un)deploy.

## [0.1.0.1] - 2022-12-27

### Added

- Documentation at [GitHub Pages site](https://fpringle.github.io/hoyo/).
- CHANGELOG.md (this file).
- More fields in hoyo.cabal - now passes `cabal check`.

## [0.1.0.0] - 2022-12-27

### Added

- Initial release.

[unreleased]: https://github.com/fpringle/hoyo/compare/v0.5.1.0...HEAD
[0.5.1.1]: https://github.com/fpringle/hoyo/compare/v0.5.1.0...v0.5.1.1
[0.5.1.0]: https://github.com/fpringle/hoyo/compare/v0.5.0.0...v0.5.1.0
[0.5.0.0]: https://github.com/fpringle/hoyo/compare/v0.4.0.0...v0.5.0.0
[0.4.0.0]: https://github.com/fpringle/hoyo/compare/v0.3.0.0...v0.4.0.0
[0.3.0.0]: https://github.com/fpringle/hoyo/compare/v0.2.3.1...v0.3.0.0
[0.2.3.1]: https://github.com/fpringle/hoyo/compare/v0.2.3.0...v0.2.3.1
[0.2.3.0]: https://github.com/fpringle/hoyo/compare/v0.2.2.1...v0.2.3.0
[0.2.2.1]: https://github.com/fpringle/hoyo/compare/v0.2.2.0...v0.2.2.1
[0.2.2.0]: https://github.com/fpringle/hoyo/compare/v0.2.1.0...v0.2.2.0
[0.2.1.0]: https://github.com/fpringle/hoyo/compare/v0.2.0.0...v0.2.1.0
[0.2.0.0]: https://github.com/fpringle/hoyo/compare/v0.1.1.0...v0.2.0.0
[0.1.1.0]: https://github.com/fpringle/hoyo/compare/v0.1.0.2...v0.1.1.0
[0.1.0.2]: https://github.com/fpringle/hoyo/compare/v0.1.0.1...v0.1.0.2
[0.1.0.1]: https://github.com/fpringle/hoyo/compare/v0.1.0.0...v0.1.0.1
[0.1.0.0]: https://github.com/fpringle/hoyo/releases/tag/v0.1.0.0
