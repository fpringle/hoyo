# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## [Unreleased]

### Added

### Fixed

### Changed

- Split Main.exe into different modules for parsing and completion.

### Removed

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

[unreleased]: https://github.com/fpringle/hoyo/compare/v0.2.0.0...HEAD
[0.2.0.0]: https://github.com/fpringle/hoyo/compare/v0.1.1.0...v0.2.0.0
[0.1.1.0]: https://github.com/fpringle/hoyo/compare/v0.1.0.2...v0.1.1.0
[0.1.0.2]: https://github.com/fpringle/hoyo/compare/v0.1.0.1...v0.1.0.2
[0.1.0.1]: https://github.com/fpringle/hoyo/compare/v0.1.0.0...v0.1.0.1
[0.1.0.0]: https://github.com/fpringle/hoyo/releases/tag/v0.1.0.0
