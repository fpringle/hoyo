# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## [Unreleased]

### Added

### Fixed

### Changed

- Instead of building documentation through a GitHub action (this is slow since the runner needs to re-download Cabal, GHC etc every time), build docs using a pre-commit hook.

### Removed

## [0.1.0.1] - 2022-12-27

### Added

- Documentation at [GitHub Pages site](https://fpringle.github.io/hoyo/).
- CHANGELOG.md (this file).
- More fields in hoyo.cabal - now passes `cabal check`.

## [0.1.0.0] - 2022-12-27

### Added

- Initial release.

[unreleased]: https://github.com/fpringle/hoyo/compare/v0.1.0.1...HEAD
[0.1.0.1]: https://github.com/fpringle/hoyo/compare/v0.1.0.0...v0.1.0.1
[0.1.0.0]: https://github.com/fpringle/hoyo/releases/tag/v0.1.0.0
