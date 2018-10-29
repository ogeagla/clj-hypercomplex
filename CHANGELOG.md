# Change Log
All notable changes to this project will be documented in this file. This change log follows the conventions of [keepachangelog.com](http://keepachangelog.com/).


## [0.0.3] - 2018-10-29
### Changed
- Added Apache Commons `Complex` type hints to remove reflection in core namespace, which yielded a 5-10x speedup for `:apache` implementation.
## [0.0.2] - 2018-10-19
### Added
- `n-hypercomplex` function which constructs a hypercomplex number based on provided coefficients, which can be of arbitrary order, so long as it is greater than 1 and a power of 2.
## [0.0.1] - 2018-10-7
### Added
- Project import.

[Unreleased]: https://github.com/ogeagla/clj-hypercomplex/compare/0.0.1...HEAD
[0.0.3]: https://github.com/ogeagla/clj-hypercomplex/compare/0.0.2...0.0.3
[0.0.2]: https://github.com/ogeagla/clj-hypercomplex/compare/0.0.1...0.0.2
[0.0.1]: https://github.com/ogeagla/clj-hypercomplex/tags/0.0.1
