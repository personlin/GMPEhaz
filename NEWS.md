# NEWS.md

## GMPEhaz 0.1.13.59

### Package compliance updates

* Fixed `DESCRIPTION`: corrected `Title` and `Description` text, removed deprecated `Maintainer` field,
  moved `lattice` from `Depends` to `Imports`, raised minimum R version to 4.1.0, updated
  `RoxygenNote` to 7.3.2, added `Config/testthat/edition: 3`, removed `| file LICENSE` from
  `License` field.
* Updated all `\url{http://dx.doi.org/...}` and `\url{http://doi.org/...}` documentation tags to
  the `\doi{}` Rd macro (58 occurrences across 30+ source files).
* Fixed repeated documentation typos: `momnet` → `moment`, `fytpe=` → `ftype=`,
  `response spetra` → `response spectra` (across many files).
* Removed invalid `@param libpath` tag from the package-level documentation (`R/GMPEhaz.R`).
* Removed `FIXME` comment block from `src/GMPEhaz_init.c`.
* Added `src/Makevars.win` for Windows build environment support.

### Test suite modernisation (testthat v3)

* Removed deprecated `context()` calls from all test files.
* Replaced `expect_that(length(x), equals(n))` with `expect_length(x, n)` in `test-LL08.R` and
  `test-Chao2017.R`.

### Code quality

* `R/atten2.R`: replaced `plot=F` with `plot=FALSE`, `log=T` with `log=TRUE`.
* `R/atten2.R`: replaced `1:(dim(Data)[1])` with `seq_len(nrow(Data))`.

### CI/CD

* Removed redundant `.github/workflows/r.yml` (superseded by `R-CMD-check.yml`).
