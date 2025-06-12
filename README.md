[![Build Status](https://travis-ci.org/personlin/GMPEhaz.svg?branch=master)](https://travis-ci.org/personlin/GMPEhaz) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/personlin/GMPEhaz?branch=master&svg=true)](https://ci.appveyor.com/project/personlin/GMPEhaz) [![codecov](https://codecov.io/gh/personlin/GMPEhaz/branch/master/graph/badge.svg)](https://codecov.io/gh/personlin/GMPEhaz) [![Join the chat at https://gitter.im/GMPEhaz/Lobby](https://badges.gitter.im/GMPEhaz/Lobby.svg)](https://gitter.im/GMPEhaz/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

# GMPEhaz

`GMPEhaz` is an R package that wraps the Ground Motion Prediction Equation (GMPE) routines from Norman Abrahamson's [HAZ](https://github.com/abrahamson/HAZ) Fortran code. It provides R interfaces to a large collection of crustal and subduction GMPE models and utilities for working with them.

## Features

- Implements more than twenty published GMPE models including LL08, ASK14, CY14 and many others.
- Generic wrapper function `atten2()` to evaluate models over multiple periods or scenarios.
- Data sets `periods.T` and `GMPEhaz.model` describing available periods for each model.
- Unit tests using `testthat` to verify key functionality.

## Installation

```r
install.packages("devtools")
devtools::install_github("personlin/GMPEhaz")
```

## Example

```r
library(GMPEhaz)

# Compute PGA on rock for a M6 interface event at 20 km
res <- LL08Rock(6, 20, 10, 0, 0)
pga <- exp(res$lnY) / exp(6.89)
print(pga)
```

```r
# Evaluate multiple magnitudes using the generic wrapper
vals <- atten2("LL08Rock", 0, Mag = c(6, 7), Rrup = c(20, 20), depth = 10, ftype = 0, plot = FALSE)
head(vals)
```

## Documentation

Additional documentation, including a vignette, is available in the `vignettes/` folder of this repository. The package manual pages provide details for each GMPE implementation.

## License

GMPEhaz is distributed under the terms of the GPL-3 license.
