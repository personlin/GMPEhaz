[![R-CMD-check](https://github.com/personlin/GMPEhaz/workflows/R-CMD-check/badge.svg)](https://github.com/personlin/GMPEhaz/actions/workflows/R-CMD-check.yml) [![Codecov test coverage](https://codecov.io/gh/personlin/GMPEhaz/branch/master/graph/badge.svg)](https://app.codecov.io/gh/personlin/GMPEhaz?branch=master) [![Join the chat at https://gitter.im/GMPEhaz/Lobby](https://badges.gitter.im/GMPEhaz/Lobby.svg)](https://gitter.im/GMPEhaz/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

# GMPEhaz

`GMPEhaz` is an R package that wraps the Ground Motion Prediction Equation (GMPE) routines from Norman Abrahamson's [HAZ](https://github.com/abrahamson/HAZ) Fortran code. It provides R interfaces to a large collection of crustal and subduction GMPE models and utilities for working with them.

## Features

- Implements more than twenty published GMPE models including LL08, ASK14, CY14 and many others.
- Data sets `periods.T` and `GMPEhaz.model` describing available periods for each model.

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

## Documentation

Additional documentation, including a vignette, is available in the `vignettes/` folder of this repository. The package manual pages provide details for each GMPE implementation.

## License

GMPEhaz is distributed under the terms of the GPL-3 license.
