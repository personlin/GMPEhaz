[![R-CMD-check](https://github.com/personlin/GMPEhaz/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/personlin/GMPEhaz/actions/workflows/R-CMD-check.yml)
[![Codecov test coverage](https://codecov.io/gh/personlin/GMPEhaz/branch/master/graph/badge.svg)](https://app.codecov.io/gh/personlin/GMPEhaz?branch=master)
[![License: GPL-3](https://img.shields.io/badge/License-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

# GMPEhaz

`GMPEhaz` is an R package that provides wrapper functions for Ground Motion Prediction Equation (GMPE) routines from Norman Abrahamson's [HAZ](https://github.com/abrahamson/HAZ) Fortran code. It covers more than 100 GMPE implementations across crustal, subduction interface, and subduction intraslab source types, including NGA-West2 models and Taiwan-specific adjustments.

- **Package site**: <https://personlin.github.io/GMPEhaz/>
- **Bug reports**: <https://github.com/personlin/GMPEhaz/issues>

## Installation

```r
# install.packages("pak")
pak::pak("personlin/GMPEhaz")
```

Or using `devtools`:

```r
# install.packages("devtools")
devtools::install_github("personlin/GMPEhaz")
```

## Quick Start

```r
library(GMPEhaz)

# NGA-West2: ASK14 — median Sa at T = 1 s for M7, Rrup = 50 km, Vs30 = 760 m/s
res <- ASK14(M = 7, Rrup = 50, Rjb = 48, Rx = 10, Ztor = 5,
             W = 15, dip = 80, Vs30 = 760, Z1.0 = 0.05,
             rake = 0, FRV = 0, FNM = 0, Finferred = 1,
             Fmeasured = 0, region = 0, T = 1.0)
cat("lnSa =", res$lnY, "\n")

# Taiwan LL08 model — PGA on rock for a M6.5 event at Rrup = 30 km
res_rock <- LL08Rock(M = 6.5, Rrup = 30, H = 10, FaultType = 0, FR = 0)
cat("lnPGA =", res_rock$lnY, "\n")

# List all available GMPE models and their supported spectral periods
head(GMPEhaz.model)

# Plot attenuation curve for a single model
atten2(M = 6.5, Rrup_range = c(1, 300), model = "LL08Rock")
```

## GMPE Models

### Crustal — Global

| Function | Reference |
|---|---|
| `AB03` | Atkinson & Boore (2003) |
| `AC10` | Atkinson & Casey (2010) |
| `AM09` | Atkinson & Morrison (2009) |
| `Arroyo2010` | Arroyo et al. (2010) |
| `ASK14` | Abrahamson, Silva & Kamai (2014) — NGA-West2 |
| `ASB14` | Atkinson, Stewart & Boore (2014) |
| `BI14` | Bindi et al. (2014) |
| `BSSA14` | Boore, Stewart, Seyhan & Atkinson (2014) — NGA-West2 |
| `Camp03H` | Campbell (2003) horizontal component |
| `CB14` | Campbell & Bozorgnia (2014) — NGA-West2 |
| `CY08` | Chiou & Youngs (2008) |
| `CY14` | Chiou & Youngs (2014) — NGA-West2 |
| `Garcia05` | Garcia et al. (2005) |
| `GK16` | Graizer & Kalkan (2016) |
| `Gregor06` | Gregor et al. (2006) |
| `I14` | Idriss (2014) — NGA-West2 |
| `KAAH15` | Kale et al. (2015) |
| `LL08`, `LL08Rock`, `LL08Soil` | Lin & Lee (2008) |
| `LL08.C02`, `LL08.F04` | Lin & Lee (2008) site-class variants |
| `Lin09` | Lin (2009) |
| `Lin11`, `Lin11Rock`, `Lin11Soil` | Lin (2011) |
| `Loh96` | Loh et al. (1996) |
| `MBR17` | Manea et al. (2017) |
| `NCREE2011` | NCREE (2011) |
| `PhungCru17`, `PhungCru18` | Phung et al. (2017, 2018) crustal |
| `Sinotech12` | Sinotech (2012) |
| `Youngs97Rock`, `Youngs97Soil` | Youngs et al. (1997) |
| `Zh06` | Zhao et al. (2006) |
| `Zh16Cru` | Zhao et al. (2016) crustal |

### Crustal — Taiwan-specific Adjustments

NGA-West2 models adjusted for Taiwan ground motion characteristics:

| Function | Base Model | Adjustment |
|---|---|---|
| `AGA16.tw.C01`, `AGA16.tw.F10` | Atkinson et al. (2016) | site class C01 / F10 |
| `ASB14.tw.B01`, `.tw.C01`, `.tw.E02` | ASB14 | B01 / C01 / E02 |
| `ASK14.tw.B01`, `.tw.C01`, `.tw.E03` | ASK14 | B01 / C01 / E03 |
| `BI14.tw.B01`, `.tw.C01`, `.tw.E02` | BI14 | B01 / C01 / E02 |
| `BSSA14.tw.B01`, `.tw.C01`, `.tw.E02` | BSSA14 | B01 / C01 / E02 |
| `CB14.tw.C01`, `.tw.E05` | CB14 | C01 / E05 |
| `CY14.tw.B01`, `.tw.C01`, `.tw.E04` | CY14 | B01 / C01 / E04 |
| `I14.tw.B01`, `.tw.C01`, `.tw.E04` | I14 | B01 / C01 / E04 |

### Subduction

| Function | Reference | Type |
|---|---|---|
| `BCHydroSub2018Global` | BC Hydro (2018) | Global interface/intraslab |
| `BCHydroSub2018SouthAM` | BC Hydro (2018) | South America |
| `BCHydroSub2018TW` | BC Hydro (2018) | Taiwan |
| `BCHydroSubV3` | BC Hydro V3 | Interface/intraslab |
| `Chao2017`, `Chao2018` | Chao et al. (2017, 2018) | Taiwan subduction |
| `Kanno06` | Kanno et al. (2006) | Interface/intraslab |
| `Mcverry2006Sub` | McVerry et al. (2006) | Interface/intraslab |
| `PhungSub17`, `PhungSub18` | Phung et al. (2017, 2018) | Subduction |
| `Zh16Inter`, `Zh16Slab`, `Zh16Sub` | Zhao et al. (2016) | Interface / intraslab / combined |

### Logic-Tree Composite Models

Pre-weighted composite models for probabilistic seismic hazard analysis (PSHA):

| Family | Functions | Count |
|---|---|---|
| Crustal composite | `Cru.Com.001` – `Cru.Com.017` | 17 |
| Subduction interface composite | `Sub.Inter.Com.001` – `Sub.Inter.Com.017` | 17 |
| Subduction intraslab composite | `Sub.Intra.Com.001` – `Sub.Intra.Com.017` | 17 |

### Utility Functions

| Function | Description |
|---|---|
| `atten2()` | Plot attenuation curves for a given magnitude and distance range |
| `get.GMPE.prd()` | Return the available spectral periods for a named GMPE model |

## Data Sets

| Object | Description |
|---|---|
| `GMPEhaz.model` | Named list mapping each GMPE to its supported spectral periods |
| `cnames` | Column name metadata for each GMPE output |
| `data` | Example ground motion records for testing |

## Documentation

Full function reference and vignettes are available at <https://personlin.github.io/GMPEhaz/>.

Two vignettes are included:

- **Introduction** (`vignettes/GMPEhaz-introduction.Rmd`) — package overview and basic usage
- **GMPE functions guide** (`vignettes/gmpe-functions-guide.Rmd`) — detailed parameter descriptions for each model

## Requirements

- R ≥ 4.1.0
- A Fortran compiler (GNU `gfortran` ≥ 7) is required to build from source

## License

GMPEhaz is distributed under the [GPL-3](https://www.gnu.org/licenses/gpl-3.0) license.
