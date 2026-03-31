## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE
)

## ----setup--------------------------------------------------------------------
library(GMPEhaz)

## -----------------------------------------------------------------------------
# Calculate PGA using BSSA14 model
result <- BSSA14(Mag=6.5, Rjb=20, Prd=0, Vs30=760, ftype=0, Z1.0=0.5, regionflag=0, basinflag=0)

# Display results
cat("Magnitude:", result$mag, "\n")
cat("Distance:", result$Rjb, "km\n")
cat("Log PGA:", round(result$lnY, 3), "\n")
cat("PGA(g):", round(exp(result$lnY)/ exp(6.89), 3), "\n")
cat("Standard deviation:", round(result$sigma, 3), "\n")

## -----------------------------------------------------------------------------
# View standard periods for NGA-West models
head(periods.T, 10)

## -----------------------------------------------------------------------------
# View available GMPE models and their period ranges
head(GMPEhaz.model)

## -----------------------------------------------------------------------------
# Query available period range for BSSA14 model
get.GMPE.prd("BSSA14")

## -----------------------------------------------------------------------------
# NGA-West2 series model comparison
models <- c("ASK14", "BSSA14", "CB14", "CY14")
mag <- 6.5
rjb <- 30
vs30 <- 760

for(model in models) {
  if(model == "ASK14") {
    result <- ASK14(mag, rjb, rjb, 0, 90, 0, 10, vs30, 0, 0, 1, 0.5, rjb, 10, 0, 0)
  } else if(model == "BSSA14") {
    result <- BSSA14(mag, rjb, 0, vs30, 0, 0.5, 0, 0)
  } else if(model == "CB14") {
    result <- CB14(mag, rjb, rjb, 0, 0, vs30, 90, 0, 0.5, 1, 0, rjb, 0, 0)
  } else if(model == "CY14") {
    result <- CY14(mag, rjb, rjb, 0, vs30, 90, 0, 0, 0.5, 1, 0, rjb, 0)
  }
  cat(model, "PGA:", round(exp(result$lnY)/exp(6.89), 3), "g\n")
}

## -----------------------------------------------------------------------------
# Subduction zone model comparison
# LL08 - Taiwan-specific model
ll08_result <- LL08(Mag=7, Rrup=50, depth=30, ftype=0, Prd=0, Vs30=760)
cat("LL08 (Interface) PGA:", round(exp(ll08_result$lnY)/exp(6.89), 3), "g\n")

# BCHydroSub2018Global - Global model
bch_result <- BCHydroSub2018Global(Mag=7, Rrup=50, Prd=0, ftype=0, Vs30=760, 
                                   forearc=1, depth=30, Rhypo=60)
cat("BCHydroSub2018Global PGA:", round(exp(bch_result$lnY)/exp(6.89), 3), "g\n")

## -----------------------------------------------------------------------------
# Calculate complete response spectrum
periods <- c(0, 0.1, 0.2, 0.5, 1.0, 2.0, 3.0)
sa_values <- numeric(length(periods))

for(i in seq_along(periods)) {
  result <- BSSA14(Mag=7, Rjb=20, Prd=periods[i], Vs30=760, ftype=0, Z1.0=0.5, regionflag=0, basinflag=0)
  sa_values[i] <- exp(result$lnY)/exp(6.89)
}

# Create response spectrum data frame
response_spectrum <- data.frame(Period = periods, Sa = sa_values)
print(response_spectrum)

## -----------------------------------------------------------------------------
# Calculate ground motion values at different distances
distances <- c(10, 20, 50, 100, 200)
pga_values <- numeric(length(distances))

for(i in seq_along(distances)) {
  result <- BSSA14(Mag=6.5, Rjb=distances[i], Prd=0, Vs30=760, ftype=0, Z1.0=0.5, regionflag=0, basinflag=0)
  pga_values[i] <- exp(result$lnY)/exp(6.89)
}

attenuation <- data.frame(Distance = distances, PGA = pga_values)
print(attenuation)

