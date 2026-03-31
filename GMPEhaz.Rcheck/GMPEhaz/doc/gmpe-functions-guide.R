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
# BSSA14 function parameter description
# Mag: Earthquake magnitude
# Rjb: Joyner-Boore distance (km)
# Prd: Period (0 for PGA, -1 for PGV)
# Vs30: 30-meter average shear wave velocity (m/s)
# ftype: Fault type (0=strike-slip, 1=reverse, -1=normal)
# Z1.0: Depth to 1.0 km/s shear wave velocity (km)
# regionflag: Region flag (0=global, 1=China-Turkey, 2=Italy-Japan)
# basinflag: Basin adjustment

# Example: Magnitude 6 earthquake, 20km distance, rock site
result_bssa14 <- BSSA14(Mag=6, Rjb=20, Prd=0, Vs30=760, ftype=0, Z1.0=0.5, regionflag=0, basinflag=0)
cat("BSSA14 PGA prediction results:\n")
cat("Log value:", round(result_bssa14$lnY, 3), "\n")
cat("PGA(g):", round(exp(result_bssa14$lnY)/exp(6.89), 3), "\n")
cat("Standard deviation:", round(result_bssa14$sigma, 3), "\n")

## -----------------------------------------------------------------------------
# CY14 requires more parameters
# Rrup: Rupture distance (km)
# Dip: Fault dip angle (degrees)
# Ztor: Depth to top of rupture (km)
# Vs30_class: Vs30 classification (1=measured, 0=estimated)
# hwflag: Hanging wall flag (1=hanging wall, 0=not hanging wall)
# Rx: Horizontal distance perpendicular to fault strike (km)

result_cy14 <- CY14(Mag=6, Rrup=20, Rjb=20, Prd=0, ftype=0, Vs30=760, 
                    Dip=90, Ztor=0, Z1.0=0.5, Vs30_class=1, hwflag=0, Rx=20, regionflag=0)
cat("CY14 PGA prediction results:\n")
cat("Log value:", round(result_cy14$lnY, 3), "\n")
cat("PGA(g):", round(exp(result_cy14$lnY)/exp(6.89), 3), "\n")

## -----------------------------------------------------------------------------
# ASB14 is relatively simple, main parameters include magnitude, distance, fault type and site conditions
result_asb14 <- ASB14(Mag=6, Rjb=20, Prd=0, ftype=0, Vs30=760)
cat("ASB14 PGA prediction results:\n")
cat("Log value:", round(result_asb14$lnY, 3), "\n")
cat("PGA(g):", round(exp(result_asb14$lnY)/exp(6.89), 3), "\n")

## -----------------------------------------------------------------------------
result_bi14 <- BI14(Mag=6, Rjb=20, Prd=0, ftype=0, Vs30=760)
cat("BI14 PGA prediction results:\n")
cat("Log value:", round(result_bi14$lnY, 3), "\n")
cat("PGA(g):", round(exp(result_bi14$lnY)/exp(6.89), 3), "\n")

## -----------------------------------------------------------------------------
# Lin11 model distinguishes between rock and soil sites (based on Vs30)
# hwflag: Hanging wall effect flag

result_lin11 <- Lin11(Mag=6, Rrup=20, Prd=0, hwflag=0, Vs30=760)
cat("Lin11 PGA prediction results:\n")
cat("Log value:", round(result_lin11$lnY, 3), "\n")
cat("PGA(g):", round(exp(result_lin11$lnY/exp(6.89)), 3), "\n")

## -----------------------------------------------------------------------------
# LL08 is applicable to subduction zone earthquakes
# ftype: 0=interface earthquake, 1=intraslab earthquake
# depth: Source depth (km)

result_ll08 <- LL08(Mag=6, Rrup=20, depth=10, ftype=0, Prd=0, Vs30=760)
cat("LL08 PGA prediction results:\n")
cat("Log value:", round(result_ll08$lnY, 3), "\n")
cat("PGA(g):", round(exp(result_ll08$lnY)/exp(6.89), 3), "\n")

## -----------------------------------------------------------------------------
# BCHydroSub2018Global considers forearc/backarc effects
# forearc: 0=forearc, 1=backarc
# Rhypo: Hypocentral distance (km)

result_bch <- BCHydroSub2018Global(Mag=6, Rrup=20, Prd=0, ftype=0, Vs30=760, 
                                   forearc=1, depth=10, Rhypo=25)
cat("BCHydroSub2018Global PGA prediction results:\n")
cat("Log value:", round(result_bch$lnY, 3), "\n")
cat("PGA(g):", round(exp(result_bch$lnY), 3)/exp(6.89), "\n")

## -----------------------------------------------------------------------------
# Calculate response spectrum for different periods
periods <- c(0, 0.1, 0.2, 0.5, 1.0, 2.0)  # Including PGA(0) and different periods
mag <- 7
distance <- 30
vs30 <- 760

# Calculate response spectrum using BSSA14
results <- data.frame(
  Period = periods,
  BSSA14_lnSa = numeric(length(periods)),
  CY14_lnSa = numeric(length(periods))
)

for(i in 1:length(periods)) {
  # BSSA14
  bssa_result <- BSSA14(Mag=mag, Rjb=distance, Prd=periods[i], Vs30=vs30, 
                        ftype=0, Z1.0=0.5, regionflag=0, basinflag=0)
  results$BSSA14_lnSa[i] <- bssa_result$lnY
  
  # CY14
  cy_result <- CY14(Mag=mag, Rrup=distance, Rjb=distance, Prd=periods[i], 
                    ftype=0, Vs30=vs30, Dip=90, Ztor=0, Z1.0=0.5, 
                    Vs30_class=1, hwflag=0, Rx=distance, regionflag=0)
  results$CY14_lnSa[i] <- cy_result$lnY
}

print(results)

## ----fig.width=7, fig.height=5------------------------------------------------
# Plot response spectrum comparison chart
plot(results$Period, exp(results$BSSA14_lnSa)/exp(6.89), type="l", col="blue", lwd=2,
     xlab="Period (sec)", ylab="Spectral Acceleration (g)", 
     main="Response Spectrum Comparison (M7.0, R=30km)",
     ylim=c(0, max(exp(results$BSSA14_lnSa)/exp(6.89), exp(results$CY14_lnSa)/exp(6.89))))
lines(results$Period, exp(results$CY14_lnSa)/exp(6.89), col="red", lwd=2)
legend("topright", legend=c("BSSA14", "CY14"), col=c("blue", "red"), lwd=2)
grid()

## -----------------------------------------------------------------------------
# Chao2018 function parameter description
# Mag: Earthquake magnitude
# Rrup: Rupture distance (km)
# Prd: Period (0 for PGA)
# ftype: Fault mechanism (-1=normal, 0=strike-slip, 1=reverse for crustal; 0=interface, 1=intraslab for subduction)
# Vs30: 30-meter average shear wave velocity (m/s)
# Vs30_class: Vs30 data quality (0=estimated, 1=measured)
# Ztor: Depth to top of rupture (km)
# Z1.0: Depth to 1.0 km/s shear wave velocity (km)
# sourcetype: Source type (0=crustal, 1=subduction)
# msasflag: Mainshock/aftershock flag (0=mainshock, 1=aftershock)

# Example: Crustal earthquake
result_chao2018_crustal <- Chao2018(Mag=6.5, Rrup=30, Prd=0, ftype=0, Vs30=760, 
                                   Vs30_class=1, Ztor=5, Z1.0=0.5, sourcetype=0, msasflag=0)
cat("Chao2018 Crustal PGA prediction:\n")
cat("Log value:", round(result_chao2018_crustal$lnY, 3), "\n")
cat("PGA(g):", round(exp(result_chao2018_crustal$lnY)/exp(6.89), 3), "\n")

# Example: Subduction interface earthquake
result_chao2018_sub <- Chao2018(Mag=7.5, Rrup=50, Prd=0, ftype=0, Vs30=760, 
                               Vs30_class=1, Ztor=0, Z1.0=0.5, sourcetype=1, msasflag=0)
cat("Chao2018 Subduction Interface PGA prediction:\n")
cat("Log value:", round(result_chao2018_sub$lnY, 3), "\n")
cat("PGA(g):", round(exp(result_chao2018_sub$lnY)/exp(6.89), 3), "\n")

## -----------------------------------------------------------------------------
# PhungCru18 function parameter description
# Mag: Earthquake magnitude
# Rrup: Rupture distance (km)
# Rjb: Joyner-Boore distance (km)
# Rx: Horizontal distance from top edge of rupture (km)
# Prd: Period (0 for PGA)
# Vs30: 30-meter average shear wave velocity (m/s)
# Dip: Dip angle of the fault plane (degrees)
# Ztor: Depth to top of rupture (km)
# ftype: Fault mechanism (-1=normal, 0=strike-slip, 1=reverse)
# Z1.0: Depth to 1.0 km/s shear wave velocity (km)
# Vs30_class: Vs30 data quality (0=estimated, 1=measured)
# regionflag: Regional flag (0=global, 1=Taiwan)
# hwflag: Hanging wall flag (0=footwall, 1=hanging wall)

# Example: Taiwan crustal earthquake
result_phungcru18 <- PhungCru18(Mag=6.5, Rrup=25, Rjb=20, Rx=15, Prd=0, Vs30=760, 
                               Dip=90, Ztor=5, ftype=0, Z1.0=0.5, Vs30_class=1, 
                               regionflag=1, hwflag=0)
cat("PhungCru18 Taiwan PGA prediction:\n")
cat("Log value:", round(result_phungcru18$lnY, 3), "\n")
cat("PGA(g):", round(exp(result_phungcru18$lnY)/exp(6.89), 3), "\n")

## -----------------------------------------------------------------------------
# PhungSub18 function parameter description
# Mag: Earthquake magnitude
# Rrup: Rupture distance (km)
# Prd: Period (0 for PGA)
# ftype: Fault type (0=interface, 1=intraslab)
# Vs30: 30-meter average shear wave velocity (m/s)
# Z1.0: Depth to 1.0 km/s shear wave velocity (km)
# Ztor: Depth to top of rupture (km)
# regionflag: Regional flag (0=Japan, 1=Taiwan)

# Example: Taiwan interface earthquake
result_phungsub18_int <- PhungSub18(Mag=7.5, Rrup=50, Prd=0, ftype=0, Vs30=760, 
                                   Z1.0=0.5, Ztor=0, regionflag=1)
cat("PhungSub18 Taiwan Interface PGA prediction:\n")
cat("Log value:", round(result_phungsub18_int$lnY, 3), "\n")
cat("PGA(g):", round(exp(result_phungsub18_int$lnY)/exp(6.89), 3), "\n")

# Example: Taiwan intraslab earthquake
result_phungsub18_slab <- PhungSub18(Mag=7.0, Rrup=60, Prd=0, ftype=1, Vs30=760, 
                                    Z1.0=0.5, Ztor=30, regionflag=1)
cat("PhungSub18 Taiwan Intraslab PGA prediction:\n")
cat("Log value:", round(result_phungsub18_slab$lnY, 3), "\n")
cat("PGA(g):", round(exp(result_phungsub18_slab$lnY)/exp(6.89), 3), "\n")

