#' GMPE function for Chiou and Youngs(2014) NGA-West2 model
#'
#' \code{CY14.tw.E04} returns the ground-motion prediction with it sigma of Chiou and Youngs(2014) GMPE
#' adjusted to Taiwan.
#'
#'Chiou, B. S.-J., and R. R. Youngs (2014), Update of the chiou and youngs NGA model for
#'the average horizontal component of peak ground motion and response spectra,
#'Earthquake Spectra, 30(3), 1117-1153.
#'\url{http://dx.doi.org/10.1193/072813EQS219M}
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Rjb Joyner and Boore distance(km), Numeric.
#' @param Prd Period of spectral acceleration.
#' @param ftype style of faulting.
#' @param Vs30 Vs30(m/s).
#' @param Dip Dip angle of the fault plane.
#' @param Ztor Depth to the top of the finite rupture model (km).
#' @param Z1.0 the depth to the shear-wave velocity horizon of 1.0(km/s).
#' @param Vs30_class 1 measured, 0 estimated.
#' @param hwflag hanging-wall flag, 1 for hanging-wall.
#' @param Rx Horizontal distance(km) from top edge of rupture. Measured perpendicular to the fault strike.
#' @param regionflag 0 Global, 1 Japan and Italy, 2 Wenchuan (note only for M7.9)
#'
#' @return A list will be return, including mag, Rrup, Rjb, specT, period, lnY, sigma, iflag, Vs30,
#' dip, Ztor, ftype, Z10, vs3-_class, hwflag, Rx, regionflag, phi, tau.
#'
#' @examples
#' CY14.tw.E04(6, 20, 20, 0, 760, 90, 0, 0, 0.5, 1, 0, 20, 0)
#' CY14.tw.E04(7, 20, 20, 0, 760, 90, 0, 0, 0.5, 1, 0, 20, 0)
#'
#' @export
CY14.tw.E04 <- function(Mag, Rrup, Rjb, Prd, Vs30, Dip, Ztor, ftype=0, Z1.0, Vs30_class=1, hwflag=0, Rx,
                    regionflag=0){
  #     fType     Mechanism                      Rake
  #------------------------------------------------------#
  #      -1       Normal                   -120 < Rake < -60.0
  #     1, 0.5    Reverse and Rev/Obl        30 < Rake < 150.0
  #     0,-0.5    Strike-Slip and NMl/Obl        Otherwise

  # Vs30_class=1 (measured), 0 (estimated)

  #     Apply Regional scaling factor.
  #     Regionflag = 0 Global
  #     Regionflag = 1 Japan and Italy
  #        Also set sigma2 equal to Japan specific value
  #     Regionflag = 2 Wenchuan (note only for M7.9)
  if (Prd != 0 & (Prd < 0.01 | Prd > 10)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("S04_CY14_TW_E04", mag=as.single(Mag), Rrup=as.single(Rrup), Rbjf=as.single(Rjb),
                      specT=as.single(Prd), period2=as.single(0), lnY=as.single(0.1), sigma=as.single(0.1),
                      iflag=as.integer(0), vs=as.single(Vs30), Delta=as.single(Dip), DTor=as.single(Ztor),
                      ftype=as.single(ftype), depthvs10=as.single(Z1.0), vs30_class=as.integer(Vs30_class),
                      hwflag=as.integer(hwflag), Rx=as.single(Rx), regionflag=as.integer(regionflag),
                      phi=as.single(0.0), tau=as.single(0.0))
  names(retvals) <- c("mag", "Rrup", "Rjb", "specT", "period", "lnY", "sigma", "iflag", "Vs30",
                      "dip", "Ztor", "ftype", "Z1.0", "vs30_class", "hwflag", "Rx", "regionflag",
                      "phi", "tau")
  return(retvals)
}
