#' GMPE function for Abrahamson et al.(2014) NGA-West2 model
#'
#' \code{ASK14.tw.B01} returns the ground-motion prediction with it sigma of Abrahamson et al.(2014) GMPE
#' adjusted to Taiwan.
#'
#'Abrahamson, N. A., W. J. Silva, and R. Kamai (2014), Summary of the ASK14 ground motion relation
#'for active crustal regions, Earthquake Spectra, 30(3), 1025-1055.
#'\url{http://dx.doi.org/10.1193/070913EQS198M}
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Rjb Joyner and Boore distance(km), Numeric.
#' @param Prd Period of spectral acceleration.
#' @param Dip Dip angle of the fault plane.
#' @param ftype style of faulting.
#' @param rupwidth Down-dip rupture width (km).
#' @param Vs30 Vs30(m/s).
#' @param hwflag hanging-wall flag, 1 for hanging-wall.
#' @param Ztor Depth to the top of the finite rupture model (km).
#' @param Vs30_class 1 measured, 0 estimated.
#' @param Z1.0 the depth to the shear-wave velocity horizon of 1.0(km/s).
#' @param Rx Horizontal distance(km) from top edge of rupture. Measured perpendicular to the fault strike.
#' @param Ry0 Horizontal distance(km) off the end of the rupture measured parallel to strike.
#' @param regionflag Regional attenuation flag, 0 = Global, 1 = Taiwan, 2 = China, 3 = Japan.
#' @param msasflag Mainshock and Aftershocks flag, 0 = Mainshocks, 1 = Aftershocks.
#'
#' @return A list will be return, including mag, dip, ftype, rupwidth, Rrup, Rjb, Vs30, hwflag,
#' lnY, sigma, specT, period, Ztor, iflag, vs30_class, z10, Rx, Ry0, regionflag, msasflag, phi, tau.
#'
#' @examples
#' ASK14.tw.B01(6, 20, 20, 0, 90, 0, 10, 760, 0, 0, 1, 0.5, 20, 10, 0, 0)
#' ASK14.tw.B01(7, 20, 20, 0, 90, 0, 10, 760, 0, 0, 1, 0.5, 20, 10, 0, 0)
#'
#' @export
ASK14.tw.B01 <- function(Mag, Rrup, Rjb, Prd, Dip, ftype=0, rupwidth, Vs30=760, hwflag=0,
                     Ztor, Vs30_class=1, Z1.0, Rx, Ry0, regionflag=0, msasflag=0){
  #Vs30_class = 0 for estimated, 1 for measured
  #     For now, convert ftype to an equivalent rake
  #     fType     Mechanism                      Rake
  #     ------------------------------------------------------
  #      -1       Normal                   -120 < Rake < -60.0
  #     1, 0.5    Reverse and Rev/Obl        30 < Rake < 150.0
  #     0,-0.5    Strike-Slip and NMl/Obl        Otherwise
  # subroutine ASK_NGAWest2_2013 ( mag, dip, fType, fltWidth, rRup, Rjb,
  #                               vs30, hwflag, lnY, sigma, specT, period2, ztor,
  #                               iflag, vs30_class, z10, Rx, Ry0, regionflag, msasflag,
  #                               phi, tau )
  if (Prd != 0 & (Prd < 0.01 | Prd > 10)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("ASK14_TW_B01", mag=as.single(Mag), dip=as.single(Dip), ftype=as.single(ftype),
                      fltWidth=as.single(rupwidth), Rrup=as.single(Rrup), Rjb=as.single(Rjb),
                      vs30=as.single(Vs30),hwflag=as.integer(hwflag), lnY=as.single(0.1), sigma=as.single(0.1),
                      specT=as.single(Prd),period2=as.single(0), ztor=as.single(Ztor), iflag=as.integer(0),
                      vs30_class=as.integer(Vs30_class),z10=as.single(Z1.0), Rx=as.single(Rx), Ry0=as.single(Ry0),
                      regionflag=as.integer(regionflag),msasflag=as.integer(msasflag),
                      phi=as.single(0.0), tau=as.single(0.0))
  names(retvals) <- c("mag", "dip", "ftype", "rupwidth", "Rrup", "Rjb", "Vs30", "hwflag", "lnY", "sigma",
                      "specT", "period", "Ztor", "iflag", "vs30_class", "Z1.0", "Rx", "Ry0", "regionflag",
                      "msasflag","phi", "tau")
  return(retvals)
}
