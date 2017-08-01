#' GMPE function for Campbell and Bozorgnia(2014) NGA-West2 model
#'
#' \code{CB14} returns the ground-motion prediction with it sigma of Campbell and Bozorgnia(2014) GMPE.
#'
#'Campbell, K. W., and Y. Bozorgnia (2014), NGA-west2 ground motion model for the average
#'horizontal components of PGA, PGV, and 5% damped linear acceleration response spectra,
#'Earthquake Spectra, 30(3), 1087-1115.
#'\url{http://dx.doi.org/10.1193/062913EQS175M}
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Rjb Joyner-Boore distance(km), Numeric.
#' @param ftype style of faulting.
#' @param Prd Period of spectral acceleration.
#' @param Vs30 Vs30(m/s).
#' @param Ztor Depth to the top of the finite rupture model (km).
#' @param Z2.5 the depth to the shear-wave velocity horizon of 2.5(km/s).
#' @param Dip Dip angle of the fault plane.
#' @param depth hypocentral depth(km).
#' @param hwflag hanging-wall flag, 1 for hanging-wall.
#' @param Rx Horizontal distance(km) from top edge of rupture. Measured perpendicular to the fault strike.
#' @param rupwidth Down-dip rupture width (km).
#' @param regionflag Regional attenuation flag, 0 = California, 1 = Japan 2 = China, 3 = Italy.
#'
#' @return A list will be return, including mag, Rrup, Rjb, ftype, specT, period, lnY, sigma,
#' iflag, Vs30, Ztor, Z25, dip, depth, hwflag, Rx, rupwidth, regionflag, phi, tau.
#'
#' @examples
#' CB14(6, 20, 20, 0, 0, 760, 0, 1.5, 90, 10, 0, 20, 10, 0)
#' CB14(7, 20, 20, 0, 0, 760, 0, 1.5, 90, 10, 0, 20, 10, 0)
#'
#' @export
CB14 <- function(Mag, Rrup, Rjb, ftype=0, Prd, Vs30=760,
                    Ztor, Z2.5, Dip, depth, hwflag=0, Rx, rupwidth, regionflag=0){
  #subroutine CB_NGAWest2_2013 ( mag, Rrup, Rbjf, Ftype, specT, period2, lnY, sigma, iflag, vs,
  #  depthtop, D25, Dip, depth, HWflag, Rx, rupwidth, regionflag, phi, tau )

  # Model Number = 2836
  if (Prd != 0 & Prd != -1 & (Prd < 0.01 | Prd > 10)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("CB_NGAWest2_2013", mag=as.single(Mag), Rrup=as.single(Rrup), Rbjf=as.single(Rjb),
                      ftype=as.single(ftype), specT=as.single(Prd), period2=as.single(0),
                      lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(0),
                      vs=as.single(Vs30), depthtop=as.single(Ztor), D25=as.single(Z2.5), Dip=as.single(Dip),
                      depth=as.single(depth), hwflag=as.integer(hwflag), Rx=as.single(Rx),
                      rupwidth=as.single(rupwidth), regionflag=as.integer(regionflag),
                      phi=as.single(0.0), tau=as.single(0.0))
  names(retvals) <- c("mag", "Rrup", "Rjb", "ftype", "specT", "period", "lnY", "sigma", "iflag", "Vs30",
                      "Ztor", "Z2.5", "dip", "depth", "hwflag", "Rx", "rupwidth", "regionflag", "phi", "tau")
  return(retvals)
}
