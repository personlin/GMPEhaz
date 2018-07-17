#' GMPE function for Phung et al.(2018)
#'
#' \code{PhungCru18} returns the ground-motion prediction with it sigma of Phung et al.(2018) GMPE.
#'
#'Phung et al.(2018) Crustal GMPE
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
#' @param regionflag 0 Global, 1 Taiwan
#' @param Rx Horizontal distance(km) from top edge of rupture. Measured perpendicular to the fault strike.
#' @param hwflag hanging-wall flag, 1 for hanging-wall.
#'
#' @return A list will be return, including m, Rrup, Rjb, Rx, specT, period2, lnY, sigma, iflag,
#'    vs, Delta, DTor, Ftype, depthvs10, vs30_class, regionflag, phi, tau, HWflag, Rx.
#'
#' @examples
#' PhungCru18(6, 20, 20, 20, 0, 760, 90, 0, 0, 0.5, 1, 1, 0)
#' PhungCru18(7, 20, 20, 20, 0, 760, 90, 0, 0, 0.5, 1, 1, 0)
#'
#' @export
PhungCru18 <- function(Mag, Rrup, Rjb, Rx, Prd, Vs30, Dip, Ztor, ftype=0, Z1.0, Vs30_class=1,
                       regionflag=1, hwflag=0){
  #     fType     Mechanism                      Rake
  #------------------------------------------------------#
  #      -1       Normal                   -120 < Rake < -60.0
  #     1, 0.5    Reverse and Rev/Obl        30 < Rake < 150.0
  #     0,-0.5    Strike-Slip and NMl/Obl        Otherwise

  # Vs30_class=1 (measured), 0 (estimated)

  #     Apply Regional scaling factor.
  #     Regionflag = 0 Global
  #     Regionflag = 1 Taiwan
  # Subroutine S04_PhungCrust2018 ( m, Rrup, Rbjf, specT, period2, lnY, sigma, iflag,
  #                                 vs, Delta, DTor, Ftype, depthvs10, vs30_class,
  #                                 regionflag, phi, tau, HWflag, Rx )
  if (Prd != 0 & (Prd < 0.01 | Prd > 5)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("S04_PhungCrust2018", m=as.single(Mag), Rrup=as.single(Rrup), Rbjf=as.single(Rjb), specT=as.single(Prd),
                      period2=as.single(0), lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(0),
                      vs=as.single(Vs30), Delta=as.single(Dip), DTor=as.single(Ztor), Ftype=as.single(ftype),
                      depthvs10=as.single(Z1.0), vs30_class=as.integer(Vs30_class),
                      regionflag=as.integer(regionflag), phi=as.single(0.0), tau=as.single(0.0), HWflag=as.integer(hwflag),
                      Rx = as.single(Rx))

  names(retvals) <- c("mag", "Rrup", "Rjb","specT", "period", "lnY", "sigma", "iflag", "Vs30",
                      "dip", "Ztor", "ftype", "Z1.0", "vs30_class", "regionflag", "phi", "tau", "hwflag", "Rx")

  return(retvals)
}
