#' GMPE function for Graizer and Kalkan(2016)
#'
#' \code{GK16} returns the ground-motion prediction with it sigma of Graizer and Kalkan(2016) GMPE.
#'
#'Graizer and E. Kalkan.(2016) Summary of the GK15 ground-motion prediction equation for
#'horizontal PGA and 5% damped PSA from shallow crustal continental earthquakes.
#'Bulletin of the Seismological Society of America, 106(2), 687-707.
#'\url{http://dx.doi.org/10.1785/0120150194}
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Prd Period of spectral acceleration.
#' @param ftype style of faulting.
#' @param Vs30 Vs30(m/s).
#' @param q0 Quality factor (Q0).
#' @param Z1.5 Depth to the 1.5km/s shear-wave velocity isosurfac.
#'
#' @return A list will be return, including mag, Rrup, specT, ftype, period, lnY, sigma, iflag, Vs30, q0, Z15.
#'
#' @examples
#' GK16(6, 20, 0, 0, 760, 150, 1)
#' GK16(7, 10, 0, 0, 760, 150, 1)
#'
#' @export
GK16 <- function(Mag, Rrup, ftype=0, Prd, Vs30=760, q0=150, Z1.5){
  #      Subroutine GK_Nov2012 ( m, Rrup, specT, ftype,
  #                     period2, lnY, sigma, iflag, vs, q0, depthvs15 )

  # Model Number = 90
  if (Prd != 0 & (Prd < 0.01 | Prd > 10)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("GK_Nov2012", mag=as.single(Mag), Rrup=as.single(Rrup),  specT=as.single(Prd),
                      ftype=as.single(ftype), period2=as.single(0), lnY=as.single(0.1), sigma=as.single(0.1),
                      iflag=as.integer(0), vs=as.single(Vs30), q0=as.single(q0), depthvs15=as.single(Z1.5))
  names(retvals) <- c("mag", "Rrup", "specT", "ftype", "period", "lnY", "sigma", "iflag", "Vs30",
                      "q0", "Z1.5")
  return(retvals)
}
