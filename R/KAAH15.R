#' GMPE function for Kale et al.(2015)
#'
#' \code{KAAH15} returns the ground-motion prediction with it sigma of Kale et al.(2015) GMPE.
#'
#'Kale, O., S. Akkar, A. Ansari, and H. Hamzehloo (2015), A ground-Motion predictive model for
#'Iran and Turkey for horizontal PGA, PGV, and 5% Damped Response Spectrum: Investigation of
#'Possible Regional Effects, Bulletin of the Seismological Society of America, 105(2A), 963-980.
#'\url{http://dx.doi.org/10.1785/0120140134}
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rjb Joyner and Boore distance(km), Numeric.
#' @param Prd Period of spectral acceleration.
#' @param ftype style of faulting.
#' @param Vs30 Vs30(m/s).
#' @param regionflag 0 for Turkey, 1 for Iran.
#'
#' @return A list will be return, including mag, Rjb, specT, period, lnY, sigma, iflag, ftype, Vs30, pgaref, region.
#'
#' @examples
#' KAAH15(6, 20, 0, 0, 760, 0)
#' KAAH15(7, 10, 0, 0, 760, 0)
#'
#' @export
KAAH15 <- function(Mag, Rjb, Prd=0, ftype=0, Vs30=760, regionflag=0){
  #subroutine KAAH_2015 ( mag, Rbjf, specT,
  #                     period2, lnY, sigma, iflag, vs, ftype, pgaref, region)
  if (Prd != 0 & Prd != -1 & (Prd < 0.01 | Prd > 4)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("KAAH_2015", mag=as.single(Mag), Rbjf=as.single(Rjb), specT=as.single(Prd),
                      period2=as.single(0), lnY=as.single(0.1), sigma=as.single(0.1),
                      iflag=as.integer(0), vs=as.single(Vs30), ftype=as.single(ftype),
                      pgaref=as.single(0.0), region=as.integer(regionflag))
  names(retvals) <- c("mag", "Rjb", "specT", "period", "lnY", "sigma", "iflag", "ftype", "Vs30",
                      "pgaref", "regionflag")
  return(retvals)
}
