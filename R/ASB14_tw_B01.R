#' GMPE function for Akkar et al.(2013)
#'
#' \code{ASB14.tw.B01} returns the ground-motion prediction with it sigma of Akkar et al.(2014) GMPE
#' adjusted to Taiwan.
#'
#'Akkar, S., M. a. Sandikkaya, and J. J. Bommer(2014), Empirical ground-motion models for
#'point- and extended-source crustal earthquake scenarios in europe and the middle east,
#'Bulletin of Earthquake Engineering, Volume 12, Issue 1, pp 359-387.
#'\url{http://dx.doi.org/10.1007/s10518-013-9461-4}
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rjb Joyner and Boore distance(km), Numeric.
#' @param Prd Period of spectral acceleration.
#' @param ftype style of faulting.
#' @param Vs30 Vs30(m/s).
#'
#' @return A list will be return, including mag, Rjb, specT, period, lnY, sigma, iflag, ftype, Vs30, phi, tau.
#'
#' @examples
#' ASB14.tw.B01(6, 20, 0, 0, 760)
#' ASB14.tw.B01(7, 10, 0, 0, 760)
#'
#' @export
ASB14.tw.B01 <- function(Mag, Rjb, Prd=0, ftype=0, Vs30=760){
  #ASB_2013 ( mag, Rbjf, specT,
  #                     period2, lnY, sigma, iflag, ftype, Vs, phiT, tauT )
  if (Prd != 0 & (Prd < 0.01 | Prd > 4)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("ASB14_TW_B01", mag=as.single(Mag), Rbjf=as.single(Rjb), specT=as.single(Prd),
                      period2=as.single(0), lnY=as.single(0.1), sigma=as.single(0.1),
                      iflag=as.integer(0), ftype=as.single(ftype), vs=as.single(Vs30),
                      phi=as.single(0.0), tau=as.single(0.0))
  names(retvals) <- c("mag", "Rjb", "specT", "period", "lnY", "sigma", "iflag", "ftype", "Vs30",
                      "phi", "tau")
  return(retvals)
}
