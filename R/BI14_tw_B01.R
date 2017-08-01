#' GMPE function for Bindi et al(2014)
#'
#' \code{BI14.tw.B01} returns the ground-motion prediction with it sigma of Bindi et al.(2014) GMPE
#' adjusted to Taiwan.
#'
#'Bindi, D., M. Massa, L. Luzi, G. Ameri, F. Pacor, R. Puglia, and P. Augliera (2014),
#'Pan-european ground-motion prediction equations for the average horizontal component
#'of pGA, pGV, and 5 %-damped pSA at spectral periods up to 3.0 s using the RESORCE
#'dataset, Bulletin of Earthquake Engineering, 12(1), 391-430.
#'\url{http://dx.doi.org/10.1007/s10518-013-9525-5}
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rjb Joyner and Boore distance(km), Numeric.
#' @param Prd Period of spectral acceleration.
#' @param ftype style of faulting.
#' @param Vs30 Vs30(m/s).
#'
#' @return A list will be return, including mag, Rjb, ftype, specT, period, lnY, sigma, iflag, Vs30, phi, tau.
#'
#' @examples
#' BI14.tw.B01(6, 20, 0, 0, 760)
#' BI14.tw.B01(7, 10, 0, 0, 760)
#'
#' @export
BI14.tw.B01 <- function(Mag, Rjb, Prd=0, ftype=0, Vs30=760){
  #Bindi_Hor_2013 ( m, jbDist, ftype, specT,
  #                     period2, lnY, sigma, iflag, vs, phiT, tauT )
  if (Prd != 0 & (Prd < 0.01 | Prd > 3)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("Bindi14_TW_B01", mag=as.single(Mag), jbDist=as.single(Rjb), ftype=as.single(ftype),
                      specT=as.single(Prd), period2=as.single(0), lnY=as.single(0.1), sigma=as.single(0.1),
                      iflag=as.integer(0), vs=as.single(Vs30), phi=as.single(0.0), tau=as.single(0.0))
  names(retvals) <- c("mag", "Rjb", "ftype", "specT", "period", "lnY", "sigma", "iflag", "Vs30",
                      "phi", "tau")
  return(retvals)
}
