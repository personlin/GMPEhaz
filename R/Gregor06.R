#' GMPE function for Gregor et al.(2006)
#'
#' \code{Gregor06} returns the ground-motion prediction with it sigma of Gregor et al.(2006) GMPE.
#'
#'Gregor, N., W. Silva, I. Wong, and R. Youngs (2006), Updated response spectral attenuation relationship
#'for cascadia subduction zone megathrust earthquake, Seismological Research Letters, 77(2), 325-326.
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Prd Period of spectral acceleration.
#' @param Vs30 Vs30(m/s).
#'
#' @return A list will be return, including mag, Rrup, lnY, sigma, specT, attenName, Vs30, period, iflag.
#'
#' @examples
#' Gregor06(6, 20, 0, 760)
#' Gregor06(7, 10, 0, 760)
#'
#' @export
Gregor06 <- function(Mag, Rrup, Prd, Vs30) {
  #subroutine Gregor06Cas(mag, rupdist, lnY, sigma, specT, attenName, vs30, period,iflag )
  if (Prd != 0 & (Prd < 0.02 | Prd > 5)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("Gregor06Cas", mag=as.single(Mag), rupdist=as.single(Rrup), lnY=as.single(0.1),
                      sigma=as.single(0.1), specT=as.single(Prd), attenName=as.character("attenName"), vs30=as.single(Vs30),
                      period=as.single(0), iflag=as.integer(1))
  names(retvals) <- c("mag", "Rrup", "lnY", "sigma", "specT", "attenName", "Vs30", "period", "iflag")
  retvals$attenName <- NULL
  return(retvals)
}
