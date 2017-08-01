#' GMPE function for  Arroyo et al. (2010)
#'
#' \code{Arroyo2010} returns the ground-motion prediction with it sigma of Arroyo et al. (2010) GMPE.
#'
#'Arroyo, D. Garcia, M. Ordaz, M. A. Mora, and S. K. Singh. Strong ground-motion relations for Mexican
#'interplate earthquakes. Journal of Seismology, 14(4):769-785, Oct 2010.
#'\url{https://link.springer.com/article/10.1007/s10950-010-9200-0}
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Prd Period of spectral acceleration.
#'
#' @return A list will be return, including mag, Rrup, lnY, specT, iflag, period, phi1, tau1, sigma.
#'
#' @examples
#' Arroyo2010(6, 20, 0)
#' Arroyo2010(7, 10, 0)
#'
#' @export
Arroyo2010 <- function(Mag, Rrup, Prd) {
  #subroutine Arroyo2010 ( mag, rRup, lnSa, specT, iflag, period1, phi1, tau1, sigma)
  if (Prd != 0 & (Prd < 0.04 | Prd > 5)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("Arroyo2010", mag=as.single(Mag), rRup=as.single(Rrup), lnSa=as.single(0.1),
                      specT=as.single(Prd), iflag=as.integer(1), period1=as.single(0),
                      phi1=as.single(0.1), tau1=as.single(0.1), sigma=as.single(0.1))
  names(retvals) <- c("mag", "Rrup", "lnY", "specT", "iflag", "period", "phi", "tau","sigma")
  return(retvals)
}
