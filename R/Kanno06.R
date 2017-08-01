#' GMPE function for Kanno et al.(2006)
#'
#' \code{Kanno06} returns the ground-motion prediction with it sigma of Kanno et al.(2006) GMPE.
#'
#'Kanno, T., A. Narita, N. Morikawa, H. Fujiwara, and Y. Fukushima (2006), A new attenuation relation
#'for strong ground motion in japan based on recorded data, Bulletin of the Seismological Society of America,
#' 96(3), 879-897.
#'\url{http://dx.doi.org/10.1785/0120050138}
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Prd Period of spectral acceleration.
#' @param Vs30 Vs30(m/s).
#' @param depth hypocentral depth(km).
#'
#' @return A list will be return, including mag, Rrup, specT, period, lnY, sigma, iflag, Vs30, depth.
#'
#' @examples
#' Kanno06(6, 20, 0, 760, 10)
#' Kanno06(7, 10, 0, 760, 10)
#'
#' @export
Kanno06 <- function(Mag, Rrup, Prd, Vs30, depth) {
  #subroutine kanno2006 (mag, Rrup, specT, period2, lnY, sigma, iflag, vs30, depth )
  if (Prd != 0 & (Prd < 0.05 | Prd > 5)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("kanno2006", mag=as.single(Mag), Rrup=as.single(Rrup), specT=as.single(Prd),
                      period2=as.single(0), lnY=as.single(0.1), sigma=as.single(0.1),
                      iflag=as.integer(1), vs30=as.single(Vs30), depth=as.single(depth))
  names(retvals) <- c("mag", "Rrup", "specT", "period", "lnY", "sigma", "iflag", "Vs30", "depth")
  return(retvals)
}
