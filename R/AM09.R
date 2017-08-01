#' GMPE function for Atkinson and Macias (2009)
#'
#' \code{AM09} returns the ground-motion prediction with it sigma of Atkinson and Macias (2009) GMPE.
#' (Cascadia Subduction interface, Horizontal NEHRP B/C)
#'
#'Atkinson, G. M., and Macias, M. (2009). Predicted Ground Motions for Great Interface Earthquakes
#'in the Cascadia Subduction Zone. Bulletin of the Seismological Society of America, 99(3), 1552-1578.
#'\url{http://doi.org/10.1785/0120080147}
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Prd Period of spectral acceleration.
#'
#' @return A list will be return, including mag, Rrup, lnY, sigma, specT, period, iflag.
#'
#' @examples
#' AM09(6, 20, 0)
#' AM09(7, 10, 0)
#'
#' @export
AM09 <- function(Mag, Rrup, Prd) {
  # subroutine AM09_Cas ( mag, rupDist, lnY, sigma,
  #                       specT, period, iflag )
  if (Prd != 0 & (Prd < 0.01 | Prd > 10)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("AM09_Cas", mag=as.single(Mag), rupDist=as.single(Rrup), lnY=as.single(0.1),
                      sigma=as.single(0.1), specT=as.single(Prd),
                      period=as.single(0), iflag=as.integer(1))
  names(retvals) <- c("mag", "Rrup", "lnY", "sigma", "specT", "period", "iflag")
  return(retvals)
}


