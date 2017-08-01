#' GMPE function for Garcia et al.(2005)
#'
#' \code{Garcia05} returns the ground-motion prediction with it sigma of Garcia et al.(2005) GMPE.
#'
#'Garcia, D., Singh, S. K., Herraiz, M., Ordaz, M., &Pacheco, J. F. (2005). Inslab Earthquakes of Central
#'Mexico: Peak Ground-Motion Parameters and Response Spectra. Bulletin of the Seismological Society of America,
#'95(6), 2272 LP-2282. Retrieved from \url{http://www.bssaonline.org/content/95/6/2272.abstract}
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Prd Period of spectral acceleration.
#' @param depth hypocentral depth(km).
#'
#' @return A list will be return, including mag, Rrup, specT, period, lnY, sigma, iflag, depth.
#'
#' @examples
#' Garcia05(6, 20, 0, 35)
#' Garcia05(7, 10, 0, 60)
#'
#' @export
Garcia05 <- function(Mag, Rrup, Prd, depth) {
  #subroutine GarciaH05 ( mag, rupdist, specT, period, lnY, sigma, iflag, depth)
  if (Prd != 0 & (Prd < 0.04 | Prd > 5)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("GarciaH05", mag=as.single(Mag), rupdist=as.single(Rrup), specT=as.single(Prd),
                      period=as.single(0), lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(1),
                      depth=as.single(depth))
  names(retvals) <- c("mag", "Rrup", "specT", "period", "lnY", "sigma", "iflag", "depth")
  return(retvals)
}
