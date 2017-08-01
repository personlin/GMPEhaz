#' GMPE function for Zhao et al.(2006)
#'
#' \code{Zh06} returns the ground-motion prediction with it sigma of Zhao et al.(2006) GMPE.
#'
#'Zhao, J. et al. (2006), Attenuation relations of strong ground motion in japan using
#'site classification based on predominant period,
#'Bulletin of the Seismological Society of America, 96(3), 898-913.
#'\url{http://dx.doi.org/10.1785/0120050122}
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Prd Period of spectral acceleration.
#' @param sclass site condition.
#' @param sourceclass source type, 0 for crustal, 1 for subduction interface, 2 for subduction intraslab.
#' @param ftype style of faulting.
#' @param depth focal depth(km).
#'
#' @return A list will be return, including mag, Rrup, ftype, lnY, sigma, sclass, specT, attenName, period, iflag, sourcetype, depth, phi, tau.
#'
#' @examples
#' Zh06(6, 20, 0, 1.0, 0, 0.0, 10)
#' Zh06(7, 10, 0, 1.0, 0, 0.0, 10)
#'
#' @export
Zh06 <- function(Mag, Rrup, ftype=0, sclass=1.0, Prd=0, sourceclass=0.0, depth=10){
  #Zhaoetal2006 ( m, dist, ftype, lnY, sigma, sclass, specT,
  #               attenName, period1, iflag, sourcetype, hypo, phi, tau )
  #     Sourcetype = 0 Crustal
  #     Sourcetype = 1 Subduction - Interface
  #     Sourcetype = 2 Subduction - Slab
  #     Sclass = 0 Hard Rock
  #     Sclass = 1 SC I
  #     Sclass = 2 SC II
  #     Sclass = 3 SC III
  #     Sclass = 4 SC IV
  #     Model Number = 256
  if (Prd != 0 & (Prd < 0.01 | Prd > 5)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("Zhaoetal2006", mag=as.single(Mag), dist=as.single(Rrup), ftype=as.single(ftype),
                      lnY=as.single(0.1), sigma=as.single(0.1), sclass=as.single(sclass),
                      specT=as.single(Prd), attenName=as.character("attenName"), period1=as.single(0), iflag=as.integer(0),
                      sourcetype=as.single(sourceclass), hypo=as.single(depth),
                      phi=as.single(0.0), tau=as.single(0.0))
  names(retvals) <- c("mag", "Rrup", "ftype", "lnY", "sigma", "sclass", "specT", "attenName", "period", "iflag",
                      "sourcetype", "depth", "phi", "tau")
  retvals$attenName <- NULL
  return(retvals)
}
