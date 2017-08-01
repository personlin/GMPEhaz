#' GMPE function for Zhao et al.(2016)
#'
#' \code{Zh16Cru} returns the ground-motion prediction with it sigma of Zhao et al.(2016) GMPE.
#'
#'Zhao, J. et al. (2016), Ground-Motion Prediction Equations for Shallow Crustal and Upper-Mantle
#'Earthquakes in Japan Using Site Class and Simple Geometric Attenuation Functions,
#'Bulletin of the Seismological Society of America, 106(4), 1552-1569.
#'\url{http://dx.doi.org/10.1785/0120150063}
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Prd Period of spectral acceleration.
#' @param sclass site condition, 0 for Hard Rock, 1 for SC I, 2 for SC II, 3 for SC III, 4 for SC IV.
#' @param sourceclass source type, 0 for crustal, 3 for Upper mantle.
#' @param ftype style of faulting, -1 for Normal, -0.5 for Normal-Oblique, 0 for Strike-Slip, 0.5 for Reverse-Oblique and 1 for Reverse.
#' @param depth focal depth(km).
#'
#' @return A list will be return, including mag, Rrup, ftype, lnY, sigma, sclass, specT, attenName, period, iflag, sourcetype, depth, phi, tau.
#'
#' @examples
#' Zh16Cru(6, 20, 0, 1.0, 0, 0.0, 10)
#' Zh16Cru(7, 10, 0, 1.0, 0, 0.0, 10)
#'
#' @export
Zh16Cru <- function(Mag, Rrup, ftype=0, sclass=1.0, Prd=0, sourceclass=0.0, depth=10){
  #subroutine Zhaoetal2016_cru ( m, dist, ftype, lnY, sigma, sclass, specT,
  #                   attenName, period1, iflag, sourcetype, depth, phiT, tauT )
  # Set attenuation name
  #     Sourcetype = 0 Crustal
  #     Sourcetype = 1 Subduction - Interface
  #     Sourcetype = 2 Subduction - Slab
  #     Sourcetype = 3 Upper mantle
  #     Sclass = 0 Hard Rock
  #     Sclass = 1 SC I
  #     Sclass = 2 SC II
  #     Sclass = 3 SC III
  #     Sclass = 4 SC IV
  if (Prd != 0 & (Prd < 0.01 | Prd > 5)) {
    stop("Period out of range! \n\n")
  }
  sourceclass <- 0   # set for crustal model
  retvals <- .Fortran("Zhaoetal2016_cru", m=as.single(Mag), dist=as.single(Rrup), ftype=as.single(ftype),
                      lnY=as.single(0.1), sigma=as.single(0.1), sclass=as.single(sclass),
                      specT=as.single(Prd), attenName=as.character("attenName"), period1=as.single(0), iflag=as.integer(0),
                      sourcetype=as.single(sourceclass), depth=as.single(depth),
                      phiT=as.single(0.0), tauT=as.single(0.0))
  names(retvals) <- c("mag", "Rrup", "ftype", "lnY", "sigma", "sclass", "specT", "attenName", "period", "iflag",
                      "sourcetype", "depth", "phi", "tau")
  retvals$attenName <- NULL
  return(retvals)
}
