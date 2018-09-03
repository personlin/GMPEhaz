#' GMPE function for Chao et al.(2017)
#'
#' \code{Chao2017} returns the ground-motion prediction with it sigma of Chao et al.(2017) GMPE.
#'
#'Chao et al.(2017)
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Prd Period of spectral acceleration.
#' @param Vs30 Vs30(m/s).
#' @param Ztor Depth to the top of the finite rupture model (km).
#' @param Z1.0 the depth to the shear-wave velocity horizon of 1.0(km/s).
#' @param Vs30_class Vs30 data class, 0 for estimated, 1 for measured.
#' @param sourcetype source type, 0 for crustal, 1 for Subduction.
#' @param ftype style of faulting, -1 for Normal, 0 for Strike-Slip, 1 for Reverse for sourcetype = 0.
#' 0 for interface, 1 for intraslab for soucetype = 1.
#' @param msasflag Mainshock and Aftershocks flag, 0 = Mainshocks, 1 = Aftershocks.
#'
#' @return A list will be return, including mag, Rrup, ftype, lnY, sigma, sclass, specT, attenName, period, iflag, sourcetype, depth, phi, tau.
#'
#' @examples
#' Chao2017(6, 20, 0, 0, 760, 0, 0, 0, 0)
#' Chao2017(7, 10, 0, 0, 760, 0, 0, 0, 0)
#'
#' @export
Chao2017 <- function(Mag, Rrup, Prd=0, ftype=0, Vs30=760, Vs30_class=0, Ztor = 0, Z1.0=0.0058959, sourcetype=0,
                     msasflag = 0){
  # subroutine Chaoetal2017 ( mag, dist, ftype, lnY, sigma, specT, vs, Ztor, Z10,
  #                           1            vs30_class, attenName, period1, iflag, sourcetype, phi, tau )
  # c Set attenuation name
  # c     Sourcetype = 0 Crustal
  # c     Sourcetype = 1 Subduction
  # c     Vs30_class = 0 for estimated
  # c     Vs30_class = 1 for measured
  if (Prd != 0 & Prd != -2 & (Prd < 0.01 | Prd > 5)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("S04_Chao2017", mag=as.single(Mag), dist=as.single(Rrup), ftype=as.single(ftype),
                      lnY=as.single(0.1), sigma=as.single(0.1), specT=as.single(Prd), vs=as.single(Vs30),
                      Ztor=as.single(Ztor), Z10=as.single(Z1.0), vs30_class=as.integer(Vs30_class),
                      attenName=as.character("attenName"), period1=as.single(0), iflag=as.integer(0),
                      sourcetype=as.single(sourcetype),
                      phi=as.single(0.0), tau=as.single(0.0), msasflag=as.integer(msasflag))
  names(retvals) <- c("mag", "Rrup", "ftype", "lnY", "sigma", "specT", "Vs30", "Ztor", "Z10", "Vs30_class",
                      "attenName", "period", "iflag", "sourcetype", "phi", "tau", "msasflag")
  retvals$attenName <- NULL
  return(retvals)
}
