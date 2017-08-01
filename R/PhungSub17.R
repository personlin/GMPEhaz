#' GMPE function for Phung et al.(2017)
#'
#' \code{PhungInt17} returns the ground-motion prediction with it sigma of Phung et al.(2017) GMPE.
#'
#'Phung et al.(2017) Subduction GMPE
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Prd Period of spectral acceleration.
#' @param ftype fytpe=0 for interface, ftype=1 for intraslab.
#' @param Vs30 Vs30(m/s).
#' @param Ztor Depth to the top of the finite rupture model (km).
#' @param Z1.0 the depth to the shear-wave velocity horizon of 1.0(km/s).
#' @param regionflag 0 Japan, 1 Taiwan
#'
#' @return A list will be return, including mag, ftype, Rrup, Vs30, lnY, sigma, specT, period, iflag, forearc, depth, Rhypo.
#'
#' @examples
#' PhungSub17(6, 20, 0, 0, 760, 0.5, 0, 1)
#' PhungSub17(7, 10, 0, 1, 760, 0.5, 0, 1)
#'
#' @export
PhungSub17 <- function(Mag, Rrup, Prd, ftype=0, Vs30, Z1.0, Ztor, regionflag=1) {
  #         subroutine PhungSub2017 ( mag, dist, vs, Z10, ZTor, lnY, sigma,  ftype,
  #                    specT, period2, iflag, regionflag )
  if (Prd != 0 & (Prd < 0.01 | Prd > 5)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("PhungSub2017", mag=as.single(Mag), dist=as.single(Rrup),
                        vs=as.single(Vs30), Z10=as.single(Z1.0), ZTor=as.single(Ztor), lnY=as.single(0.1),
                        sigma=as.single(0.1), ftype=as.single(ftype), specT=as.single(Prd), period2=as.single(0),
                        iflag=as.integer(1), regionflag=as.integer(regionflag))
  names(retvals) <- c("mag", "Rrup", "Vs30", "Z1.0", "Ztor", "lnY", "sigma", "ftype", "specT",
                      "period", "iflag", "regionflag")
  return(retvals)
}
