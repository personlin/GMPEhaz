#' GMPE function for Phung et al.(2018)
#'
#' \code{PhungSub18} returns the ground-motion prediction with it sigma of Phung et al.(2018) GMPE.
#'
#'Phung et al.(2018) Subduction GMPE
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
#' PhungSub18(6, 20, 0, 0, 760, 0.5, 0, 1)
#' PhungSub18(7, 10, 0, 1, 760, 0.5, 0, 1)
#'
#' @export
PhungSub18 <- function(Mag, Rrup, Prd, ftype=0, Vs30, Z1.0, Ztor, regionflag=1) {
  # subroutine S04_PhungSub2018 ( mag, rRup, vs30, Z10, ZTor, lnY, sigma,
  #                               specT, period2, iflag, regionflag, ftype )
  if (Prd != 0 & (Prd < 0.01 | Prd > 5)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("S04_PhungSub2018", mag=as.single(Mag), rRup=as.single(Rrup),
                      vs30=as.single(Vs30), Z10=as.single(Z1.0), ZTor=as.single(Ztor), lnY=as.single(0.1),
                        sigma=as.single(0.1), specT=as.single(Prd), period2=as.single(0),
                        iflag=as.integer(1), regionflag=as.integer(regionflag), ftype=as.single(ftype))
  names(retvals) <- c("mag", "Rrup", "Vs30", "Z1.0", "Ztor", "lnY", "sigma", "specT",
                      "period", "iflag", "regionflag", "ftype")
  return(retvals)
}
