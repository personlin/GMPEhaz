#' GMPE function for Montalva et al.(2017)
#'
#' \code{MBR17} returns the ground-motion prediction with it sigma of Montalva et al.(2017) GMPE.
#'
#' Montalva, G. A., Bastias, N., and Rodriguez-Marek, A. (2017). Ground-Motion Prediction Equation for the
#' Chilean Subduction Zone. Bulletin of the Seismological Society of America, 107(2), 901-911.
#' \url{http://doi.org/10.1785/0120160221}
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Prd Period of spectral acceleration.
#' @param ftype fytpe=0 for interface (use rupture distance), ftype=1 for intraslab (use hypocentral distance).
#' @param Vs30 Vs30(m/s).
#' @param forearc 0 = Forearc site, 1 = Backarc site.
#' @param depth hypocentral depth(km).
#' @param Rhypo hypocentral distance(km).
#'
#' @return A list will be return, including mag, ftype, Rrup, Vs30, lnY, sigma, specT, period, iflag, forearc, depth, Rhypo.
#'
#' @examples
#' MBR17(6, 20, 0, 0, 760, 1, 10, 20)
#' MBR17(7, 10, 0, 0, 760, 1, 10, 20)
#'
#' @export
MBR17 <- function(Mag, Rrup, Prd, ftype=0, Vs30, forearc=1, depth, Rhypo) {
  # subroutine Montalva2017 ( mag, fType, rRup, vs30, lnSa, sigma1,
  #                           specT, period1, iflag, forearc, depth1, disthypo )
  if (Prd != 0 & (Prd < 0.02 | Prd > 10)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("Montalva2017", mag=as.single(Mag), ftype=as.single(ftype), rRup=as.single(Rrup),
                      vs30=as.single(Vs30), lnY=as.single(0.1), sigma=as.single(0.1),
                      specT=as.single(Prd), period1=as.single(0), iflag=as.integer(1),
                      forearc=as.integer(forearc), depth=as.single(depth), disthypo=as.single(Rhypo))
  names(retvals) <- c("mag", "ftype", "Rrup", "Vs30", "lnY", "sigma", "specT",  "period", "iflag",
                      "forearc", "depth", "Rhypo")
  return(retvals)
}
