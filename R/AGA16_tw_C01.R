#' GMPE function for adjusted Abrahamson et al.(2016)
#'
#' \code{AGA16.tw.C01} returns the ground-motion prediction with it sigma of Abrahamson et al.(2016) GMPE
#' adjusted to Taiwan.
#'
#'Norman Abrahamson, Nicholas Gregor, and Kofi Addo(2016) BC Hydro Ground Motion Prediction Equations
#'for Subduction Earthquakes, Earthquake Spectra, Vol. 32, No. 1, pp. 23-44.
#'\url{http://dx.doi.org/10.1193/051712EQS188MR}
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Prd Period of spectral acceleration.
#' @param ftype fytpe=0 for interface (use rupture distance), ftype=1 for intraslab (use hypocentral distance)
#' @param Vs30 Vs30(m/s).
#' @param Ztor Depth to the top of the finite rupture model (km).
#' @param depth hypocentral depth(km).
#' @param Rhypo hypocentral distance(km)
#'
#' @return A list will be return, including mag, ftype, Rrup, Vs30, lnY, sigma, specT, period, iflag, forearc, depth, Rhypo, deltac1.
#'
#' @examples
#' AGA16.tw.C01(6, 20, 0, 0, 760, 3, 10, 20)
#' AGA16.tw.C01(7, 10, 0, 0, 760, 0, 10, 20)
#'
#' @export
AGA16.tw.C01 <- function(Mag, Rrup, Prd, ftype=0, Vs30, Ztor, depth, Rhypo) {
  #subroutine       subroutine AGA16_TW_C01 ( mag, fType, rRup, vs30, lnSa, sigma1, specT, period1, iflag, Ztor, depth, disthypo)
  if (Prd != 0 & (Prd < 0.01 | Prd > 10)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("S04_AGA16_TW_C01", mag=as.single(Mag), fType=as.single(ftype), rRup=as.single(Rrup),
                      vs30=as.single(Vs30), lnSa=as.single(0.1), sigma1=as.single(0.1),
                      specT=as.single(Prd), period1=as.single(0), iflag=as.integer(1),
                      Ztor=as.single(Ztor), depth=as.single(depth), disthypo=as.single(Rhypo))
  names(retvals) <- c("mag", "ftype", "Rrup", "Vs30", "lnY", "sigma", "specT",  "period", "iflag",
                      "Ztor", "depth", "Rhypo")
  return(retvals)
}
