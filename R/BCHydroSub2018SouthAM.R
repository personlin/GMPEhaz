#' GMPE function for Abrahamson et al.(2018)
#'
#' \code{BCHydroSub2018SouthAM} returns the ground-motion prediction with it sigma of Abrahamson et al.(2016) GMPE.
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
#' @param forearc 0 = Forearc site, 1 = Backarc site
#' @param depth hypocentral depth(km).
#' @param Rhypo hypocentral distance(km)
#'
#' @return A list will be return, including mag, ftype, Rrup, Vs30, lnY, sigma, specT, period, iflag, forearc, depth, Rhypo, deltac1.
#'
#' @examples
#' BCHydroSub2018SouthAM(6, 20, 0, 0, 760, 1, 10, 20)
#' BCHydroSub2018SouthAM(7, 10, 0, 0, 760, 1, 10, 20)
#'
#' @export
BCHydroSub2018SouthAM <- function(Mag, Rrup, Prd, ftype=0, Vs30, forearc=1, depth, Rhypo) {
  if (Prd != 0 & (Prd < 0.02 | Prd > 10)) {
    stop("Period out of range! \n\n")
  }
  #subroutine S05_BCHydroSub_southAm ( mag, fType, rRup, vs30, lnSa, sigma1,
  #           specT, period1, iflag, forearc, depth, disthypo, deltac1 )
  retvals <- .Fortran("S05_BCHydroSub_southAm", mag=as.single(Mag), fType=as.single(ftype), rRup=as.single(Rrup),
                      vs30=as.single(Vs30), lnSa=as.single(0.1), sigma1=as.single(0.1),
                      specT=as.single(Prd), period1=as.single(0), iflag=as.integer(1),
                      forearc=as.integer(forearc), depth=as.single(depth), disthypo=as.single(Rhypo),
                      deltac1=as.single(0))
  names(retvals) <- c("mag", "ftype", "Rrup", "Vs30", "lnY", "sigma", "specT",  "period", "iflag",
                      "forearc", "depth", "Rhypo", "deltac1")
  return(retvals)
}
