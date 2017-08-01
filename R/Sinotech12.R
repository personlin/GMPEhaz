#' GMPE function for Sinotech(2012)
#'
#' \code{Sinotech12} returns the ground-motion prediction with it sigma of Sinotech(2012) GMPE.
#'
#'Sinotech (2012). (in Chinese).
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Prd Period of spectral acceleration.
#' @param Vs30 Vs30(m/s)
#' @param ftype style of faulting.
#'
#' @return A list will be return, including mag, Rrup, specT, period, lnY, sigma, Vs30, iflag.
#'
#' @examples
#' Sinotech12(6, 20, 0, 760, 0)
#' Sinotech12(7, 10, 0, 760, 0)
#'
#' @export
Sinotech12 <- function(Mag, Rrup, Prd, Vs30=760, ftype){
#   subroutine TG09221_2012(mag, rupDist, specT, period1, lnY, sigma, vs, iflag, ftype )
#     fType     Mechanism                      Rake
#     ------------------------------------------------------
#      -1       Normal                   -120 < Rake < -60.0
#     1, 0.5    Reverse and Rev/Obl        30 < Rake < 150.0
#     0,-0.5    Strike-Slip and NMl/Obl        Otherwise
  if (Prd != 0 & (Prd < 0.01 | Prd > 10)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("TG09221_2012", mag=as.single(Mag), rupdist=as.single(Rrup), specT=as.single(Prd),
                      period1=as.single(0), lnY=as.single(0.1), sigma=as.single(0.1),
                      vs=as.single(Vs30), iflag=as.integer(1), ftype=as.single(ftype))
  names(retvals) <- c("mag", "Rrup", "specT", "period", "lnY", "sigma", "Vs30", "iflag", "ftype")
  return(retvals)
}
