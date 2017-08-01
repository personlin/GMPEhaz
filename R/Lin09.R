#' GMPE function for Lin (2009)
#'
#' \code{Lin09} returns the ground-motion prediction with it sigma of Lin (2009) GMPE.
#'
#'Lin, P.-S. (2009). Ground-motion attenuation relationship and path-effect
#'study using Taiwan data set, Ph.D. Thesis, Nation Central University, Taiwan (in Chinese).
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
#' Lin09(6, 20, 0, 760, 0)
#' Lin09(7, 10, 0, 760, 0)
#'
#' @export
Lin09 <- function(Mag, Rrup, Prd, Vs30=760, ftype){
#      subroutine Lin2009(mag, rupDist, specT, period1, lnY, sigma, vs, iflag, ftype)
#     fType     Mechanism                      Rake
#     ------------------------------------------------------
#      -1       Normal                   -120 < Rake < -60.0
#     1, 0.5    Reverse and Rev/Obl        30 < Rake < 150.0
#     0,-0.5    Strike-Slip and NMl/Obl        Otherwise
  if (Prd != 0 & (Prd < 0.01 | Prd > 5)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("Lin2009", mag=as.single(Mag), rupdist=as.single(Rrup), specT=as.single(Prd),
                      period1=as.single(0), lnY=as.single(0.1), sigma=as.single(0.1),
                      vs=as.single(Vs30), iflag=as.integer(1), ftype=as.single(ftype))
  names(retvals) <- c("mag", "Rrup", "specT", "period", "lnY", "sigma", "Vs30", "iflag", "ftype")
  return(retvals)
}
