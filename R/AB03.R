#' GMPE function for Atkinson and Boore(2003)
#'
#' \code{AB03} returns the ground-motion prediction with it sigma of Atkinson and Boore(2003) GMPE.
#'
#'Atkinson, G., and D. Boore (2003), Empirical ground-motion relations for subduction-zone earthquakes
#'and their application to cascadia and other regions, Bulletin of the Seismological Society of America,
#'93(4), 1703-1729.
#'\url{http://dx.doi.org/10.1785/0120020156}
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Prd Period of spectral acceleration.
#' @param ftype style of faulting.
#' @param depth hypocentral depth(km).
#' @param Sc NEHRP B Sc=0, Sd=0, Se=0, Vs>760m/s, NEHRP C Sc=1, Sd=0, Se=0, 360<Vs<760
#' @param Sd NEHRP D Sc=0, Sd=1, Se=0, 180<Vs<360
#' @param Se NEHRP E Sc=0, Sd=0, Se=1, Vs<180
#'
#' @return A list will be return, including mag, Rrup, lnY, sigma, specT, period, iflag, ftype, depth, Sc, Sd, Se.
#'
#' @examples
#' AB03(6, 20, 0, 0, 10, 0, 0, 0)
#' AB03(7, 10, 0, 0, 10, 0, 0, 0)
#'
#' @export
AB03 <- function(Mag, Rrup, Prd, ftype=0, depth, Sc=0, Sd=0, Se=0) {
  #subroutine subroutine AB03 ( mag, rupdist, lnY, sigma, specT, attenName, period,iflag, ftype, depth, Sc, Sd, Se)
  #     Site Classses are as follows:
  #          NEHRP B --> Sc=0, Sd=0, Se=0   Vs>760m/s
  #          NEHRP C --> Sc=1, Sd=0, Se=0   360<Vs<760
  #          NEHRP D --> Sc=0, Sd=1, Se=0   180<Vs<360
  #          NEHRP E --> Sc=0, Sd=0, Se=1   Vs<180
  if (Prd != 0 & (Prd < 0.04 | Prd > 3)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("AB03", mag=as.single(Mag), rupdist=as.single(Rrup), lnY=as.single(0.1),
                      sigma=as.single(0.1), specT=as.single(Prd), attenName=as.character("attenName"),
                      period=as.single(0), iflag=as.integer(1),
                      ftype=as.single(ftype), depth=as.single(depth), Sc=as.single(Sc), Sd=as.single(Sd),
                      Se=as.single(Se))
  names(retvals) <- c("mag", "Rrup", "lnY", "sigma", "specT", "attenName", "period", "iflag", "ftype", "depth",
                      "Sc", "Sd", "Se")
  retvals$attenName <- NULL
  return(retvals)
}
