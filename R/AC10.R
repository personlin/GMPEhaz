#' GMPE function for Akkar and Cagnan(2010)
#'
#' \code{AC10} returns the ground-motion prediction with it sigma of Akkar and Cagnan(2010) GMPE.
#'
#'Akkar, S., and Z. Cagnan (2010), A local ground-motion predictive model for turkey,
#'and its comparison with other regional and global ground-motion models,
#'Bulletin of the Seismological Society of America, 100(6), 2978-2995.
#'\url{http://dx.doi.org/10.1785/0120090367}
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rjb Joyner and Boore distance(km), Numeric.
#' @param Prd Period of spectral acceleration.
#' @param ftype style of faulting.
#' @param Vs30 Vs30(m/s).
#'
#' @return A list will be return, including mag, Rjb, specT, period, lnY, sigma, iflag, Vs30, ftype, pga4nl.
#'
#' @examples
#' AC10(6, 20, 0, 0, 760)
#' AC10(7, 10, 0, 0, 760)
#'
#' @export
AC10 <- function(Mag, Rjb, Prd=0, ftype=0, Vs30=760){
  #AC_2010 ( mag, Rbjf, specT,
  #                     period2, lnY, sigma, iflag, vs, ftype, pga4nl )
  if (Prd != 0 & Prd != -1 & (Prd < 0.01 | Prd > 2)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("AC_2010", mag=as.single(Mag), Rbjf=as.single(Rjb), specT=as.single(Prd),
                      period2=as.single(0), lnY=as.single(0.1), sigma=as.single(0.1),
                      iflag=as.integer(0), vs=as.single(Vs30), ftype=as.single(ftype),
                      pga4nl=as.single(0.0))
  names(retvals) <- c("mag", "Rjb", "specT", "period", "lnY", "sigma", "iflag", "Vs30", "ftype",
                      "pga4nl")
  return(retvals)
}
