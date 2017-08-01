#' GMPE function for Campbell and Bozorgnia(2003)
#'
#' \code{Camp03H} returns the ground-motion prediction with it sigma of Campbell and Bozorgnia(2003) GMPE.
#'
#'Campbell, Kenneth W. and Bozorgnia, Yousef (2003), Updated Near-Source Ground-Motion (Attenuation)
#'Relations for the Horizontal and Vertical Components of Peak Ground Acceleration and
#'Acceleration Response Spectra, Bulletin of the Seismological Society of America, 93(1), 314-331.
#'\url{http://dx.doi.org/10.1785/0120020029}
#'
#' @param Mag Earthquake local magnitude, Numeric.
#' @param Rjb Rupture distance(km), Numeric.
#' @param Rseis Seismogenic distance(km)
#' @param Prd Period of spectral acceleration.
#' @param Svfs Svfs
#' @param Ssr Ssr
#' @param Sfr Sfr
#' @param Frv Frv
#' @param Fth Fth
#' @param hwflag hanging-wall flag, 1 for hanging-wall.
#'
#' @return A list will be return, including mag, Rseis, Rjb, lnY, sigma, specT, period, Svfs, Ssr, Sfr, Frv, Fth, hwflag, iflag.
#'
#' @examples
#' Camp03H(6, 20, 20, 0, 0, 0, 0, 0, 0)
#' Camp03H(7, 10, 20, 0, 0, 0, 0, 0, 0)
#'
#' @export
# ---- need to be rewrite !! ----
Camp03H <- function(Mag, Rjb, Rseis, Prd, Svfs, Ssr, Sfr, Frv, Fth, hwflag=0) {
  if (Prd != 0 & (Prd < 0.05 | Prd > 4)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("Camp03_H", mag=as.single(Mag), seismodist=as.single(Rseis), jbDist=as.single(Rjb),
                      lnY=as.single(0.1), sigma=as.single(0.1), specT=as.single(Prd), period1=as.single(0),
                      Svfs=as.single(Svfs), Ssr=as.single(Ssr), Sfr=as.single(Sfr),
                      Frv=as.single(Frv), Fth=as.single(Fth), hwflag=as.single(hwflag),
                      iflag=as.single(0))
  names(retvals) <- c("mag", "Rseis", "Rjb", "lnY", "sigma", "specT", "period", "Svfs", "Ssr", "Sfr",
                      "Frv", "Fth", "hwflag", "iflag")
  return(retvals)
}
