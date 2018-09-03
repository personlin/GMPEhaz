#' GMPE function for adjusted Lin and Lee(2008)
#'
#' \code{LL08.F04} returns the ground-motion prediction with it sigma of adjusted Lin and Lee(2008) GMPE.
#'
#'Lin, P., and C. Lee (2008), Ground-motion attenuation relationships for subduction-zone earthquakes in
#'northeastern taiwan, Bulletin of the Seismological Society of America, 98(1), 220-240.
#'\url{http://dx.doi.org/10.1785/0120060002}
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Ztor Depth to the top of the finite rupture model (km).
#' @param ftype fytpe=0 for interface, ftype=1 for intraslab
#' @param Prd Period of spectral acceleration.
#' @param Vs30 Vs30(m/s)
#'
#' @return A list will be return, including mag, Rrup, specT, period, lnY, sigma, iflag, Ztor, ftype, Vs30.
#'
#' @examples
#' LL08.F04(6, 20, 10, 0, 0, 760)
#' LL08.F04(7, 10, 10, 0, 0, 760)
#'
#' @export
LL08.F04 <- function(Mag, Rrup, Ztor, ftype=0, Prd, Vs30=760) {
  # subroutine S04_LL08_E04 ( mag, rupdist, specT, period, lnY, sigma,
  #                           iflag, Ztor, ftype, vs30)
  if (Prd != 0 & (Prd < 0.01 | Prd > 5)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("S04_LL08_F04", mag=as.single(Mag), rupdist=as.single(Rrup), specT=as.single(Prd),
                        period=as.single(0), lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(1),
                        Ztor=as.single(Ztor), ftype=as.single(ftype), vs30=as.single(Vs30))
  names(retvals) <- c("mag", "Rrup", "specT", "period", "lnY", "sigma", "iflag", "Ztor", "ftype", "Vs30")
  return(retvals)
}


