#' GMPE function for Lin and Lee(2008)
#'
#' \code{LL08} returns the ground-motion prediction (Rock or Soil site base on Vs30)
#' with it sigma of Lin and Lee(2008) GMPE.
#'
#'Lin, P., and C. Lee (2008), Ground-motion attenuation relationships for subduction-zone earthquakes in
#'northeastern taiwan, Bulletin of the Seismological Society of America, 98(1), 220-240.
#'\url{http://dx.doi.org/10.1785/0120060002}
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param depth hypocentral depth(km).
#' @param ftype fytpe=0 for interface, ftype=1 for intraslab
#' @param Prd Period of spectral acceleration.
#' @param Vs30 Vs30(m/s)
#'
#' @return A list will be return, including mag, Rrup, specT, period, lnY, sigma, iflag, depth, ftype.
#'
#' @examples
#' LL08(6, 20, 10, 0, 0, 760)
#' LL08(7, 10, 10, 0, 0, 760)
#'
#' @export
LL08 <- function(Mag, Rrup, depth, ftype, Prd, Vs30=760) {
  if (Prd != 0 & (Prd < 0.01 | Prd > 5)) {
    stop("Period out of range! \n\n")
  }
  if (Vs30 > 360){
    retvals <- .Fortran("LinLee08rock", mag=as.single(Mag), rupdist=as.single(Rrup), specT=as.single(Prd),
                        period=as.single(0), lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(1),
                        depth=as.single(depth), ftype=as.single(ftype)
    )
  } else {
    retvals <- .Fortran("LinLee08soil", mag=as.single(Mag), rupdist=as.single(Rrup), specT=as.single(Prd),
                        period=as.single(0), lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(1),
                        depth=as.single(depth), ftype=as.single(ftype)
    )
  }
  names(retvals) <- c("mag", "Rrup", "specT", "period", "lnY", "sigma", "iflag", "depth", "ftype")
  return(retvals)
}

#' GMPE function for Lin and Lee(2008)
#'
#' \code{LL08Rock} returns the ground-motion prediction (rock site) with it sigma of Lin and Lee(2008) GMPE.
#'
#'Lin, P., and C. Lee (2008), Ground-motion attenuation relationships for subduction-zone earthquakes in
#'northeastern taiwan, Bulletin of the Seismological Society of America, 98(1), 220-240.
#'\url{http://dx.doi.org/10.1785/0120060002}
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param depth hypocentral depth(km).
#' @param ftype fytpe=0 for interface, ftype=1 for intraslab
#' @param Prd Period of spectral acceleration.
#'
#' @return A list will be return, including mag, Rrup, specT, period, lnY, sigma, iflag, depth, ftype.
#'
#' @examples
#' LL08Rock(6, 20, 10, 0, 0)
#' LL08Rock(7, 10, 10, 0, 0)
#'
#' @export
LL08Rock <- function(Mag, Rrup, depth, ftype, Prd) {
  #  subroutine LinLee08rock ( mag, rupdist, specT, period, lnY, sigma, iflag, depth, ftype )
  if (Prd != 0 & (Prd < 0.01 | Prd > 5)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("LinLee08rock", mag=as.single(Mag), rupdist=as.single(Rrup), specT=as.single(Prd),
                      period=as.single(0), lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(1),
                      depth=as.single(depth), ftype=as.single(ftype)
  )
  names(retvals) <- c("mag", "Rrup", "specT", "period", "lnY", "sigma", "iflag", "depth", "ftype")
  return(retvals)
}

#' GMPE function for Lin and Lee(2008)
#'
#' \code{LL08Soil} returns the ground-motion prediction (soil site) with it sigma of Lin and Lee(2008) GMPE.
#'
#'Lin, P., and C. Lee (2008), Ground-motion attenuation relationships for subduction-zone earthquakes in
#'northeastern taiwan, Bulletin of the Seismological Society of America, 98(1), 220-240.
#'\url{http://dx.doi.org/10.1785/0120060002}
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param depth hypocentral depth(km).
#' @param ftype fytpe=0 for interface, ftype=1 for intraslab
#' @param Prd Period of spectral acceleration.
#'
#' @return A list will be return, including mag, Rrup, specT, period, lnY, sigma, iflag, depth, ftype.
#'
#' @examples
#' LL08Soil(6, 20, 10, 0, 0)
#' LL08Soil(7, 10, 10, 0, 0)
#'
#' @export
LL08Soil <- function(Mag, Rrup, depth, ftype, Prd) {
  if (Prd != 0 & (Prd < 0.01 | Prd > 5)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("LinLee08soil", mag=as.single(Mag), rupdist=as.single(Rrup), specT=as.single(Prd),
                      period=as.single(0), lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(1),
                      depth=as.single(depth), ftype=as.single(ftype)
  )
  names(retvals) <- c("mag", "Rrup", "specT", "period", "lnY", "sigma", "iflag", "depth", "ftype")
  return(retvals)
}


