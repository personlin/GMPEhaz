#' GMPE function for Youngs et al.(1997)
#'
#' \code{Youngs97Rock} returns the ground-motion prediction (rock site) with it sigma of Youngs et al.(1997) GMPE.
#'
#'Youngs, R. R., S.-J. Chiou, W. J. Silva, and J. R. Humphrey (1997), Strong ground motion attenuation
#'relationships for subduction zone earthquakes, Seismological Research Letters, 68(1), 58-73.
#'\url{http://dx.doi.org/10.1785/gssrl.68.1.58}
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Prd Period of spectral acceleration.
#' @param ftype fytpe=0 for interface, ftype=1 for intraslab
#' @param depth hypocentral depth(km).
#'
#' @return A list will be return, including mag, Rrup, lnY, sigma, period, specT, ftype, depth, iflag.
#'
#' @examples
#' Youngs97Rock(6, 20, 0, 0, 10)
#' Youngs97Rock(7, 10, 0, 0, 10)
#'
#' @export
Youngs97Rock <- function(Mag, Rrup, Prd, ftype=0, depth) {
  #subroutine youngs97_rock ( mag, rupDist, lnY, sigma, attenName, period, specT, ftype, depth,iflag )
  if (Prd != 0 & (Prd < 0.075 | Prd > 3)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("youngs97_rock", mag=as.single(Mag), rupdist=as.single(Rrup), lnY=as.single(0.1),
                      sigma=as.single(0.1), attenName=as.character("attenName"), period=as.single(0), specT=as.single(Prd),ftype=as.single(ftype),
                      depth=as.single(depth), iflag=as.integer(1))
  names(retvals) <- c("mag", "Rrup", "lnY", "sigma", "attenName", "period", "specT", "ftype", "depth", "iflag")
  retvals$attenName <- NULL
  return(retvals)
}

#' GMPE function for Youngs et al.(1997)
#'
#' \code{Youngs97Soil} returns the ground-motion prediction (soil site) with it sigma of Youngs et al.(1997) GMPE.
#'
#'Youngs, R. R., S.-J. Chiou, W. J. Silva, and J. R. Humphrey (1997), Strong ground motion attenuation
#'relationships for subduction zone earthquakes, Seismological Research Letters, 68(1), 58-73.
#'\url{http://dx.doi.org/10.1785/gssrl.68.1.58}
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Prd Period of spectral acceleration.
#' @param ftype fytpe=0 for interface, ftype=1 for intraslab
#' @param depth hypocentral depth(km).
#'
#' @return A list will be return, including mag, Rrup, lnY, sigma, period, specT, ftype, depth, iflag.
#'
#' @examples
#' Youngs97Soil(6, 20, 0, 0, 10)
#' Youngs97Soil(7, 10, 0, 0, 10)
#'
#' @export
Youngs97Soil <- function(Mag, Rrup, Prd, ftype=0, depth) {
  #subroutine youngs97_soil ( mag, rupDist, lnY, sigma, attenName, period, specT, ftype, depth, iflag )
  if (Prd != 0 & (Prd < 0.075 | Prd > 4)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("youngs97_soil", mag=as.single(Mag), rupdist=as.single(Rrup), lnY=as.single(0.1),
                      sigma=as.single(0.1), attenName=as.character("attenName"), period=as.single(0), specT=as.single(Prd),ftype=as.single(ftype),
                      depth=as.single(depth), iflag=as.integer(1))
  names(retvals) <- c("mag", "Rrup", "lnY", "sigma", "attenName", "period", "specT", "ftype", "depth", "iflag")
  retvals$attenName <- NULL
  return(retvals)
}
