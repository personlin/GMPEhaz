#' GMPE function for Mcverry et al.(2006) Subduction GMPE.
#'
#' \code{Mcverry2006Sub} returns the ground-motion prediction with it sigma of Mcverry et al.(2006) Subduction GMPE.
#'
#' Mcverry, G. H., Zhao, J. X., Abrahamson, N. A., and Somerville, P. G. (2006). New Zealand acceleration response spectrum
#' attenuation relations for crustal and subduction zone earthquakes,
#' Bulletin of the New Zealand Society for Earthquake Engineering, 39(1), 1-58.
#' \url{http://bulletin.nzsee.org.nz/39/1/0001}
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param ftype fytpe=0 for interface, ftype=1 for intraslab.
#' @param Prd Period of spectral acceleration.
#' @param Ztor Depth to the top of the finite rupture model (km).
#' @param Dip Dip angle of the fault plane.
#' @param rupwidth Down-dip rupture width (km).
#' @param depth hypocentral depth(km).
#' @param Sc 1 for site class C, 0 for others.
#' @param Sd 1 for site class D, 0 for others.
#'
#' @return A list will be return, including mag, Rrup, Rjb, ftype, specT, period, lnY, sigma,
#' iflag, Vs30, Ztor, Z25, dip, depth, hwflag, Rx, rupwidth, regionflag, phi, tau.
#'
#' @examples
#' Mcverry2006Sub(6, 20, 0, 0, 0, 90, 10, 10, 0, 0)
#' Mcverry2006Sub(7, 20, 0, 0, 0, 90, 10, 10, 0, 0)
#'
#' @export
Mcverry2006Sub <- function(Mag, Rrup, Prd, ftype=0, Ztor, Dip, rupwidth, depth, Sc=0, Sd=0){
  # subroutine S02_McVerry_Subduction_2006 ( m, Rrup, specT, period2, lnY, sigma, iflag, Ftype,
  #                                         depthtop, dip, width, hypodepth, Sc, Sd )
  # c     fType     Mechanism
  # C     ----------------------------------
  # c     0         Interface
  # c     1         Intraslab
  if (Prd != 0 & (Prd < 0.01 | Prd > 3)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("S02_McVerry_Subduction_2006", m=as.single(Mag), Rrup=as.single(Rrup),
                        specT=as.single(Prd), period2=as.single(0), lnY=as.single(0.1), sigma=as.single(0.1),
                        iflag=as.integer(0), Ftype=as.single(ftype), depthtop=as.single(Ztor), dip=as.single(Dip),
                        width=as.single(rupwidth), hypodepth=as.single(depth), Sc=as.single(Sc), Sd=as.single(Sd))
  names(retvals) <- c("mag", "Rrup",  "specT", "period", "lnY", "sigma", "iflag", "ftype", "Ztor", "dip",
                      "rupwidth", "depth", "Sc", "Sd")
  return(retvals)
}
