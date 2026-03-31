#' GMPE function for NCREE(2011)
#'
#' \code{NCREE2011} returns the ground-motion prediction (rock site, Vs30>=360m/s)
#' with it sigma of NCREE(2011) GMPE.
#'
#' National Center for Research on Earthquake Engineering (NCREE) (2011). Research on area source and
#' attenuation law parameters and nuclear power plant site response measurement.
#' Commissioned research report by the Atomic Energy Council, Institute of Nuclear Energy Research, Taiwan.
#'
#' @param Mag Earthquake moment magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Prd Period of spectral acceleration.
#'
#' @return A list will be return, including mag, Rrup, specT, period, lnY, sigma.
#'
#' @examples
#' NCREE2011(6, 20, 0)
#' NCREE2011(7, 10, 0)
#'
#' @export
NCREE2011 <- function(Mag, Rrup, Prd){
  # subroutine NCREE_2011(mag, rupDist, specT, period1, lnY, sigma )
  retvals <- .Fortran("NCREE_2011", mag=as.single(Mag), rupdist=as.single(Rrup), specT=as.single(Prd),
                      period1=as.single(0), lnY=as.single(0.1), sigma=as.single(0.1))
  names(retvals) <- c("mag", "Rrup", "specT", "period", "lnY", "sigma")
  return(retvals)
}

