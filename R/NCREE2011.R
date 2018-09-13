#' GMPE function for NCREE(2011)
#'
#' \code{NCREE2011} returns the ground-motion prediction (rock site, Vs30>=360m/s)
#' with it sigma of NCREE(2011) GMPE.
#'
#' 國家地震工程研究中心(2011)，面震源與衰減律參數研究及核電廠地盤反應量測，行政院原子能委員會核能研究所委託研究報告。
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
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

