
#' GMPE function for Loh et al.(1996) PGA only
#'
#' \code{Loh96} returns the ground-motion prediction (rock site) with it sigma of Loh et al.(1996) GMPE.
#'
#'
#' @param Mag Earthquake local magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#'
#' @return A list will be return, including mag, Rrup, lnY, sigma, period.
#'
#' @examples
#' Loh96(6, 20)
#' Loh96(7, 10)
#'
#' @export
Loh96 <- function(Mag, Rrup) {
  retvals <- .Fortran("loh96", mag=as.single(Mag), rupDist=as.single(Rrup),
                      lnY=as.single(0.1), sigma=as.single(1), attenName=as.character("attenName"), period=as.single(0))
  names(retvals) <- c("mag", "Rrup", "lnY", "sigma", "attenName", "period")
  retvals$attenName <- NULL
  return(retvals)
}
