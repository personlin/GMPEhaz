#' Wrap function for all GMPE
#'
#' \code{atten2} returns the ground-motion prediction with it sigma of any GMPE.
#'
#' @param GMPEname Name of GMPE.
#' @param Periods Periods of response spetra
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Ztor Depth to the top of the finite rupture model (km).
#' @param RV Indicator of reverse fault.
#' @param depth Focal depth.
#' @param ftype style of fault.
#' @param plot flag for plot or not.
#'
#' @return A list will be return, including mag, Rjb, ftype, specT, period, lnY, sigma, iflag, Vs30, phi, tau.
#'
#' @examples
#' atten2("LL08Rock", 0, Mag=c(6,7,8), 20, 0, 0, 10, 0, FALSE)
#' atten2("LL08Rock", Periods=c(0.01, 0.1, 0.2, 0.5, 1, 2, 5), 6, 10, 0, 0, 10, 0, FALSE)
#'
#' @importFrom lattice xyplot
#'
#' @export
# ---- Wrapper function  !!--not finished--!! ----
atten2 <- function(GMPEname, Periods, Mag, Rrup, Ztor=0, RV=0, depth, ftype=0, plot=F) {

  if(!GMPEname %in% c("LL08Rock", "LL08Soil", "Loh96", "Camp03H", "CY14", "ASK14")) stop(paste0("GMPE ", GMPEname, " not yet implemented!"))

  # Make sure Data includes only the needed arguments to the function named in GMPEname
  Data <- data.frame(Mag=Mag, Rrup=Rrup, Prd=Periods, depth=depth, ftype=ftype)  # .. need to include other variables
  if (GMPEname == 'Loh96') Data <- Data[, c("Mag","Rrup")]

  vals <- sapply(1:(dim(Data)[1]), FUN=function(i) {do.call(GMPEname, Data[i, ])})
  vals <- apply(vals, 1, unlist)
  vals <- as.data.frame(vals)
  vals$lnY <- exp(vals$lnY)/exp(6.89)

  if (plot) {
    if (length(Mag) > 1)     p <- xyplot(lnY ~ mag, data=vals, xlab='M', ylab='PSA (g)', type=c("l","g"), scales=list(y=list(log=T)))
    if (length(Periods) > 1) p <- xyplot(lnY ~ period, data=vals, xlab='Period (s)', ylab='PSA (g)', type=c("l","g"), scales=list(log=T))
    if (length(Rrup) > 1)   p <- xyplot(lnY ~ rupdist, data=vals, xlab='Distance (km)', ylab='PSA (g)', type=c("l","g"), scales=list(log=T))
    print(p)
  }
  return(vals)
}
