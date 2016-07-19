#'@importFrom lattice xyplot
#'@export
# ---- Wrapper function  !!--not finished--!! ----
atten2 <- function(GMPEname, Periods, Mag, ClstD, Ztor=0, RV=0, plot=F) {

  if(!GMPEname %in% c("ll08rock", "ll08soil", "loh96", "Camp03_H", "CY14Haz", "ASK14Haz")) stop(paste0("GMPE ", GMPEname, " not yet implemented!"))

  # Make sure Data includes only the needed arguments to the function named in GMPEname
  Data <- data.frame(Mag=Mag, ClstD=ClstD, Ztor=Ztor, RV=RV, Prd=Periods)  # .. need to include other variables
  if (GMPEname == 'loh96') Data <- Data[, c("Mag","ClstD")]

  vals <- sapply(1:(dim(Data)[1]), FUN=function(i) {do.call(GMPEname, Data[i, ])})
  vals <- apply(vals, 1, unlist)
  vals <- as.data.frame(vals)
  vals$lnY <- exp(vals$lnY)/981.0

  if (plot) {
    if (length(Mag) > 1)     p <- xyplot(lnY ~ mag, data=vals, xlab='M', ylab='PSA (g)', type=c("l","g"), scales=list(y=list(log=T)))
    if (length(Periods) > 1) p <- xyplot(lnY ~ period, data=vals, xlab='Period (s)', ylab='PSA (g)', type=c("l","g"), scales=list(log=T))
    if (length(ClstD) > 1)   p <- xyplot(lnY ~ rupdist, data=vals, xlab='Distance (km)', ylab='PSA (g)', type=c("l","g"), scales=list(log=T))
    print(p)
  }
  return(vals)
}
