#' GMPE function for Boore et al.(2014) NGA-West2 model
#'
#' \code{BSSA14.tw.B01} returns the ground-motion prediction with it sigma of Boore et al.(2014) GMPE
#' adjusted to Taiwan.
#'
#'Boore, D. M., J. P. Stewart, E. Seyhan, and G. M. Atkinson (2014), NGA-west2 equations for predicting
#'PGA, PGV, and 5% damped PSA for shallow crustal earthquakes, Earthquake Spectra, 30(3), 1057-1085.
#'\url{http://dx.doi.org/10.1193/070113EQS184M}
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rjb Joyner and Boore distance(km), Numeric.
#' @param Prd Period of spectral acceleration.
#' @param Vs30 Vs30(m/s).
#' @param ftype style of faulting.
#' @param Z1.0 the depth to the shear-wave velocity horizon of 1.0(km/s).
#' @param regionflag Regional attenuation flag, 0 = Global, 1 = China-Turkey, 2 = Italy-Japan.
#' @param basinflag Basin Adjustments.
#'
#' @return A list will be return, including mag, Rjb, specT, period, lnY, sigma, iflag, Vs30, ftype, pga4nl,
#' Z10, regionflag, basinflag, phi, tau.
#'
#' @examples
#' BSSA14.tw.B01(6, 20, 0, 760, 0, 0.5, 0, 0)
#' BSSA14.tw.B01(7, 20, 0, 760, 0, 0.5, 0, 0)
#'
#' @export
BSSA14.tw.B01 <- function(Mag, Rjb, Prd, Vs30, ftype=0, Z1.0, regionflag=0, basinflag=0){
  #     Notes:
  #            Region Flag:
  #               0 = Global
  #               1 = China-Turkey
  #               2 = Italy-Japan
  # ---------------------------------------------------------------------------
  #BSSA_NGAWest2_2013 ( mag, Rbjf, specT,
  #     1        period2, lnY, sigma, iflag, vs, ftype, pga4nl, z10, regionflag, basinflag,
  #     1        phi, tau )
  # Model Number = 2925
  if (Prd != 0 & (Prd < 0.01 | Prd > 10)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("BSSA14_TW_B01", mag=as.single(Mag), Rbjf=as.single(Rjb),
                      specT=as.single(Prd), period2=as.single(0), lnY=as.single(0.1),
                      sigma=as.single(0.1), iflag=as.integer(0), vs=as.single(Vs30),
                      ftype=as.single(ftype), pga4nl=as.single(0.0), z10=as.single(Z1.0),
                      regionflag=as.integer(regionflag), basinflag=as.integer(basinflag),
                      phi=as.single(0.0), tau=as.single(0.0))
  names(retvals) <- c("mag", "Rjb", "specT", "period", "lnY", "sigma", "iflag", "Vs30",
                      "ftype", "pga4nl", "Z1.0", "regionflag", "basinflag", "phi", "tau")
  return(retvals)
}
