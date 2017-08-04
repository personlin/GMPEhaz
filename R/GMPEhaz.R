#' GMPEhaz: A package for wrapping GMPE functions from Norman Abrahamson's fortran HAZ code.
#'
#' The GMPEhaz package provides several GMPE functions:
#' AB03, AC10, ASB13, ASK14, BCHydroSubV3, BI14, BSSA14, CB14, CY14, Gregor06, Kanno06, Lin11, LL08, Youngs97, Zh06.
#'
#'
#' @docType package
#' @name GMPEhaz
#' @useDynLib GMPEhaz, .registration = TRUE
#' @param libpath library path

.onUnload <- function(libpath){
  library.dynam.unload("GMPEhaz", libpath)
}

#atten2 load Fortran code in R for windows
# # ---- 2015.11.19 Person

# # ---- 2015.12.02 Brian
#
# 1. This atten2.DLL is a 32bit DLL and can only be used in 32-bit version of R
# 2. Since arguments to the function call vary from GMPE to GMPE, I have to write a R function for each GMPEs in the DLL
# 2. I write a wrapper function 'atten2' to
#  a. Vectorize calculation for multiple periods of a single scenario, or for multiple scenarios of a single period
#  b. To the extent possible, wrapper function should use a common data format (say, similar to that used in function CY14).
#     Variables in the data frame are matched up to the function arguments of an indidividual GMPE.
#

# # ---- 2016.06.20 Person
# add more GMPE to fit GMC sensitivity analysis

#     fType     Mechanism                      Rake
#     ------------------------------------------------------
#      -1       Normal                    -120 < Rake <  -60
#     -0.5      Normal/Oblique            -150 < Rake < -120
#                                          -60 < Rake <  -30
#       0       Strike-Slip               -180 < Rake < -150
#                                          -30 < Rake <   30
#                                          150 < Rake <  180
#      0.5      Reverse/Oblique             30 < Rake <   60
#                                          120 < Rake <  150
#       1       Reverse                     60 < Rake <  120

# variable in Haz program (for calling GMPE subroutine)
# mag, rupDist, jbdist, ftype, vs, vs30_class, specT, period2, lnY, pga4nl,
# depth, depthtop, depthvs10, d25, rupwidth, Rx, Ry0, dip,
# iflag, hwflag, regionflag, basinflag, msasflag, sigma, phi, tau

# variable in subroutine meanInten (rupdist, jbdist, seismodist, hwflag,
#  mag, jcalc, specT, lnY, siga, ftype, attenName, period1, iAtten, iProb,
#  jType, vs, depth,intflag, AR, dip1, disthypo, depthvs10,
#  depthvs15, D25, tau, depthTop, Theta_Site, RupWidth, vs30_class,
#  foreArc, Rx, phi, cfcoefrrup, cfcoefrjb, Ry0 )

# variable in this program
# mag, Rrup, Rjb, Rseis, Rhypo, ftype, Vs30, vs30_class, sclass, specT, period, lnY, pga4nl, deltac1,
# depth, Ztor, Z10, Z15, Z25, rupwidth, Rx, Ry0, dip,
# sourcetype, iflag, hwflag, forearc, regionflag, basinflag, msasflag, sigma, phi, tau

# ftype
# for subduction GMPE, fytpe=0 for interface, ftype=1 for intraslab
# for crustal GMPE, ftype=0 for SS, ftype=0.5 for RO, ftype=1 for RV,
#                   ftype=-0.5 for NO, ftype=-1 for NM



# Mag, Rrup, Rjb, ftype, vs, vs30_class, specT, period2, lnY, pga4nl,
# depth, depthtop, Z10, Z25, rupwidth, Rx, Ry0, dip,
# iflag, hwflag, regionflag, basinflag, msasflag, sigma, phi, tau

# save cnames
# save(list=ls(pattern = "cnames.*"), file="data/cnames.rda")
