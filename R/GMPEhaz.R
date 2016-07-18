#-------------------------------------------------------------------------------
# Program Name: haz43_atten2
# Description:  Load Haz43 GMPE fortran code from Normman Abrahamson's source code
# Date: 2016.01.18
# Author: Person
#
#-------------------------------------------------------------------------------

# if (!is.loaded("src/GMPEhaz.dll")) {
#   dyn.load("src/GMPEhaz.dll")
# }

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

#   Three example R functions for ll08rock, ll08soil, loh96

LL08Rock <- function(Mag, Rrup, depth, ftype, Prd) {
#  subroutine LinLee08rock ( mag, rupdist, specT, period, lnY, sigma, iflag, depth, ftype )
  retvals <- .Fortran("LinLee08rock", mag=as.single(Mag), rupdist=as.single(Rrup), specT=as.single(Prd),
                      period=as.single(0), lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(1),
                      depth=as.single(depth), ftype=as.single(ftype)
  )
  names(retvals) <- c("mag", "Rrup", "specT", "period", "lnY", "sigma", "iflag", "depth", "ftype")
  return(retvals)
}

LL08Soil <- function(Mag, Rrup, depth, ftype, Prd) {
  retvals <- .Fortran("LinLee08soil", mag=as.single(Mag), rupdist=as.single(Rrup), specT=as.single(Prd),
                      period=as.single(0), lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(1),
                      depth=as.single(depth), ftype=as.single(ftype)
  )
  names(retvals) <- c("mag", "Rrup", "specT", "period", "lnY", "sigma", "iflag", "depth", "ftype")
  return(retvals)
}

Loh96 <- function(Mag, Rrup) {
  retvals <- .Fortran("loh96", mag=as.single(Mag), rupDist=as.single(Rrup),
                      lnY=as.single(0.1), sigma=as.single(1), attenName=as.character("attenName"), period=as.single(0))
  names(retvals) <- c("mag", "Rrup", "lnY", "sigma", "attenName", "period")
  retvals$attenName <- NULL
  return(retvals)
}

# ---- need to be rewrite !! ----
Camp03H <- function(Mag, SeismD, JBD, Svfs, Ssr, Sfr, Frv, Fth, hwflag) {
  retvals <- .Fortran("Camp03_H", mag=as.single(Mag), seismodist=as.single(SeismD), jbDist=as.single(JBD),
                      lnY=as.single(0.1), sigma=as.single(0.1), specT=as.single(Prd), period1=as.single(0),
                      Svfs=as.single(Svfs), Ssr=as.single(Ssr), Sfr=as.single(Sfr),
                      Frv=as.single(Frv), Fth=as.single(Fth), hwflag=as.single(hwflag),
                      iflag=as.single(0))
  names(retvals) <- c("mag", "Rseis", "Rjb", "lnY", "sigma", "specT", "period", "Svfs", "Ssr", "Sfr",
                      "Frv", "Fth", "hwflag", "iflag")
  return(retvals)
}

CY14Haz <- function(Mag, Rrup, Rjb, Prd, Vs30, Dip, Ztor, ftype=0, Z1.0, Vs30_class=1, hwflag=0, Rx,
                    regionflag=0){
  #     fType     Mechanism                      Rake
  #------------------------------------------------------#
  #      -1       Normal                   -120 < Rake < -60.0
  #     1, 0.5    Reverse and Rev/Obl        30 < Rake < 150.0
  #     0,-0.5    Strike-Slip and NMl/Obl        Otherwise

  # Vs30_class=1 (measured), 0 (estimated)

  #     Apply Regional scaling factor.
  #     Regionflag = 0 Global
  #     Regionflag = 1 Japan and Italy
  #        Also set sigma2 equal to Japan specific value
  #     Regionflag = 2 Wenchuan (note only for M7.9)

  retvals <- .Fortran("CY_NGAWest2_2013", mag=as.single(Mag), Rrup=as.single(Rrup), Rbjf=as.single(Rjb),
                      specT=as.single(Prd), period2=as.single(0), lnY=as.single(0.1), sigma=as.single(0.1),
                      iflag=as.integer(0), vs=as.single(Vs30), Delta=as.single(Dip), DTor=as.single(Ztor),
                      ftype=as.single(ftype), depthvs10=as.single(Z1.0), vs30_class=as.integer(Vs30_class),
                      hwflag=as.integer(hwflag), Rx=as.single(Rx), regionflag=as.integer(regionflag),
                      phi=as.single(0.0), tau=as.single(0.0))
  names(retvals) <- c("mag", "Rrup", "Rjb", "specT", "period", "lnY", "sigma", "iflag", "Vs30",
                      "dip", "Ztor", "ftype", "Z10", "vs3-_class", "hwflag", "Rx", "regionflag",
                      "phi", "tau")
  return(retvals)
}

ASK14Haz <- function(Mag, Rrup, Rjb, Prd, Dip, ftype=0, rupwidth, Vs30, hwflag=0,
                     Ztor, Vs30_class=1, Z1.0, Rx, Ry0, regionflag, msasflag=0){
  #Vs30_class = 0 for estimated, 1 for measured
  #     For now, convert ftype to an equivalent rake
  #     fType     Mechanism                      Rake
  #     ------------------------------------------------------
  #      -1       Normal                   -120 < Rake < -60.0
  #     1, 0.5    Reverse and Rev/Obl        30 < Rake < 150.0
  #     0,-0.5    Strike-Slip and NMl/Obl        Otherwise
# subroutine ASK_NGAWest2_2013 ( mag, dip, fType, fltWidth, rRup, Rjb,
#                               vs30, hwflag, lnY, sigma, specT, period2, ztor,
#                               iflag, vs30_class, z10, Rx, Ry0, regionflag, msasflag,
#                               phi, tau )
  retvals <- .Fortran("ASK_NGAWest2_2013", mag=as.single(Mag), dip=as.single(Dip), ftype=as.single(ftype),
                      fltWidth=as.single(rupwidth), Rrup=as.single(Rrup), Rjb=as.single(Rjb),
                      vs30=as.single(Vs30),hwflag=as.integer(hwflag), lnY=as.single(0.1), sigma=as.single(0.1),
                      specT=as.single(Prd),period2=as.single(0), ztor=as.single(Ztor), iflag=as.integer(0),
                      vs30_class=as.integer(Vs30_class),z10=as.single(Z1.0), Rx=as.single(Rx), Ry0=as.single(Ry0),
                      regionflag=as.integer(regionflag),msasflag=as.integer(msasflag),
                      phi=as.single(0.0), tau=as.single(0.0))
  names(retvals) <- c("mag", "dip", "ftype", "rupwidth","Rrup", "Rjb", "Vs30", "hwflag", "lnY", "sigma",
                      "specT", "period", "Ztor", "iflag", "vs30_class", "z10", "Rx", "Ry0", "regionflag",
                      "msasflag","phi", "tau")
  return(retvals)
}

BSSA14Haz <- function(Mag, Rjb, Prd, Vs30, ftype=0, pga4nl=0.0, Z1.0, regionflag=0, basinflag=0){
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
  retvals <- .Fortran("BSSA_NGAWest2_2013", mag=as.single(Mag), Rbjf=as.single(Rjb),
                      specT=as.single(Prd), period2=as.single(0), lnY=as.single(0.1),
                      sigma=as.single(0.1), iflag=as.integer(0), vs=as.single(Vs30),
                      ftype=as.single(ftype), pga4nl=as.single(0.0), z10=as.single(Z1.0),
                      regionflag=as.integer(regionflag), basinflag=as.integer(basinflag),
                      phi=as.single(0.0), tau=as.single(0.0))
  names(retvals) <- c("mag", "Rjb", "specT", "period", "lnY", "sigma", "iflag", "Vs30",
                      "ftype", "pga4nl", "Z10", "regionflag", "basinflag", "phi", "tau")
  return(retvals)
}

CB14Haz <- function(Mag, Rrup, Rjb, ftype=0, Prd, Vs30,
                    Ztor, Z2.5, Dip, depth, hwflag=0, Rx, rupwidth, regionflag){
  #subroutine CB_NGAWest2_2013 ( mag, Rrup, Rbjf, Ftype, specT, period2, lnY, sigma, iflag, vs,
  #  depthtop, D25, Dip, depth, HWflag, Rx, rupwidth, regionflag, phi, tau )

  # Model Number = 2836
  retvals <- .Fortran("CB_NGAWest2_2013", mag=as.single(Mag), Rrup=as.single(Rrup), Rbjf=as.single(Rjb),
                      ftype=as.single(ftype), specT=as.single(Prd), period2=as.single(0),
                      lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(0),
                      vs=as.single(Vs30), depthtop=as.single(Ztor), D25=as.single(Z2.5), Dip=as.single(Dip),
                      depth=as.single(depth), hwflag=as.integer(hwflag), Rx=as.single(Rx),
                      rupwidth=as.single(rupwidth), regionflag=as.integer(regionflag),
                      phi=as.single(0.0), tau=as.single(0.0))
  names(retvals) <- c("mag", "Rrup", "Rjb", "ftype", "specT", "period", "lnY", "sigma", "iflag", "Vs30",
                      "Ztor", "Z25", "dip", "depth", "hwflag", "Rx", "rupwidth", "regionflag", "phi", "tau")
  return(retvals)
}

Lin11Rock <- function(Mag, Rrup, Prd, hwflag=0){
  if(hwflag==0){
    retvals <- .Fortran("Lin_fw_rock", mag=as.single(Mag), rupdist=as.single(Rrup), specT=as.single(Prd),
                        period=as.single(0), lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(1))
  }else{
    retvals <- .Fortran("Lin_hw_rock", mag=as.single(Mag), rupdist=as.single(Rrup), specT=as.single(Prd),
                        period=as.single(0), lnY=as.single(0.1), sigma=as.single(1), iflag=as.integer(1))
  }
  names(retvals) <- c("mag", "Rrup", "specT", "period", "lnY", "sigma", "iflag")
  return(retvals)
}

Lin11Soil <- function(Mag, Rrup, Prd, hwflag=0){
  if(hwflag==0){
    retvals <- .Fortran("Lin_fw_soil", mag=as.single(Mag), rupdist=as.single(Rrup), specT=as.single(Prd),
                        period=as.single(0), lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(1))
  }else{
    retvals <- .Fortran("Lin_hw_soil", mag=as.single(Mag), rupdist=as.single(Rrup), specT=as.single(Prd),
                        period=as.single(0), lnY=as.single(0.1), sigma=as.single(1), iflag=as.integer(1))
  }
  names(retvals) <- c("mag", "Rrup", "specT", "period", "lnY", "sigma", "iflag")
  return(retvals)
}

Zh06Haz <- function(Mag, Rrup, ftype, sclass=1.0, Prd, sourceclass=0.0, depth){
  #Zhaoetal2006 ( m, dist, ftype, lnY, sigma, sclass, specT,
  #               attenName, period1, iflag, sourcetype, hypo, phi, tau )
  #     Sourcetype = 0 Crustal
  #     Sourcetype = 1 Subduction - Interface
  #     Sourcetype = 2 Subduction - Slab
  #     Sclass = 0 Hard Rock
  #     Sclass = 1 SC I
  #     Sclass = 2 SC II
  #     Sclass = 3 SC III
  #     Sclass = 4 SC IV
  #     Model Number = 256
  retvals <- .Fortran("Zhaoetal2006", mag=as.single(Mag), dist=as.single(Rrup), ftype=as.single(ftype),
                      lnY=as.single(0.1), sigma=as.single(0.1), sclass=as.single(sclass),
                      specT=as.single(Prd), attenName=as.character("attenName"), period1=as.single(0), iflag=as.integer(0),
                      sourcetype=as.single(sourceclass), hypo=as.single(depth),
                      phi=as.single(0.0), tau=as.single(0.0))
  names(retvals) <- c("mag", "Rrup", "ftype", "lnY", "sigma", "sclass", "specT", "attenName", "period", "iflag",
                      "sourcetype", "depth", "phi", "tau")
  retvals$attenName <- NULL
  return(retvals)
}

ASB13Haz <- function(Mag, Rjb, Prd, ftype, Vs30){
  #ASB_2013 ( mag, Rbjf, specT,
  #                     period2, lnY, sigma, iflag, ftype, Vs, phiT, tauT )
  retvals <- .Fortran("ASB_2013", mag=as.single(Mag), Rbjf=as.single(Rjb), specT=as.single(Prd),
                      period2=as.single(0), lnY=as.single(0.1), sigma=as.single(0.1),
                      iflag=as.integer(0), ftype=as.single(ftype), vs=as.single(Vs30),
                      phi=as.single(0.0), tau=as.single(0.0))
  names(retvals) <- c("mag", "Rjb", "specT", "period", "lnY", "sigma", "iflag", "ftype", "Vs30",
                      "phi", "tau")
  return(retvals)
}

AC10Haz <- function(Mag, Rjb, Prd, ftype, Vs30){
  #AC_2010 ( mag, Rbjf, specT,
  #                     period2, lnY, sigma, iflag, vs, ftype, pga4nl )
  retvals <- .Fortran("AC_2010", mag=as.single(Mag), Rbjf=as.single(Rjb), specT=as.single(Prd),
                      period2=as.single(0), lnY=as.single(0.1), sigma=as.single(0.1),
                      iflag=as.integer(0), vs=as.single(Vs30), ftype=as.single(ftype),
                      pga4nl=as.single(0.0))
  names(retvals) <- c("mag", "Rjb", "specT", "period", "lnY", "sigma", "iflag", "Vs30", "ftype",
                      "pga4nl")
  return(retvals)
}

BI14Haz <- function(Mag, Rjb, Prd, ftype, Vs30){
  #Bindi_Hor_2013 ( m, jbDist, ftype, specT,
  #                     period2, lnY, sigma, iflag, vs, phiT, tauT )
  retvals <- .Fortran("Bindi_Hor_2013", mag=as.single(Mag), jbDist=as.single(Rjb), ftype=as.single(ftype),
                      specT=as.single(Prd), period2=as.single(0), lnY=as.single(0.1), sigma=as.single(0.1),
                      iflag=as.integer(0), vs=as.single(Vs30), phi=as.single(0.0), tau=as.single(0.0))
  names(retvals) <- c("mag", "Rjb", "ftype", "specT", "period", "lnY", "sigma", "iflag", "Vs30",
                      "phi", "tau")
  return(retvals)
}

BCHydroSubV3 <- function(Mag, ftype=0, Rrup, Vs30, Prd, forearc=1, depth, disthypo) {
  if (ftype == 0) {
    if (Prd <= 0.3) {
      deltaC1 = 0.2
    }else if (Prd > 0.3 & Prd <= 0.5) {
      deltaC1 = 0.2 + (0.1-0.2)*(log(Prd)-log(0.3)) / (log(0.5)-log(0.3))
    }else if (Prd > 0.5 & Prd <= 1.0) {
      deltaC1 = 0.1 + (0.0-0.1)*(log(Prd)-log(0.5)) / (log(1.0)-log(0.5))
    }else if (Prd > 1.0 & Prd <= 2.0){
      deltaC1 = 0.0 + (-0.1-0.0)*(log(Prd)-log(1.0)) / (log(2.0)-log(1.0))
    }else if (Prd > 2.0 & Prd <= 3.0){
      deltaC1 = -0.1 + (-0.2+0.1)*(log(Prd)-log(2.0)) / (log(3.0)-log(2.0))
    }else{
      deltaC1 = -0.2
    }
  }else{
    deltaC1 = -0.3
  }
#subroutine BCHydroSub_V3 ( mag, fType, rRup, vs30, lnSa, sigma1, specT, period1, iflag, forearc, depth, disthypo, deltac1 )
  retvals <- .Fortran("BCHydroSub_V3", mag=as.single(Mag), ftype=as.single(ftype), rRup=as.single(Rrup),
                      vs30=as.single(Vs30), lnSa=as.single(0.1), sigma=as.single(0.1),
                      specT=as.single(Prd), period1=as.single(0), iflag=as.integer(1),
                      forearc=as.integer(forearc), depth=as.single(depth), disthypo=as.single(disthypo),
                      deltac1=as.single(deltaC1))
  names(retvals) <- c("mag", "ftype", "Rrup", "Vs30", "lnY", "sigma", "specT",  "period", "iflag",
                          "forearc", "depth", "Rhypo", "deltac1")
  return(retvals)
}

Youngs97Rock <- function(Mag, Rrup, Prd, ftype=0, depth) {
  #subroutine youngs97_rock ( mag, rupDist, lnY, sigma, attenName, period, specT, ftype, depth,iflag )
  retvals <- .Fortran("youngs97_rock", mag=as.single(Mag), rupdist=as.single(Rrup), lnY=as.single(0.1),
                      sigma=as.single(0.1), attenName=as.character("attenName"), period=as.single(0), specT=as.single(Prd),ftype=as.single(ftype),
                      depth=as.single(depth), iflag=as.integer(1))
  names(retvals) <- c("mag", "Rrup", "lnY", "sigma", "attenName", "period", "specT", "ftype", "depth", "iflag")
  retvals$attenName <- NULL
  return(retvals)
}

Youngs97Soil <- function(Mag, Rrup, Prd, ftype=0, depth) {
  #subroutine youngs97_soil ( mag, rupDist, lnY, sigma, attenName, period, specT, ftype, depth, iflag )
  retvals <- .Fortran("youngs97_soil", mag=as.single(Mag), rupdist=as.single(Rrup), lnY=as.single(0.1),
                      sigma=as.single(0.1), attenName=as.character("attenName"), period=as.single(0), specT=as.single(Prd),ftype=as.single(ftype),
                      depth=as.single(depth), iflag=as.integer(1))
  names(retvals) <- c("mag", "Rrup", "lnY", "sigma", "attenName", "period", "specT", "ftype", "depth", "iflag")
  retvals$attenName <- NULL
  return(retvals)
}

AB03Haz <- function(Mag, Rrup, Prd, ftype=0, depth, Sc=0, Sd=0, Se=0) {
#subroutine subroutine AB03 ( mag, rupdist, lnY, sigma, specT, attenName, period,iflag, ftype, depth, Sc, Sd, Se)
  #     Site Classses are as follows:
  #          NEHRP B --> Sc=0, Sd=0, Se=0   Vs>760m/s
  #          NEHRP C --> Sc=1, Sd=0, Se=0   360<Vs<760
  #          NEHRP D --> Sc=0, Sd=1, Se=0   180<Vs<360
  #          NEHRP E --> Sc=0, Sd=0, Se=1   Vs<180
  retvals <- .Fortran("AB03", mag=as.single(Mag), rupdist=as.single(Rrup), lnY=as.single(0.1),
                      sigma=as.single(0.1), specT=as.single(Prd), attenName=as.character("attenName"),
                      period=as.single(0), iflag=as.integer(1),
                      ftype=as.single(ftype), depth=as.single(depth), Sc=as.single(Sc), Sd=as.single(Sd),
                      Se=as.single(Se))
  names(retvals) <- c("mag", "Rrup", "lnY", "sigma", "specT", "attenName", "period", "iflag", "ftype", "depth",
                      "Sc", "Sd", "Se")
  retvals$attenName <- NULL
  return(retvals)
}

Kanno06 <- function(Mag, Rrup, Prd, Vs30, depth) {
  #subroutine kanno2006 (mag, Rrup, specT, period2, lnY, sigma, iflag, vs30, depth )
  retvals <- .Fortran("kanno2006", mag=as.single(Mag), Rrup=as.single(Rrup), specT=as.single(Prd),
                      period2=as.single(0), lnY=as.single(0.1), sigma=as.single(0.1),
                      iflag=as.integer(1), vs30=as.single(Vs30), depth=as.single(depth))
  names(retvals) <- c("mag", "Rrup", "specT", "period", "lnY", "sigma", "iflag", "Vs30", "depth")
  return(retvals)
}

Gregor06 <- function(Mag, Rrup, Prd, Vs30) {
  #subroutine Gregor06Cas(mag, rupdist, lnY, sigma, specT, attenName, vs30, period,iflag )
  retvals <- .Fortran("Gregor06Cas", mag=as.single(Mag), rupdist=as.single(Rrup), lnY=as.single(0.1),
                      sigma=as.single(0.1), specT=as.single(Prd), attenName=as.character("attenName"), vs30=as.single(Vs30),
                      period=as.single(0), iflag=as.integer(1))
  names(retvals) <- c("mag", "Rrup", "lnY", "sigma", "specT", "attenName", "Vs30", "period", "iflag")
  retvals$attenName <- NULL
  return(retvals)
}
# Mag, Rrup, Rjb, ftype, vs, vs30_class, specT, period2, lnY, pga4nl,
# depth, depthtop, Z10, Z25, rupwidth, Rx, Ry0, dip,
# iflag, hwflag, regionflag, basinflag, msasflag, sigma, phi, tau

# ---- !! not finished ----
GMPE <- function(Mag=6.0, Rrup=10, Rjb=10, Rseis=10, Rhypo=10,
                 jcalc=8797, period=0.00, lnY=0.0, Sigma=0.65, tau=0.0, phi=0.0,
                 Vs30=1130, Depth=10, Vs30_class=0, hwflag=0, ftype=0,
                 attenName="GMPE", period1=matrix(as.single(0), 4, 3),
                 iAtten=0, iProb=0, jType=0, intflag=matrix(as.single(0), 4, 3), AR=0, Dip=0.0,
                 Z10=30, Z15=50, Z25=100, Ztor=0, Theta_Site=0,Width=50, foreArc=0, Rx=0.0,
                 Ry=0.0, cfcoefrrup=matrix(as.single(0), 40, 11), cfcoefrjb=matrix(as.single(0), 40, 11)){
  retvals <- .Fortran("meanInten",
                      rupdist=as.single(Rrup),
                      jbdist=as.single(Rjb),
                      seismodist=as.single(Rseis),
                      hwflag=as.integer(hwflag),
                      mag=as.single(Mag),
                      jcalc=as.integer(jcalc),
                      specT=as.single(period),
                      lnY=as.single(0.1),
                      siga=as.single(Sigma),
                      ftype=as.single(ftype),
                      attenName=attenName,
                      period1=period1,
                      iAtten=as.integer(iAtten),
                      iProb=as.integer(iProb),
                      jType=as.integer(jType),
                      vs=as.single(Vs30),
                      depth=as.single(Depth),
                      intflag=intflag,
                      AR=as.single(AR),
                      dip1=as.single(Dip),
                      disthypo=as.single(Rhypo),
                      depthvs10=as.single(Z10),
                      depthvs15=as.single(Z15),
                      D25=as.single(Z25),
                      tau=as.single(tau),
                      depthTop=as.single(Ztor),
                      Theta_Site=as.single(Theta_Site),
                      RupWidth=as.single(Width),
                      vs30_class=as.single(Vs30_class),
                      foreArc=as.integer(foreArc),
                      Rx=as.single(Rx),
                      phi=as.single(phi),
                      cfcoefrrup=cfcoefrrup,
                      cfcoefrjb=cfcoefrjb,
                      Ry0=as.single(Ry))
  return(retvals)
}

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

