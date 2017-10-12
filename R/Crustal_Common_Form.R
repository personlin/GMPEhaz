#' GMPE function for Crustal Common form 001 (2017)
#'
#' \code{Cru.Com.001} returns the ground-motion prediction with it sigma of Crustal Common form 001 GMPE.
#'
#'Crustal Common form 001
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Rjb Joyner and Boore distance(km), Numeric.
#' @param Ztor Depth to the top of the finite rupture model (km).
#' @param ftype style of faulting.
#' @param Dip Dip angle of the fault plane.
#' @param rupwidth Down-dip rupture width (km).
#' @param Rx Horizontal distance(km) from top edge of rupture. Measured perpendicular to the fault strike.
#' @param hwflag hanging-wall flag, 1 for hanging-wall.
#' @param Prd Period of spectral acceleration.
#'
#' @return A list will be return, including mag, Rrup, Rjb, Ztor, ftype, dip, rupwidth, Rx,
#'  hwflag, pecT, lnY, sigma, iflag.
#'
#' @examples
#' Cru.Com.001(6, 20, 20, 5, 0, 90, 20, 20, 0, 0)
#' Cru.Com.001(7, 20, 20, 2, 0, 90, 20, 20, 0, 0 )
#'
#' @export
Cru.Com.001 <- function(Mag, Rrup, Rjb, Ztor, ftype=0, Dip, rupwidth, Rx, hwflag=0, Prd){
  # Subroutine S04_Crustal_Common001 ( m, Rrup, Rjb, ztor, ftype, dip, Width, Rx, HWFlag,
  #                                    specT, lnY, sigma, iflag )
  if (Prd != 0 & (Prd < 0.01 | Prd > 2)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("S04_Crustal_Common001", m=as.single(Mag), Rrup=as.single(Rrup), Rjb=as.single(Rjb),
                      ztor=as.single(Ztor), ftype=as.single(ftype), dip=as.single(Dip), Width=as.single(rupwidth),
                      Rx=as.single(Rx), HWFlag=as.integer(hwflag), specT=as.single(Prd),
                      lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(0))

  names(retvals) <- c("mag", "Rrup", "Rjb", "Ztor", "ftype", "dip", "rupwidth", "Rx", "hwflag",
                      "specT", "lnY", "sigma", "iflag")

  return(retvals)
}



#' GMPE function for Crustal Common form 002 (2017)
#'
#' \code{Cru.Com.002} returns the ground-motion prediction with it sigma of Crustal Common form 002 GMPE.
#'
#'Crustal Common form 002
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Rjb Joyner and Boore distance(km), Numeric.
#' @param Ztor Depth to the top of the finite rupture model (km).
#' @param ftype style of faulting.
#' @param Dip Dip angle of the fault plane.
#' @param rupwidth Down-dip rupture width (km).
#' @param Rx Horizontal distance(km) from top edge of rupture. Measured perpendicular to the fault strike.
#' @param hwflag hanging-wall flag, 1 for hanging-wall.
#' @param Prd Period of spectral acceleration.
#'
#' @return A list will be return, including mag, Rrup, Rjb, Ztor, ftype, dip, rupwidth, Rx,
#'  hwflag, pecT, lnY, sigma, iflag.
#'
#' @examples
#' Cru.Com.002(6, 20, 20, 5, 0, 90, 20, 20, 0, 0)
#' Cru.Com.002(7, 20, 20, 2, 0, 90, 20, 20, 0, 0 )
#'
#' @export
Cru.Com.002 <- function(Mag, Rrup, Rjb, Ztor, ftype=0, Dip, rupwidth, Rx, hwflag=0, Prd){
  # Subroutine S04_Crustal_Common002 ( m, Rrup, Rjb, ztor, ftype, dip, Width, Rx, HWFlag,
  #                                    specT, lnY, sigma, iflag )
  if (Prd != 0 & (Prd < 0.01 | Prd > 2)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("S04_Crustal_Common002", m=as.single(Mag), Rrup=as.single(Rrup), Rjb=as.single(Rjb),
                      ztor=as.single(Ztor), ftype=as.single(ftype), dip=as.single(Dip), Width=as.single(rupwidth),
                      Rx=as.single(Rx), HWFlag=as.integer(hwflag), specT=as.single(Prd),
                      lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(0))

  names(retvals) <- c("mag", "Rrup", "Rjb", "Ztor", "ftype", "dip", "rupwidth", "Rx", "hwflag",
                      "specT", "lnY", "sigma", "iflag")

  return(retvals)
}

#' GMPE function for Crustal Common form 003 (2017)
#'
#' \code{Cru.Com.003} returns the ground-motion prediction with it sigma of Crustal Common form 003 GMPE.
#'
#'Crustal Common form 003
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Rjb Joyner and Boore distance(km), Numeric.
#' @param Ztor Depth to the top of the finite rupture model (km).
#' @param ftype style of faulting.
#' @param Dip Dip angle of the fault plane.
#' @param rupwidth Down-dip rupture width (km).
#' @param Rx Horizontal distance(km) from top edge of rupture. Measured perpendicular to the fault strike.
#' @param hwflag hanging-wall flag, 1 for hanging-wall.
#' @param Prd Period of spectral acceleration.
#'
#' @return A list will be return, including mag, Rrup, Rjb, Ztor, ftype, dip, rupwidth, Rx,
#'  hwflag, pecT, lnY, sigma, iflag.
#'
#' @examples
#' Cru.Com.003(6, 20, 20, 5, 0, 90, 20, 20, 0, 0)
#' Cru.Com.003(7, 20, 20, 2, 0, 90, 20, 20, 0, 0 )
#'
#' @export
Cru.Com.003 <- function(Mag, Rrup, Rjb, Ztor, ftype=0, Dip, rupwidth, Rx, hwflag=0, Prd){
  # Subroutine S04_Crustal_Common003 ( m, Rrup, Rjb, ztor, ftype, dip, Width, Rx, HWFlag,
  #                                    specT, lnY, sigma, iflag )
  if (Prd != 0 & (Prd < 0.01 | Prd > 2)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("S04_Crustal_Common003", m=as.single(Mag), Rrup=as.single(Rrup), Rjb=as.single(Rjb),
                      ztor=as.single(Ztor), ftype=as.single(ftype), dip=as.single(Dip), Width=as.single(rupwidth),
                      Rx=as.single(Rx), HWFlag=as.integer(hwflag), specT=as.single(Prd),
                      lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(0))

  names(retvals) <- c("mag", "Rrup", "Rjb", "Ztor", "ftype", "dip", "rupwidth", "Rx", "hwflag",
                      "specT", "lnY", "sigma", "iflag")

  return(retvals)
}


#' GMPE function for Crustal Common form 004 (2017)
#'
#' \code{Cru.Com.004} returns the ground-motion prediction with it sigma of Crustal Common form 004 GMPE.
#'
#'Crustal Common form 004
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Rjb Joyner and Boore distance(km), Numeric.
#' @param Ztor Depth to the top of the finite rupture model (km).
#' @param ftype style of faulting.
#' @param Dip Dip angle of the fault plane.
#' @param rupwidth Down-dip rupture width (km).
#' @param Rx Horizontal distance(km) from top edge of rupture. Measured perpendicular to the fault strike.
#' @param hwflag hanging-wall flag, 1 for hanging-wall.
#' @param Prd Period of spectral acceleration.
#'
#' @return A list will be return, including mag, Rrup, Rjb, Ztor, ftype, dip, rupwidth, Rx,
#'  hwflag, pecT, lnY, sigma, iflag.
#'
#' @examples
#' Cru.Com.004(6, 20, 20, 5, 0, 90, 20, 20, 0, 0)
#' Cru.Com.004(7, 20, 20, 2, 0, 90, 20, 20, 0, 0 )
#'
#' @export
Cru.Com.004 <- function(Mag, Rrup, Rjb, Ztor, ftype=0, Dip, rupwidth, Rx, hwflag=0, Prd){
  # Subroutine S04_Crustal_Common004 ( m, Rrup, Rjb, ztor, ftype, dip, Width, Rx, HWFlag,
  #                                    specT, lnY, sigma, iflag )
  if (Prd != 0 & (Prd < 0.01 | Prd > 2)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("S04_Crustal_Common004", m=as.single(Mag), Rrup=as.single(Rrup), Rjb=as.single(Rjb),
                      ztor=as.single(Ztor), ftype=as.single(ftype), dip=as.single(Dip), Width=as.single(rupwidth),
                      Rx=as.single(Rx), HWFlag=as.integer(hwflag), specT=as.single(Prd),
                      lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(0))

  names(retvals) <- c("mag", "Rrup", "Rjb", "Ztor", "ftype", "dip", "rupwidth", "Rx", "hwflag",
                      "specT", "lnY", "sigma", "iflag")

  return(retvals)
}


#' GMPE function for Crustal Common form 005 (2017)
#'
#' \code{Cru.Com.005} returns the ground-motion prediction with it sigma of Crustal Common form 005 GMPE.
#'
#'Crustal Common form 005
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Rjb Joyner and Boore distance(km), Numeric.
#' @param Ztor Depth to the top of the finite rupture model (km).
#' @param ftype style of faulting.
#' @param Dip Dip angle of the fault plane.
#' @param rupwidth Down-dip rupture width (km).
#' @param Rx Horizontal distance(km) from top edge of rupture. Measured perpendicular to the fault strike.
#' @param hwflag hanging-wall flag, 1 for hanging-wall.
#' @param Prd Period of spectral acceleration.
#'
#' @return A list will be return, including mag, Rrup, Rjb, Ztor, ftype, dip, rupwidth, Rx,
#'  hwflag, pecT, lnY, sigma, iflag.
#'
#' @examples
#' Cru.Com.005(6, 20, 20, 5, 0, 90, 20, 20, 0, 0)
#' Cru.Com.005(7, 20, 20, 2, 0, 90, 20, 20, 0, 0 )
#'
#' @export
Cru.Com.005 <- function(Mag, Rrup, Rjb, Ztor, ftype=0, Dip, rupwidth, Rx, hwflag=0, Prd){
  # Subroutine S04_Crustal_Common005 ( m, Rrup, Rjb, ztor, ftype, dip, Width, Rx, HWFlag,
  #                                    specT, lnY, sigma, iflag )
  if (Prd != 0 & (Prd < 0.01 | Prd > 2)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("S04_Crustal_Common005", m=as.single(Mag), Rrup=as.single(Rrup), Rjb=as.single(Rjb),
                      ztor=as.single(Ztor), ftype=as.single(ftype), dip=as.single(Dip), Width=as.single(rupwidth),
                      Rx=as.single(Rx), HWFlag=as.integer(hwflag), specT=as.single(Prd),
                      lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(0))

  names(retvals) <- c("mag", "Rrup", "Rjb", "Ztor", "ftype", "dip", "rupwidth", "Rx", "hwflag",
                      "specT", "lnY", "sigma", "iflag")

  return(retvals)
}


#' GMPE function for Crustal Common form 006 (2017)
#'
#' \code{Cru.Com.006} returns the ground-motion prediction with it sigma of Crustal Common form 006 GMPE.
#'
#'Crustal Common form 006
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Rjb Joyner and Boore distance(km), Numeric.
#' @param Ztor Depth to the top of the finite rupture model (km).
#' @param ftype style of faulting.
#' @param Dip Dip angle of the fault plane.
#' @param rupwidth Down-dip rupture width (km).
#' @param Rx Horizontal distance(km) from top edge of rupture. Measured perpendicular to the fault strike.
#' @param hwflag hanging-wall flag, 1 for hanging-wall.
#' @param Prd Period of spectral acceleration.
#'
#' @return A list will be return, including mag, Rrup, Rjb, Ztor, ftype, dip, rupwidth, Rx,
#'  hwflag, pecT, lnY, sigma, iflag.
#'
#' @examples
#' Cru.Com.006(6, 20, 20, 5, 0, 90, 20, 20, 0, 0)
#' Cru.Com.006(7, 20, 20, 2, 0, 90, 20, 20, 0, 0 )
#'
#' @export
Cru.Com.006 <- function(Mag, Rrup, Rjb, Ztor, ftype=0, Dip, rupwidth, Rx, hwflag=0, Prd){
  # Subroutine S04_Crustal_Common006 ( m, Rrup, Rjb, ztor, ftype, dip, Width, Rx, HWFlag,
  #                                    specT, lnY, sigma, iflag )
  if (Prd != 0 & (Prd < 0.01 | Prd > 2)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("S04_Crustal_Common006", m=as.single(Mag), Rrup=as.single(Rrup), Rjb=as.single(Rjb),
                      ztor=as.single(Ztor), ftype=as.single(ftype), dip=as.single(Dip), Width=as.single(rupwidth),
                      Rx=as.single(Rx), HWFlag=as.integer(hwflag), specT=as.single(Prd),
                      lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(0))

  names(retvals) <- c("mag", "Rrup", "Rjb", "Ztor", "ftype", "dip", "rupwidth", "Rx", "hwflag",
                      "specT", "lnY", "sigma", "iflag")

  return(retvals)
}

#' GMPE function for Crustal Common form 007 (2017)
#'
#' \code{Cru.Com.007} returns the ground-motion prediction with it sigma of Crustal Common form 007 GMPE.
#'
#'Crustal Common form 007
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Rjb Joyner and Boore distance(km), Numeric.
#' @param Ztor Depth to the top of the finite rupture model (km).
#' @param ftype style of faulting.
#' @param Dip Dip angle of the fault plane.
#' @param rupwidth Down-dip rupture width (km).
#' @param Rx Horizontal distance(km) from top edge of rupture. Measured perpendicular to the fault strike.
#' @param hwflag hanging-wall flag, 1 for hanging-wall.
#' @param Prd Period of spectral acceleration.
#'
#' @return A list will be return, including mag, Rrup, Rjb, Ztor, ftype, dip, rupwidth, Rx,
#'  hwflag, pecT, lnY, sigma, iflag.
#'
#' @examples
#' Cru.Com.007(6, 20, 20, 5, 0, 90, 20, 20, 0, 0)
#' Cru.Com.007(7, 20, 20, 2, 0, 90, 20, 20, 0, 0 )
#'
#' @export
Cru.Com.007 <- function(Mag, Rrup, Rjb, Ztor, ftype=0, Dip, rupwidth, Rx, hwflag=0, Prd){
  # Subroutine S04_Crustal_Common007 ( m, Rrup, Rjb, ztor, ftype, dip, Width, Rx, HWFlag,
  #                                    specT, lnY, sigma, iflag )
  if (Prd != 0 & (Prd < 0.01 | Prd > 2)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("S04_Crustal_Common007", m=as.single(Mag), Rrup=as.single(Rrup), Rjb=as.single(Rjb),
                      ztor=as.single(Ztor), ftype=as.single(ftype), dip=as.single(Dip), Width=as.single(rupwidth),
                      Rx=as.single(Rx), HWFlag=as.integer(hwflag), specT=as.single(Prd),
                      lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(0))

  names(retvals) <- c("mag", "Rrup", "Rjb", "Ztor", "ftype", "dip", "rupwidth", "Rx", "hwflag",
                      "specT", "lnY", "sigma", "iflag")

  return(retvals)
}


#' GMPE function for Crustal Common form 008 (2017)
#'
#' \code{Cru.Com.008} returns the ground-motion prediction with it sigma of Crustal Common form 008 GMPE.
#'
#'Crustal Common form 008
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Rjb Joyner and Boore distance(km), Numeric.
#' @param Ztor Depth to the top of the finite rupture model (km).
#' @param ftype style of faulting.
#' @param Dip Dip angle of the fault plane.
#' @param rupwidth Down-dip rupture width (km).
#' @param Rx Horizontal distance(km) from top edge of rupture. Measured perpendicular to the fault strike.
#' @param hwflag hanging-wall flag, 1 for hanging-wall.
#' @param Prd Period of spectral acceleration.
#'
#' @return A list will be return, including mag, Rrup, Rjb, Ztor, ftype, dip, rupwidth, Rx,
#'  hwflag, pecT, lnY, sigma, iflag.
#'
#' @examples
#' Cru.Com.008(6, 20, 20, 5, 0, 90, 20, 20, 0, 0)
#' Cru.Com.008(7, 20, 20, 2, 0, 90, 20, 20, 0, 0 )
#'
#' @export
Cru.Com.008 <- function(Mag, Rrup, Rjb, Ztor, ftype=0, Dip, rupwidth, Rx, hwflag=0, Prd){
  # Subroutine S04_Crustal_Common008 ( m, Rrup, Rjb, ztor, ftype, dip, Width, Rx, HWFlag,
  #                                    specT, lnY, sigma, iflag )
  if (Prd != 0 & (Prd < 0.01 | Prd > 2)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("S04_Crustal_Common008", m=as.single(Mag), Rrup=as.single(Rrup), Rjb=as.single(Rjb),
                      ztor=as.single(Ztor), ftype=as.single(ftype), dip=as.single(Dip), Width=as.single(rupwidth),
                      Rx=as.single(Rx), HWFlag=as.integer(hwflag), specT=as.single(Prd),
                      lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(0))

  names(retvals) <- c("mag", "Rrup", "Rjb", "Ztor", "ftype", "dip", "rupwidth", "Rx", "hwflag",
                      "specT", "lnY", "sigma", "iflag")

  return(retvals)
}


#' GMPE function for Crustal Common form 009 (2017)
#'
#' \code{Cru.Com.009} returns the ground-motion prediction with it sigma of Crustal Common form 009 GMPE.
#'
#'Crustal Common form 009
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Rjb Joyner and Boore distance(km), Numeric.
#' @param Ztor Depth to the top of the finite rupture model (km).
#' @param ftype style of faulting.
#' @param Dip Dip angle of the fault plane.
#' @param rupwidth Down-dip rupture width (km).
#' @param Rx Horizontal distance(km) from top edge of rupture. Measured perpendicular to the fault strike.
#' @param hwflag hanging-wall flag, 1 for hanging-wall.
#' @param Prd Period of spectral acceleration.
#'
#' @return A list will be return, including mag, Rrup, Rjb, Ztor, ftype, dip, rupwidth, Rx,
#'  hwflag, pecT, lnY, sigma, iflag.
#'
#' @examples
#' Cru.Com.009(6, 20, 20, 5, 0, 90, 20, 20, 0, 0)
#' Cru.Com.009(7, 20, 20, 2, 0, 90, 20, 20, 0, 0 )
#'
#' @export
Cru.Com.009 <- function(Mag, Rrup, Rjb, Ztor, ftype=0, Dip, rupwidth, Rx, hwflag=0, Prd){
  # Subroutine S04_Crustal_Common009 ( m, Rrup, Rjb, ztor, ftype, dip, Width, Rx, HWFlag,
  #                                    specT, lnY, sigma, iflag )
  if (Prd != 0 & (Prd < 0.01 | Prd > 2)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("S04_Crustal_Common009", m=as.single(Mag), Rrup=as.single(Rrup), Rjb=as.single(Rjb),
                      ztor=as.single(Ztor), ftype=as.single(ftype), dip=as.single(Dip), Width=as.single(rupwidth),
                      Rx=as.single(Rx), HWFlag=as.integer(hwflag), specT=as.single(Prd),
                      lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(0))

  names(retvals) <- c("mag", "Rrup", "Rjb", "Ztor", "ftype", "dip", "rupwidth", "Rx", "hwflag",
                      "specT", "lnY", "sigma", "iflag")

  return(retvals)
}


#' GMPE function for Crustal Common form 010 (2017)
#'
#' \code{Cru.Com.010} returns the ground-motion prediction with it sigma of Crustal Common form 010 GMPE.
#'
#'Crustal Common form 010
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Rjb Joyner and Boore distance(km), Numeric.
#' @param Ztor Depth to the top of the finite rupture model (km).
#' @param ftype style of faulting.
#' @param Dip Dip angle of the fault plane.
#' @param rupwidth Down-dip rupture width (km).
#' @param Rx Horizontal distance(km) from top edge of rupture. Measured perpendicular to the fault strike.
#' @param hwflag hanging-wall flag, 1 for hanging-wall.
#' @param Prd Period of spectral acceleration.
#'
#' @return A list will be return, including mag, Rrup, Rjb, Ztor, ftype, dip, rupwidth, Rx,
#'  hwflag, pecT, lnY, sigma, iflag.
#'
#' @examples
#' Cru.Com.010(6, 20, 20, 5, 0, 90, 20, 20, 0, 0)
#' Cru.Com.010(7, 20, 20, 2, 0, 90, 20, 20, 0, 0 )
#'
#' @export
Cru.Com.010 <- function(Mag, Rrup, Rjb, Ztor, ftype=0, Dip, rupwidth, Rx, hwflag=0, Prd){
  # Subroutine S04_Crustal_Common010 ( m, Rrup, Rjb, ztor, ftype, dip, Width, Rx, HWFlag,
  #                                    specT, lnY, sigma, iflag )
  if (Prd != 0 & (Prd < 0.01 | Prd > 2)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("S04_Crustal_Common010", m=as.single(Mag), Rrup=as.single(Rrup), Rjb=as.single(Rjb),
                      ztor=as.single(Ztor), ftype=as.single(ftype), dip=as.single(Dip), Width=as.single(rupwidth),
                      Rx=as.single(Rx), HWFlag=as.integer(hwflag), specT=as.single(Prd),
                      lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(0))

  names(retvals) <- c("mag", "Rrup", "Rjb", "Ztor", "ftype", "dip", "rupwidth", "Rx", "hwflag",
                      "specT", "lnY", "sigma", "iflag")

  return(retvals)
}


#' GMPE function for Crustal Common form 011 (2017)
#'
#' \code{Cru.Com.011} returns the ground-motion prediction with it sigma of Crustal Common form 011 GMPE.
#'
#'Crustal Common form 011
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Rjb Joyner and Boore distance(km), Numeric.
#' @param Ztor Depth to the top of the finite rupture model (km).
#' @param ftype style of faulting.
#' @param Dip Dip angle of the fault plane.
#' @param rupwidth Down-dip rupture width (km).
#' @param Rx Horizontal distance(km) from top edge of rupture. Measured perpendicular to the fault strike.
#' @param hwflag hanging-wall flag, 1 for hanging-wall.
#' @param Prd Period of spectral acceleration.
#'
#' @return A list will be return, including mag, Rrup, Rjb, Ztor, ftype, dip, rupwidth, Rx,
#'  hwflag, pecT, lnY, sigma, iflag.
#'
#' @examples
#' Cru.Com.011(6, 20, 20, 5, 0, 90, 20, 20, 0, 0)
#' Cru.Com.011(7, 20, 20, 2, 0, 90, 20, 20, 0, 0 )
#'
#' @export
Cru.Com.011 <- function(Mag, Rrup, Rjb, Ztor, ftype=0, Dip, rupwidth, Rx, hwflag=0, Prd){
  # Subroutine S04_Crustal_Common011 ( m, Rrup, Rjb, ztor, ftype, dip, Width, Rx, HWFlag,
  #                                    specT, lnY, sigma, iflag )
  if (Prd != 0 & (Prd < 0.01 | Prd > 2)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("S04_Crustal_Common011", m=as.single(Mag), Rrup=as.single(Rrup), Rjb=as.single(Rjb),
                      ztor=as.single(Ztor), ftype=as.single(ftype), dip=as.single(Dip), Width=as.single(rupwidth),
                      Rx=as.single(Rx), HWFlag=as.integer(hwflag), specT=as.single(Prd),
                      lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(0))

  names(retvals) <- c("mag", "Rrup", "Rjb", "Ztor", "ftype", "dip", "rupwidth", "Rx", "hwflag",
                      "specT", "lnY", "sigma", "iflag")

  return(retvals)
}


#' GMPE function for Crustal Common form 012 (2017)
#'
#' \code{Cru.Com.012} returns the ground-motion prediction with it sigma of Crustal Common form 012 GMPE.
#'
#'Crustal Common form 012
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Rjb Joyner and Boore distance(km), Numeric.
#' @param Ztor Depth to the top of the finite rupture model (km).
#' @param ftype style of faulting.
#' @param Dip Dip angle of the fault plane.
#' @param rupwidth Down-dip rupture width (km).
#' @param Rx Horizontal distance(km) from top edge of rupture. Measured perpendicular to the fault strike.
#' @param hwflag hanging-wall flag, 1 for hanging-wall.
#' @param Prd Period of spectral acceleration.
#'
#' @return A list will be return, including mag, Rrup, Rjb, Ztor, ftype, dip, rupwidth, Rx,
#'  hwflag, pecT, lnY, sigma, iflag.
#'
#' @examples
#' Cru.Com.012(6, 20, 20, 5, 0, 90, 20, 20, 0, 0)
#' Cru.Com.012(7, 20, 20, 2, 0, 90, 20, 20, 0, 0 )
#'
#' @export
Cru.Com.012 <- function(Mag, Rrup, Rjb, Ztor, ftype=0, Dip, rupwidth, Rx, hwflag=0, Prd){
  # Subroutine S04_Crustal_Common012 ( m, Rrup, Rjb, ztor, ftype, dip, Width, Rx, HWFlag,
  #                                    specT, lnY, sigma, iflag )
  if (Prd != 0 & (Prd < 0.01 | Prd > 2)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("S04_Crustal_Common012", m=as.single(Mag), Rrup=as.single(Rrup), Rjb=as.single(Rjb),
                      ztor=as.single(Ztor), ftype=as.single(ftype), dip=as.single(Dip), Width=as.single(rupwidth),
                      Rx=as.single(Rx), HWFlag=as.integer(hwflag), specT=as.single(Prd),
                      lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(0))

  names(retvals) <- c("mag", "Rrup", "Rjb", "Ztor", "ftype", "dip", "rupwidth", "Rx", "hwflag",
                      "specT", "lnY", "sigma", "iflag")

  return(retvals)
}


#' GMPE function for Crustal Common form 013 (2017)
#'
#' \code{Cru.Com.013} returns the ground-motion prediction with it sigma of Crustal Common form 013 GMPE.
#'
#'Crustal Common form 013
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Rjb Joyner and Boore distance(km), Numeric.
#' @param Ztor Depth to the top of the finite rupture model (km).
#' @param ftype style of faulting.
#' @param Dip Dip angle of the fault plane.
#' @param rupwidth Down-dip rupture width (km).
#' @param Rx Horizontal distance(km) from top edge of rupture. Measured perpendicular to the fault strike.
#' @param hwflag hanging-wall flag, 1 for hanging-wall.
#' @param Prd Period of spectral acceleration.
#'
#' @return A list will be return, including mag, Rrup, Rjb, Ztor, ftype, dip, rupwidth, Rx,
#'  hwflag, pecT, lnY, sigma, iflag.
#'
#' @examples
#' Cru.Com.013(6, 20, 20, 5, 0, 90, 20, 20, 0, 0)
#' Cru.Com.013(7, 20, 20, 2, 0, 90, 20, 20, 0, 0 )
#'
#' @export
Cru.Com.013 <- function(Mag, Rrup, Rjb, Ztor, ftype=0, Dip, rupwidth, Rx, hwflag=0, Prd){
  # Subroutine S04_Crustal_Common013 ( m, Rrup, Rjb, ztor, ftype, dip, Width, Rx, HWFlag,
  #                                    specT, lnY, sigma, iflag )
  if (Prd != 0 & (Prd < 0.01 | Prd > 2)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("S04_Crustal_Common013", m=as.single(Mag), Rrup=as.single(Rrup), Rjb=as.single(Rjb),
                      ztor=as.single(Ztor), ftype=as.single(ftype), dip=as.single(Dip), Width=as.single(rupwidth),
                      Rx=as.single(Rx), HWFlag=as.integer(hwflag), specT=as.single(Prd),
                      lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(0))

  names(retvals) <- c("mag", "Rrup", "Rjb", "Ztor", "ftype", "dip", "rupwidth", "Rx", "hwflag",
                      "specT", "lnY", "sigma", "iflag")

  return(retvals)
}


#' GMPE function for Crustal Common form 014 (2017)
#'
#' \code{Cru.Com.014} returns the ground-motion prediction with it sigma of Crustal Common form 014 GMPE.
#'
#'Crustal Common form 014
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Rjb Joyner and Boore distance(km), Numeric.
#' @param Ztor Depth to the top of the finite rupture model (km).
#' @param ftype style of faulting.
#' @param Dip Dip angle of the fault plane.
#' @param rupwidth Down-dip rupture width (km).
#' @param Rx Horizontal distance(km) from top edge of rupture. Measured perpendicular to the fault strike.
#' @param hwflag hanging-wall flag, 1 for hanging-wall.
#' @param Prd Period of spectral acceleration.
#'
#' @return A list will be return, including mag, Rrup, Rjb, Ztor, ftype, dip, rupwidth, Rx,
#'  hwflag, pecT, lnY, sigma, iflag.
#'
#' @examples
#' Cru.Com.014(6, 20, 20, 5, 0, 90, 20, 20, 0, 0)
#' Cru.Com.014(7, 20, 20, 2, 0, 90, 20, 20, 0, 0 )
#'
#' @export
Cru.Com.014 <- function(Mag, Rrup, Rjb, Ztor, ftype=0, Dip, rupwidth, Rx, hwflag=0, Prd){
  # Subroutine S04_Crustal_Common014 ( m, Rrup, Rjb, ztor, ftype, dip, Width, Rx, HWFlag,
  #                                    specT, lnY, sigma, iflag )
  if (Prd != 0 & (Prd < 0.01 | Prd > 2)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("S04_Crustal_Common014", m=as.single(Mag), Rrup=as.single(Rrup), Rjb=as.single(Rjb),
                      ztor=as.single(Ztor), ftype=as.single(ftype), dip=as.single(Dip), Width=as.single(rupwidth),
                      Rx=as.single(Rx), HWFlag=as.integer(hwflag), specT=as.single(Prd),
                      lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(0))

  names(retvals) <- c("mag", "Rrup", "Rjb", "Ztor", "ftype", "dip", "rupwidth", "Rx", "hwflag",
                      "specT", "lnY", "sigma", "iflag")

  return(retvals)
}


#' GMPE function for Crustal Common form 015 (2017)
#'
#' \code{Cru.Com.015} returns the ground-motion prediction with it sigma of Crustal Common form 015 GMPE.
#'
#'Crustal Common form 015
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Rjb Joyner and Boore distance(km), Numeric.
#' @param Ztor Depth to the top of the finite rupture model (km).
#' @param ftype style of faulting.
#' @param Dip Dip angle of the fault plane.
#' @param rupwidth Down-dip rupture width (km).
#' @param Rx Horizontal distance(km) from top edge of rupture. Measured perpendicular to the fault strike.
#' @param hwflag hanging-wall flag, 1 for hanging-wall.
#' @param Prd Period of spectral acceleration.
#'
#' @return A list will be return, including mag, Rrup, Rjb, Ztor, ftype, dip, rupwidth, Rx,
#'  hwflag, pecT, lnY, sigma, iflag.
#'
#' @examples
#' Cru.Com.015(6, 20, 20, 5, 0, 90, 20, 20, 0, 0)
#' Cru.Com.015(7, 20, 20, 2, 0, 90, 20, 20, 0, 0 )
#'
#' @export
Cru.Com.015 <- function(Mag, Rrup, Rjb, Ztor, ftype=0, Dip, rupwidth, Rx, hwflag=0, Prd){
  # Subroutine S04_Crustal_Common015 ( m, Rrup, Rjb, ztor, ftype, dip, Width, Rx, HWFlag,
  #                                    specT, lnY, sigma, iflag )
  if (Prd != 0 & (Prd < 0.01 | Prd > 2)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("S04_Crustal_Common015", m=as.single(Mag), Rrup=as.single(Rrup), Rjb=as.single(Rjb),
                      ztor=as.single(Ztor), ftype=as.single(ftype), dip=as.single(Dip), Width=as.single(rupwidth),
                      Rx=as.single(Rx), HWFlag=as.integer(hwflag), specT=as.single(Prd),
                      lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(0))

  names(retvals) <- c("mag", "Rrup", "Rjb", "Ztor", "ftype", "dip", "rupwidth", "Rx", "hwflag",
                      "specT", "lnY", "sigma", "iflag")

  return(retvals)
}


#' GMPE function for Crustal Common form 016 (2017)
#'
#' \code{Cru.Com.016} returns the ground-motion prediction with it sigma of Crustal Common form 016 GMPE.
#'
#'Crustal Common form 016
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Rjb Joyner and Boore distance(km), Numeric.
#' @param Ztor Depth to the top of the finite rupture model (km).
#' @param ftype style of faulting.
#' @param Dip Dip angle of the fault plane.
#' @param rupwidth Down-dip rupture width (km).
#' @param Rx Horizontal distance(km) from top edge of rupture. Measured perpendicular to the fault strike.
#' @param hwflag hanging-wall flag, 1 for hanging-wall.
#' @param Prd Period of spectral acceleration.
#'
#' @return A list will be return, including mag, Rrup, Rjb, Ztor, ftype, dip, rupwidth, Rx,
#'  hwflag, pecT, lnY, sigma, iflag.
#'
#' @examples
#' Cru.Com.016(6, 20, 20, 5, 0, 90, 20, 20, 0, 0)
#' Cru.Com.016(7, 20, 20, 2, 0, 90, 20, 20, 0, 0 )
#'
#' @export
Cru.Com.016 <- function(Mag, Rrup, Rjb, Ztor, ftype=0, Dip, rupwidth, Rx, hwflag=0, Prd){
  # Subroutine S04_Crustal_Common016 ( m, Rrup, Rjb, ztor, ftype, dip, Width, Rx, HWFlag,
  #                                    specT, lnY, sigma, iflag )
  if (Prd != 0 & (Prd < 0.01 | Prd > 2)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("S04_Crustal_Common016", m=as.single(Mag), Rrup=as.single(Rrup), Rjb=as.single(Rjb),
                      ztor=as.single(Ztor), ftype=as.single(ftype), dip=as.single(Dip), Width=as.single(rupwidth),
                      Rx=as.single(Rx), HWFlag=as.integer(hwflag), specT=as.single(Prd),
                      lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(0))

  names(retvals) <- c("mag", "Rrup", "Rjb", "Ztor", "ftype", "dip", "rupwidth", "Rx", "hwflag",
                      "specT", "lnY", "sigma", "iflag")

  return(retvals)
}

#' GMPE function for Crustal Common form 017 (2017)
#'
#' \code{Cru.Com.017} returns the ground-motion prediction with it sigma of Crustal Common form 017 GMPE.
#'
#'Crustal Common form 017
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Rjb Joyner and Boore distance(km), Numeric.
#' @param Ztor Depth to the top of the finite rupture model (km).
#' @param ftype style of faulting.
#' @param Dip Dip angle of the fault plane.
#' @param rupwidth Down-dip rupture width (km).
#' @param Rx Horizontal distance(km) from top edge of rupture. Measured perpendicular to the fault strike.
#' @param hwflag hanging-wall flag, 1 for hanging-wall.
#' @param Prd Period of spectral acceleration.
#'
#' @return A list will be return, including mag, Rrup, Rjb, Ztor, ftype, dip, rupwidth, Rx,
#'  hwflag, pecT, lnY, sigma, iflag.
#'
#' @examples
#' Cru.Com.017(6, 20, 20, 5, 0, 90, 20, 20, 0, 0)
#' Cru.Com.017(7, 20, 20, 2, 0, 90, 20, 20, 0, 0 )
#'
#' @export
Cru.Com.017 <- function(Mag, Rrup, Rjb, Ztor, ftype=0, Dip, rupwidth, Rx, hwflag=0, Prd){
  # Subroutine S04_Crustal_Common017 ( m, Rrup, Rjb, ztor, ftype, dip, Width, Rx, HWFlag,
  #                                    specT, lnY, sigma, iflag )
  if (Prd != 0 & (Prd < 0.01 | Prd > 2)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("S04_Crustal_Common017", m=as.single(Mag), Rrup=as.single(Rrup), Rjb=as.single(Rjb),
                      ztor=as.single(Ztor), ftype=as.single(ftype), dip=as.single(Dip), Width=as.single(rupwidth),
                      Rx=as.single(Rx), HWFlag=as.integer(hwflag), specT=as.single(Prd),
                      lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(0))

  names(retvals) <- c("mag", "Rrup", "Rjb", "Ztor", "ftype", "dip", "rupwidth", "Rx", "hwflag",
                      "specT", "lnY", "sigma", "iflag")

  return(retvals)
}
