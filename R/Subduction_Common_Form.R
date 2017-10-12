#' GMPE function for Subduction intraslab common form 001 (2017)
#'
#' \code{Sub.Intra.Com.001} returns the ground-motion prediction with it sigma of Subduction intraslab Common form 001 GMPE.
#'
#'Subduction intraslab common form 001
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Ztor Depth to the top of the finite rupture model (km).
#' @param Prd Period of spectral acceleration.
#'
#' @return A list will be return, including mag, Rrup, Ztor, pecT, lnY, sigma, iflag.
#'
#' @examples
#' Sub.Intra.Com.001(6, 20, 5, 0)
#' Sub.Intra.Com.001(7, 20, 2, 0)
#'
#' @export
Sub.Intra.Com.001 <- function(Mag, Rrup, Ztor, Prd){
  # Subroutine S04_SubIntra_Common001 ( m, Rrup, ztor, specT, lnY, sigma, iflag )
  if (Prd != 0 & (Prd < 0.01 | Prd > 2)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("S04_SubIntra_Common001", m=as.single(Mag), Rrup=as.single(Rrup), ztor=as.single(Ztor),
                      specT=as.single(Prd), lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(0))

  names(retvals) <- c("mag", "Rrup", "Ztor", "specT", "lnY", "sigma", "iflag")

  return(retvals)
}


#' GMPE function for Subduction intraslab common form 002 (2017)
#'
#' \code{Sub.Intra.Com.002} returns the ground-motion prediction with it sigma of Subduction intraslab Common form 002 GMPE.
#'
#'Subduction intraslab common form 002
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Ztor Depth to the top of the finite rupture model (km).
#' @param Prd Period of spectral acceleration.
#'
#' @return A list will be return, including mag, Rrup, Ztor, pecT, lnY, sigma, iflag.
#'
#' @examples
#' Sub.Intra.Com.002(6, 20, 5, 0)
#' Sub.Intra.Com.002(7, 20, 2, 0)
#'
#' @export
Sub.Intra.Com.002 <- function(Mag, Rrup, Ztor, Prd){
  # Subroutine S04_SubIntra_Common002 ( m, Rrup, ztor, specT, lnY, sigma, iflag )
  if (Prd != 0 & (Prd < 0.01 | Prd > 2)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("S04_SubIntra_Common002", m=as.single(Mag), Rrup=as.single(Rrup), ztor=as.single(Ztor),
                      specT=as.single(Prd), lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(0))

  names(retvals) <- c("mag", "Rrup", "Ztor", "specT", "lnY", "sigma", "iflag")

  return(retvals)
}


#' GMPE function for Subduction intraslab common form 003 (2017)
#'
#' \code{Sub.Intra.Com.003} returns the ground-motion prediction with it sigma of Subduction intraslab Common form 003 GMPE.
#'
#'Subduction intraslab common form 003
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Ztor Depth to the top of the finite rupture model (km).
#' @param Prd Period of spectral acceleration.
#'
#' @return A list will be return, including mag, Rrup, Ztor, pecT, lnY, sigma, iflag.
#'
#' @examples
#' Sub.Intra.Com.003(6, 20, 5, 0)
#' Sub.Intra.Com.003(7, 20, 2, 0)
#'
#' @export
Sub.Intra.Com.003 <- function(Mag, Rrup, Ztor, Prd){
  # Subroutine S04_SubIntra_Common003 ( m, Rrup, ztor, specT, lnY, sigma, iflag )
  if (Prd != 0 & (Prd < 0.01 | Prd > 2)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("S04_SubIntra_Common003", m=as.single(Mag), Rrup=as.single(Rrup), ztor=as.single(Ztor),
                      specT=as.single(Prd), lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(0))

  names(retvals) <- c("mag", "Rrup", "Ztor", "specT", "lnY", "sigma", "iflag")

  return(retvals)
}


#' GMPE function for Subduction intraslab common form 004 (2017)
#'
#' \code{Sub.Intra.Com.004} returns the ground-motion prediction with it sigma of Subduction intraslab Common form 004 GMPE.
#'
#'Subduction intraslab common form 004
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Ztor Depth to the top of the finite rupture model (km).
#' @param Prd Period of spectral acceleration.
#'
#' @return A list will be return, including mag, Rrup, Ztor, pecT, lnY, sigma, iflag.
#'
#' @examples
#' Sub.Intra.Com.004(6, 20, 5, 0)
#' Sub.Intra.Com.004(7, 20, 2, 0)
#'
#' @export
Sub.Intra.Com.004 <- function(Mag, Rrup, Ztor, Prd){
  # Subroutine S04_SubIntra_Common004 ( m, Rrup, ztor, specT, lnY, sigma, iflag )
  if (Prd != 0 & (Prd < 0.01 | Prd > 2)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("S04_SubIntra_Common004", m=as.single(Mag), Rrup=as.single(Rrup), ztor=as.single(Ztor),
                      specT=as.single(Prd), lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(0))

  names(retvals) <- c("mag", "Rrup", "Ztor", "specT", "lnY", "sigma", "iflag")

  return(retvals)
}



#' GMPE function for Subduction intraslab common form 005 (2017)
#'
#' \code{Sub.Intra.Com.005} returns the ground-motion prediction with it sigma of Subduction intraslab Common form 005 GMPE.
#'
#'Subduction intraslab common form 005
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Ztor Depth to the top of the finite rupture model (km).
#' @param Prd Period of spectral acceleration.
#'
#' @return A list will be return, including mag, Rrup, Ztor, pecT, lnY, sigma, iflag.
#'
#' @examples
#' Sub.Intra.Com.005(6, 20, 5, 0)
#' Sub.Intra.Com.005(7, 20, 2, 0)
#'
#' @export
Sub.Intra.Com.005 <- function(Mag, Rrup, Ztor, Prd){
  # Subroutine S04_SubIntra_Common005 ( m, Rrup, ztor, specT, lnY, sigma, iflag )
  if (Prd != 0 & (Prd < 0.01 | Prd > 2)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("S04_SubIntra_Common005", m=as.single(Mag), Rrup=as.single(Rrup), ztor=as.single(Ztor),
                      specT=as.single(Prd), lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(0))

  names(retvals) <- c("mag", "Rrup", "Ztor", "specT", "lnY", "sigma", "iflag")

  return(retvals)
}


#' GMPE function for Subduction intraslab common form 006 (2017)
#'
#' \code{Sub.Intra.Com.006} returns the ground-motion prediction with it sigma of Subduction intraslab Common form 006 GMPE.
#'
#'Subduction intraslab common form 006
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Ztor Depth to the top of the finite rupture model (km).
#' @param Prd Period of spectral acceleration.
#'
#' @return A list will be return, including mag, Rrup, Ztor, pecT, lnY, sigma, iflag.
#'
#' @examples
#' Sub.Intra.Com.006(6, 20, 5, 0)
#' Sub.Intra.Com.006(7, 20, 2, 0)
#'
#' @export
Sub.Intra.Com.006 <- function(Mag, Rrup, Ztor, Prd){
  # Subroutine S04_SubIntra_Common006 ( m, Rrup, ztor, specT, lnY, sigma, iflag )
  if (Prd != 0 & (Prd < 0.01 | Prd > 2)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("S04_SubIntra_Common006", m=as.single(Mag), Rrup=as.single(Rrup), ztor=as.single(Ztor),
                      specT=as.single(Prd), lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(0))

  names(retvals) <- c("mag", "Rrup", "Ztor", "specT", "lnY", "sigma", "iflag")

  return(retvals)
}


#' GMPE function for Subduction intraslab common form 007 (2017)
#'
#' \code{Sub.Intra.Com.007} returns the ground-motion prediction with it sigma of Subduction intraslab Common form 007 GMPE.
#'
#'Subduction intraslab common form 007
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Ztor Depth to the top of the finite rupture model (km).
#' @param Prd Period of spectral acceleration.
#'
#' @return A list will be return, including mag, Rrup, Ztor, pecT, lnY, sigma, iflag.
#'
#' @examples
#' Sub.Intra.Com.007(6, 20, 5, 0)
#' Sub.Intra.Com.007(7, 20, 2, 0)
#'
#' @export
Sub.Intra.Com.007 <- function(Mag, Rrup, Ztor, Prd){
  # Subroutine S04_SubIntra_Common007 ( m, Rrup, ztor, specT, lnY, sigma, iflag )
  if (Prd != 0 & (Prd < 0.01 | Prd > 2)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("S04_SubIntra_Common007", m=as.single(Mag), Rrup=as.single(Rrup), ztor=as.single(Ztor),
                      specT=as.single(Prd), lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(0))

  names(retvals) <- c("mag", "Rrup", "Ztor", "specT", "lnY", "sigma", "iflag")

  return(retvals)
}


#' GMPE function for Subduction intraslab common form 008 (2017)
#'
#' \code{Sub.Intra.Com.008} returns the ground-motion prediction with it sigma of Subduction intraslab Common form 008 GMPE.
#'
#'Subduction intraslab common form 008
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Ztor Depth to the top of the finite rupture model (km).
#' @param Prd Period of spectral acceleration.
#'
#' @return A list will be return, including mag, Rrup, Ztor, pecT, lnY, sigma, iflag.
#'
#' @examples
#' Sub.Intra.Com.008(6, 20, 5, 0)
#' Sub.Intra.Com.008(7, 20, 2, 0)
#'
#' @export
Sub.Intra.Com.008 <- function(Mag, Rrup, Ztor, Prd){
  # Subroutine S04_SubIntra_Common008 ( m, Rrup, ztor, specT, lnY, sigma, iflag )
  if (Prd != 0 & (Prd < 0.01 | Prd > 2)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("S04_SubIntra_Common008", m=as.single(Mag), Rrup=as.single(Rrup), ztor=as.single(Ztor),
                      specT=as.single(Prd), lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(0))

  names(retvals) <- c("mag", "Rrup", "Ztor", "specT", "lnY", "sigma", "iflag")

  return(retvals)
}


#' GMPE function for Subduction intraslab common form 009 (2017)
#'
#' \code{Sub.Intra.Com.009} returns the ground-motion prediction with it sigma of Subduction intraslab Common form 009 GMPE.
#'
#'Subduction intraslab common form 009
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Ztor Depth to the top of the finite rupture model (km).
#' @param Prd Period of spectral acceleration.
#'
#' @return A list will be return, including mag, Rrup, Ztor, pecT, lnY, sigma, iflag.
#'
#' @examples
#' Sub.Intra.Com.009(6, 20, 5, 0)
#' Sub.Intra.Com.009(7, 20, 2, 0)
#'
#' @export
Sub.Intra.Com.009 <- function(Mag, Rrup, Ztor, Prd){
  # Subroutine S04_SubIntra_Common009 ( m, Rrup, ztor, specT, lnY, sigma, iflag )
  if (Prd != 0 & (Prd < 0.01 | Prd > 2)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("S04_SubIntra_Common009", m=as.single(Mag), Rrup=as.single(Rrup), ztor=as.single(Ztor),
                      specT=as.single(Prd), lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(0))

  names(retvals) <- c("mag", "Rrup", "Ztor", "specT", "lnY", "sigma", "iflag")

  return(retvals)
}


#' GMPE function for Subduction intraslab common form 010 (2017)
#'
#' \code{Sub.Intra.Com.010} returns the ground-motion prediction with it sigma of Subduction intraslab Common form 010 GMPE.
#'
#'Subduction intraslab common form 010
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Ztor Depth to the top of the finite rupture model (km).
#' @param Prd Period of spectral acceleration.
#'
#' @return A list will be return, including mag, Rrup, Ztor, pecT, lnY, sigma, iflag.
#'
#' @examples
#' Sub.Intra.Com.010(6, 20, 5, 0)
#' Sub.Intra.Com.010(7, 20, 2, 0)
#'
#' @export
Sub.Intra.Com.010 <- function(Mag, Rrup, Ztor, Prd){
  # Subroutine S04_SubIntra_Common010 ( m, Rrup, ztor, specT, lnY, sigma, iflag )
  if (Prd != 0 & (Prd < 0.01 | Prd > 2)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("S04_SubIntra_Common010", m=as.single(Mag), Rrup=as.single(Rrup), ztor=as.single(Ztor),
                      specT=as.single(Prd), lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(0))

  names(retvals) <- c("mag", "Rrup", "Ztor", "specT", "lnY", "sigma", "iflag")

  return(retvals)
}


#' GMPE function for Subduction intraslab common form 011 (2017)
#'
#' \code{Sub.Intra.Com.011} returns the ground-motion prediction with it sigma of Subduction intraslab Common form 011 GMPE.
#'
#'Subduction intraslab common form 011
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Ztor Depth to the top of the finite rupture model (km).
#' @param Prd Period of spectral acceleration.
#'
#' @return A list will be return, including mag, Rrup, Ztor, pecT, lnY, sigma, iflag.
#'
#' @examples
#' Sub.Intra.Com.011(6, 20, 5, 0)
#' Sub.Intra.Com.011(7, 20, 2, 0)
#'
#' @export
Sub.Intra.Com.011 <- function(Mag, Rrup, Ztor, Prd){
  # Subroutine S04_SubIntra_Common011 ( m, Rrup, ztor, specT, lnY, sigma, iflag )
  if (Prd != 0 & (Prd < 0.01 | Prd > 2)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("S04_SubIntra_Common011", m=as.single(Mag), Rrup=as.single(Rrup), ztor=as.single(Ztor),
                      specT=as.single(Prd), lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(0))

  names(retvals) <- c("mag", "Rrup", "Ztor", "specT", "lnY", "sigma", "iflag")

  return(retvals)
}


#' GMPE function for Subduction intraslab common form 012 (2017)
#'
#' \code{Sub.Intra.Com.012} returns the ground-motion prediction with it sigma of Subduction intraslab Common form 012 GMPE.
#'
#'Subduction intraslab common form 012
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Ztor Depth to the top of the finite rupture model (km).
#' @param Prd Period of spectral acceleration.
#'
#' @return A list will be return, including mag, Rrup, Ztor, pecT, lnY, sigma, iflag.
#'
#' @examples
#' Sub.Intra.Com.012(6, 20, 5, 0)
#' Sub.Intra.Com.012(7, 20, 2, 0)
#'
#' @export
Sub.Intra.Com.012 <- function(Mag, Rrup, Ztor, Prd){
  # Subroutine S04_SubIntra_Common012 ( m, Rrup, ztor, specT, lnY, sigma, iflag )
  if (Prd != 0 & (Prd < 0.01 | Prd > 2)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("S04_SubIntra_Common012", m=as.single(Mag), Rrup=as.single(Rrup), ztor=as.single(Ztor),
                      specT=as.single(Prd), lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(0))

  names(retvals) <- c("mag", "Rrup", "Ztor", "specT", "lnY", "sigma", "iflag")

  return(retvals)
}


#' GMPE function for Subduction intraslab common form 013 (2017)
#'
#' \code{Sub.Intra.Com.013} returns the ground-motion prediction with it sigma of Subduction intraslab Common form 013 GMPE.
#'
#'Subduction intraslab common form 013
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Ztor Depth to the top of the finite rupture model (km).
#' @param Prd Period of spectral acceleration.
#'
#' @return A list will be return, including mag, Rrup, Ztor, pecT, lnY, sigma, iflag.
#'
#' @examples
#' Sub.Intra.Com.013(6, 20, 5, 0)
#' Sub.Intra.Com.013(7, 20, 2, 0)
#'
#' @export
Sub.Intra.Com.013 <- function(Mag, Rrup, Ztor, Prd){
  # Subroutine S04_SubIntra_Common013 ( m, Rrup, ztor, specT, lnY, sigma, iflag )
  if (Prd != 0 & (Prd < 0.01 | Prd > 2)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("S04_SubIntra_Common013", m=as.single(Mag), Rrup=as.single(Rrup), ztor=as.single(Ztor),
                      specT=as.single(Prd), lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(0))

  names(retvals) <- c("mag", "Rrup", "Ztor", "specT", "lnY", "sigma", "iflag")

  return(retvals)
}


#' GMPE function for Subduction intraslab common form 014 (2017)
#'
#' \code{Sub.Intra.Com.014} returns the ground-motion prediction with it sigma of Subduction intraslab Common form 014 GMPE.
#'
#'Subduction intraslab common form 014
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Ztor Depth to the top of the finite rupture model (km).
#' @param Prd Period of spectral acceleration.
#'
#' @return A list will be return, including mag, Rrup, Ztor, pecT, lnY, sigma, iflag.
#'
#' @examples
#' Sub.Intra.Com.014(6, 20, 5, 0)
#' Sub.Intra.Com.014(7, 20, 2, 0)
#'
#' @export
Sub.Intra.Com.014 <- function(Mag, Rrup, Ztor, Prd){
  # Subroutine S04_SubIntra_Common014 ( m, Rrup, ztor, specT, lnY, sigma, iflag )
  if (Prd != 0 & (Prd < 0.01 | Prd > 2)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("S04_SubIntra_Common014", m=as.single(Mag), Rrup=as.single(Rrup), ztor=as.single(Ztor),
                      specT=as.single(Prd), lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(0))

  names(retvals) <- c("mag", "Rrup", "Ztor", "specT", "lnY", "sigma", "iflag")

  return(retvals)
}


#' GMPE function for Subduction intraslab common form 015 (2017)
#'
#' \code{Sub.Intra.Com.015} returns the ground-motion prediction with it sigma of Subduction intraslab Common form 015 GMPE.
#'
#'Subduction intraslab common form 015
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Ztor Depth to the top of the finite rupture model (km).
#' @param Prd Period of spectral acceleration.
#'
#' @return A list will be return, including mag, Rrup, Ztor, pecT, lnY, sigma, iflag.
#'
#' @examples
#' Sub.Intra.Com.015(6, 20, 5, 0)
#' Sub.Intra.Com.015(7, 20, 2, 0)
#'
#' @export
Sub.Intra.Com.015 <- function(Mag, Rrup, Ztor, Prd){
  # Subroutine S04_SubIntra_Common015 ( m, Rrup, ztor, specT, lnY, sigma, iflag )
  if (Prd != 0 & (Prd < 0.01 | Prd > 2)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("S04_SubIntra_Common015", m=as.single(Mag), Rrup=as.single(Rrup), ztor=as.single(Ztor),
                      specT=as.single(Prd), lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(0))

  names(retvals) <- c("mag", "Rrup", "Ztor", "specT", "lnY", "sigma", "iflag")

  return(retvals)
}


#' GMPE function for Subduction intraslab common form 016 (2017)
#'
#' \code{Sub.Intra.Com.016} returns the ground-motion prediction with it sigma of Subduction intraslab Common form 016 GMPE.
#'
#'Subduction intraslab common form 016
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Ztor Depth to the top of the finite rupture model (km).
#' @param Prd Period of spectral acceleration.
#'
#' @return A list will be return, including mag, Rrup, Ztor, pecT, lnY, sigma, iflag.
#'
#' @examples
#' Sub.Intra.Com.016(6, 20, 5, 0)
#' Sub.Intra.Com.016(7, 20, 2, 0)
#'
#' @export
Sub.Intra.Com.016 <- function(Mag, Rrup, Ztor, Prd){
  # Subroutine S04_SubIntra_Common016 ( m, Rrup, ztor, specT, lnY, sigma, iflag )
  if (Prd != 0 & (Prd < 0.01 | Prd > 2)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("S04_SubIntra_Common016", m=as.single(Mag), Rrup=as.single(Rrup), ztor=as.single(Ztor),
                      specT=as.single(Prd), lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(0))

  names(retvals) <- c("mag", "Rrup", "Ztor", "specT", "lnY", "sigma", "iflag")

  return(retvals)
}


#' GMPE function for Subduction intraslab common form 017 (2017)
#'
#' \code{Sub.Intra.Com.017} returns the ground-motion prediction with it sigma of Subduction intraslab Common form 017 GMPE.
#'
#'Subduction intraslab common form 017
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Ztor Depth to the top of the finite rupture model (km).
#' @param Prd Period of spectral acceleration.
#'
#' @return A list will be return, including mag, Rrup, Ztor, pecT, lnY, sigma, iflag.
#'
#' @examples
#' Sub.Intra.Com.017(6, 20, 5, 0)
#' Sub.Intra.Com.017(7, 20, 2, 0)
#'
#' @export
Sub.Intra.Com.017 <- function(Mag, Rrup, Ztor, Prd){
  # Subroutine S04_SubIntra_Common017 ( m, Rrup, ztor, specT, lnY, sigma, iflag )
  if (Prd != 0 & (Prd < 0.01 | Prd > 2)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("S04_SubIntra_Common017", m=as.single(Mag), Rrup=as.single(Rrup), ztor=as.single(Ztor),
                      specT=as.single(Prd), lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(0))

  names(retvals) <- c("mag", "Rrup", "Ztor", "specT", "lnY", "sigma", "iflag")

  return(retvals)
}



#' GMPE function for Subduction interface common form 001 (2017)
#'
#' \code{Sub.Inter.Com.001} returns the ground-motion prediction with it sigma of Subduction interface Common form 001 GMPE.
#'
#'Subduction interface common form 001
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Ztor Depth to the top of the finite rupture model (km).
#' @param Prd Period of spectral acceleration.
#'
#' @return A list will be return, including mag, Rrup, Ztor, pecT, lnY, sigma, iflag.
#'
#' @examples
#' Sub.Inter.Com.001(6, 20, 5, 0)
#' Sub.Inter.Com.001(7, 20, 2, 0)
#'
#' @export
Sub.Inter.Com.001 <- function(Mag, Rrup, Ztor, Prd){
  # Subroutine S04_SubInter_Common001 ( m, Rrup, ztor, specT, lnY, sigma, iflag )
  if (Prd != 0 & (Prd < 0.01 | Prd > 2)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("S04_SubInter_Common001", m=as.single(Mag), Rrup=as.single(Rrup), ztor=as.single(Ztor),
                      specT=as.single(Prd), lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(0))

  names(retvals) <- c("mag", "Rrup", "Ztor", "specT", "lnY", "sigma", "iflag")

  return(retvals)
}


#' GMPE function for Subduction interface common form 002 (2017)
#'
#' \code{Sub.Inter.Com.002} returns the ground-motion prediction with it sigma of Subduction interface Common form 002 GMPE.
#'
#'Subduction interface common form 002
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Ztor Depth to the top of the finite rupture model (km).
#' @param Prd Period of spectral acceleration.
#'
#' @return A list will be return, including mag, Rrup, Ztor, pecT, lnY, sigma, iflag.
#'
#' @examples
#' Sub.Inter.Com.002(6, 20, 5, 0)
#' Sub.Inter.Com.002(7, 20, 2, 0)
#'
#' @export
Sub.Inter.Com.002 <- function(Mag, Rrup, Ztor, Prd){
  # Subroutine S04_SubInter_Common002 ( m, Rrup, ztor, specT, lnY, sigma, iflag )
  if (Prd != 0 & (Prd < 0.01 | Prd > 2)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("S04_SubInter_Common002", m=as.single(Mag), Rrup=as.single(Rrup), ztor=as.single(Ztor),
                      specT=as.single(Prd), lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(0))

  names(retvals) <- c("mag", "Rrup", "Ztor", "specT", "lnY", "sigma", "iflag")

  return(retvals)
}


#' GMPE function for Subduction interface common form 003 (2017)
#'
#' \code{Sub.Inter.Com.003} returns the ground-motion prediction with it sigma of Subduction interface Common form 003 GMPE.
#'
#'Subduction interface common form 003
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Ztor Depth to the top of the finite rupture model (km).
#' @param Prd Period of spectral acceleration.
#'
#' @return A list will be return, including mag, Rrup, Ztor, pecT, lnY, sigma, iflag.
#'
#' @examples
#' Sub.Inter.Com.003(6, 20, 5, 0)
#' Sub.Inter.Com.003(7, 20, 2, 0)
#'
#' @export
Sub.Inter.Com.003 <- function(Mag, Rrup, Ztor, Prd){
  # Subroutine S04_SubInter_Common003 ( m, Rrup, ztor, specT, lnY, sigma, iflag )
  if (Prd != 0 & (Prd < 0.01 | Prd > 2)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("S04_SubInter_Common003", m=as.single(Mag), Rrup=as.single(Rrup), ztor=as.single(Ztor),
                      specT=as.single(Prd), lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(0))

  names(retvals) <- c("mag", "Rrup", "Ztor", "specT", "lnY", "sigma", "iflag")

  return(retvals)
}


#' GMPE function for Subduction interface common form 004 (2017)
#'
#' \code{Sub.Inter.Com.004} returns the ground-motion prediction with it sigma of Subduction interface Common form 004 GMPE.
#'
#'Subduction interface common form 004
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Ztor Depth to the top of the finite rupture model (km).
#' @param Prd Period of spectral acceleration.
#'
#' @return A list will be return, including mag, Rrup, Ztor, pecT, lnY, sigma, iflag.
#'
#' @examples
#' Sub.Inter.Com.004(6, 20, 5, 0)
#' Sub.Inter.Com.004(7, 20, 2, 0)
#'
#' @export
Sub.Inter.Com.004 <- function(Mag, Rrup, Ztor, Prd){
  # Subroutine S04_SubInter_Common004 ( m, Rrup, ztor, specT, lnY, sigma, iflag )
  if (Prd != 0 & (Prd < 0.01 | Prd > 2)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("S04_SubInter_Common004", m=as.single(Mag), Rrup=as.single(Rrup), ztor=as.single(Ztor),
                      specT=as.single(Prd), lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(0))

  names(retvals) <- c("mag", "Rrup", "Ztor", "specT", "lnY", "sigma", "iflag")

  return(retvals)
}



#' GMPE function for Subduction interface common form 005 (2017)
#'
#' \code{Sub.Inter.Com.005} returns the ground-motion prediction with it sigma of Subduction interface Common form 005 GMPE.
#'
#'Subduction interface common form 005
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Ztor Depth to the top of the finite rupture model (km).
#' @param Prd Period of spectral acceleration.
#'
#' @return A list will be return, including mag, Rrup, Ztor, pecT, lnY, sigma, iflag.
#'
#' @examples
#' Sub.Inter.Com.005(6, 20, 5, 0)
#' Sub.Inter.Com.005(7, 20, 2, 0)
#'
#' @export
Sub.Inter.Com.005 <- function(Mag, Rrup, Ztor, Prd){
  # Subroutine S04_SubInter_Common005 ( m, Rrup, ztor, specT, lnY, sigma, iflag )
  if (Prd != 0 & (Prd < 0.01 | Prd > 2)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("S04_SubInter_Common005", m=as.single(Mag), Rrup=as.single(Rrup), ztor=as.single(Ztor),
                      specT=as.single(Prd), lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(0))

  names(retvals) <- c("mag", "Rrup", "Ztor", "specT", "lnY", "sigma", "iflag")

  return(retvals)
}


#' GMPE function for Subduction interface common form 006 (2017)
#'
#' \code{Sub.Inter.Com.006} returns the ground-motion prediction with it sigma of Subduction interface Common form 006 GMPE.
#'
#'Subduction interface common form 006
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Ztor Depth to the top of the finite rupture model (km).
#' @param Prd Period of spectral acceleration.
#'
#' @return A list will be return, including mag, Rrup, Ztor, pecT, lnY, sigma, iflag.
#'
#' @examples
#' Sub.Inter.Com.006(6, 20, 5, 0)
#' Sub.Inter.Com.006(7, 20, 2, 0)
#'
#' @export
Sub.Inter.Com.006 <- function(Mag, Rrup, Ztor, Prd){
  # Subroutine S04_SubInter_Common006 ( m, Rrup, ztor, specT, lnY, sigma, iflag )
  if (Prd != 0 & (Prd < 0.01 | Prd > 2)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("S04_SubInter_Common006", m=as.single(Mag), Rrup=as.single(Rrup), ztor=as.single(Ztor),
                      specT=as.single(Prd), lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(0))

  names(retvals) <- c("mag", "Rrup", "Ztor", "specT", "lnY", "sigma", "iflag")

  return(retvals)
}


#' GMPE function for Subduction interface common form 007 (2017)
#'
#' \code{Sub.Inter.Com.007} returns the ground-motion prediction with it sigma of Subduction interface Common form 007 GMPE.
#'
#'Subduction interface common form 007
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Ztor Depth to the top of the finite rupture model (km).
#' @param Prd Period of spectral acceleration.
#'
#' @return A list will be return, including mag, Rrup, Ztor, pecT, lnY, sigma, iflag.
#'
#' @examples
#' Sub.Inter.Com.007(6, 20, 5, 0)
#' Sub.Inter.Com.007(7, 20, 2, 0)
#'
#' @export
Sub.Inter.Com.007 <- function(Mag, Rrup, Ztor, Prd){
  # Subroutine S04_SubInter_Common007 ( m, Rrup, ztor, specT, lnY, sigma, iflag )
  if (Prd != 0 & (Prd < 0.01 | Prd > 2)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("S04_SubInter_Common007", m=as.single(Mag), Rrup=as.single(Rrup), ztor=as.single(Ztor),
                      specT=as.single(Prd), lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(0))

  names(retvals) <- c("mag", "Rrup", "Ztor", "specT", "lnY", "sigma", "iflag")

  return(retvals)
}


#' GMPE function for Subduction interface common form 008 (2017)
#'
#' \code{Sub.Inter.Com.008} returns the ground-motion prediction with it sigma of Subduction interface Common form 008 GMPE.
#'
#'Subduction interface common form 008
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Ztor Depth to the top of the finite rupture model (km).
#' @param Prd Period of spectral acceleration.
#'
#' @return A list will be return, including mag, Rrup, Ztor, pecT, lnY, sigma, iflag.
#'
#' @examples
#' Sub.Inter.Com.008(6, 20, 5, 0)
#' Sub.Inter.Com.008(7, 20, 2, 0)
#'
#' @export
Sub.Inter.Com.008 <- function(Mag, Rrup, Ztor, Prd){
  # Subroutine S04_SubInter_Common008 ( m, Rrup, ztor, specT, lnY, sigma, iflag )
  if (Prd != 0 & (Prd < 0.01 | Prd > 2)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("S04_SubInter_Common008", m=as.single(Mag), Rrup=as.single(Rrup), ztor=as.single(Ztor),
                      specT=as.single(Prd), lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(0))

  names(retvals) <- c("mag", "Rrup", "Ztor", "specT", "lnY", "sigma", "iflag")

  return(retvals)
}


#' GMPE function for Subduction interface common form 009 (2017)
#'
#' \code{Sub.Inter.Com.009} returns the ground-motion prediction with it sigma of Subduction interface Common form 009 GMPE.
#'
#'Subduction interface common form 009
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Ztor Depth to the top of the finite rupture model (km).
#' @param Prd Period of spectral acceleration.
#'
#' @return A list will be return, including mag, Rrup, Ztor, pecT, lnY, sigma, iflag.
#'
#' @examples
#' Sub.Inter.Com.009(6, 20, 5, 0)
#' Sub.Inter.Com.009(7, 20, 2, 0)
#'
#' @export
Sub.Inter.Com.009 <- function(Mag, Rrup, Ztor, Prd){
  # Subroutine S04_SubInter_Common009 ( m, Rrup, ztor, specT, lnY, sigma, iflag )
  if (Prd != 0 & (Prd < 0.01 | Prd > 2)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("S04_SubInter_Common009", m=as.single(Mag), Rrup=as.single(Rrup), ztor=as.single(Ztor),
                      specT=as.single(Prd), lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(0))

  names(retvals) <- c("mag", "Rrup", "Ztor", "specT", "lnY", "sigma", "iflag")

  return(retvals)
}


#' GMPE function for Subduction interface common form 010 (2017)
#'
#' \code{Sub.Inter.Com.010} returns the ground-motion prediction with it sigma of Subduction interface Common form 010 GMPE.
#'
#'Subduction interface common form 010
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Ztor Depth to the top of the finite rupture model (km).
#' @param Prd Period of spectral acceleration.
#'
#' @return A list will be return, including mag, Rrup, Ztor, pecT, lnY, sigma, iflag.
#'
#' @examples
#' Sub.Inter.Com.010(6, 20, 5, 0)
#' Sub.Inter.Com.010(7, 20, 2, 0)
#'
#' @export
Sub.Inter.Com.010 <- function(Mag, Rrup, Ztor, Prd){
  # Subroutine S04_SubInter_Common010 ( m, Rrup, ztor, specT, lnY, sigma, iflag )
  if (Prd != 0 & (Prd < 0.01 | Prd > 2)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("S04_SubInter_Common010", m=as.single(Mag), Rrup=as.single(Rrup), ztor=as.single(Ztor),
                      specT=as.single(Prd), lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(0))

  names(retvals) <- c("mag", "Rrup", "Ztor", "specT", "lnY", "sigma", "iflag")

  return(retvals)
}


#' GMPE function for Subduction interface common form 011 (2017)
#'
#' \code{Sub.Inter.Com.011} returns the ground-motion prediction with it sigma of Subduction interface Common form 011 GMPE.
#'
#'Subduction interface common form 011
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Ztor Depth to the top of the finite rupture model (km).
#' @param Prd Period of spectral acceleration.
#'
#' @return A list will be return, including mag, Rrup, Ztor, pecT, lnY, sigma, iflag.
#'
#' @examples
#' Sub.Inter.Com.011(6, 20, 5, 0)
#' Sub.Inter.Com.011(7, 20, 2, 0)
#'
#' @export
Sub.Inter.Com.011 <- function(Mag, Rrup, Ztor, Prd){
  # Subroutine S04_SubInter_Common011 ( m, Rrup, ztor, specT, lnY, sigma, iflag )
  if (Prd != 0 & (Prd < 0.01 | Prd > 2)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("S04_SubInter_Common011", m=as.single(Mag), Rrup=as.single(Rrup), ztor=as.single(Ztor),
                      specT=as.single(Prd), lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(0))

  names(retvals) <- c("mag", "Rrup", "Ztor", "specT", "lnY", "sigma", "iflag")

  return(retvals)
}


#' GMPE function for Subduction interface common form 012 (2017)
#'
#' \code{Sub.Inter.Com.012} returns the ground-motion prediction with it sigma of Subduction interface Common form 012 GMPE.
#'
#'Subduction interface common form 012
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Ztor Depth to the top of the finite rupture model (km).
#' @param Prd Period of spectral acceleration.
#'
#' @return A list will be return, including mag, Rrup, Ztor, pecT, lnY, sigma, iflag.
#'
#' @examples
#' Sub.Inter.Com.012(6, 20, 5, 0)
#' Sub.Inter.Com.012(7, 20, 2, 0)
#'
#' @export
Sub.Inter.Com.012 <- function(Mag, Rrup, Ztor, Prd){
  # Subroutine S04_SubInter_Common012 ( m, Rrup, ztor, specT, lnY, sigma, iflag )
  if (Prd != 0 & (Prd < 0.01 | Prd > 2)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("S04_SubInter_Common012", m=as.single(Mag), Rrup=as.single(Rrup), ztor=as.single(Ztor),
                      specT=as.single(Prd), lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(0))

  names(retvals) <- c("mag", "Rrup", "Ztor", "specT", "lnY", "sigma", "iflag")

  return(retvals)
}


#' GMPE function for Subduction interface common form 013 (2017)
#'
#' \code{Sub.Inter.Com.013} returns the ground-motion prediction with it sigma of Subduction interface Common form 013 GMPE.
#'
#'Subduction interface common form 013
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Ztor Depth to the top of the finite rupture model (km).
#' @param Prd Period of spectral acceleration.
#'
#' @return A list will be return, including mag, Rrup, Ztor, pecT, lnY, sigma, iflag.
#'
#' @examples
#' Sub.Inter.Com.013(6, 20, 5, 0)
#' Sub.Inter.Com.013(7, 20, 2, 0)
#'
#' @export
Sub.Inter.Com.013 <- function(Mag, Rrup, Ztor, Prd){
  # Subroutine S04_SubInter_Common013 ( m, Rrup, ztor, specT, lnY, sigma, iflag )
  if (Prd != 0 & (Prd < 0.01 | Prd > 2)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("S04_SubInter_Common013", m=as.single(Mag), Rrup=as.single(Rrup), ztor=as.single(Ztor),
                      specT=as.single(Prd), lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(0))

  names(retvals) <- c("mag", "Rrup", "Ztor", "specT", "lnY", "sigma", "iflag")

  return(retvals)
}


#' GMPE function for Subduction interface common form 014 (2017)
#'
#' \code{Sub.Inter.Com.014} returns the ground-motion prediction with it sigma of Subduction interface Common form 014 GMPE.
#'
#'Subduction interface common form 014
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Ztor Depth to the top of the finite rupture model (km).
#' @param Prd Period of spectral acceleration.
#'
#' @return A list will be return, including mag, Rrup, Ztor, pecT, lnY, sigma, iflag.
#'
#' @examples
#' Sub.Inter.Com.014(6, 20, 5, 0)
#' Sub.Inter.Com.014(7, 20, 2, 0)
#'
#' @export
Sub.Inter.Com.014 <- function(Mag, Rrup, Ztor, Prd){
  # Subroutine S04_SubInter_Common014 ( m, Rrup, ztor, specT, lnY, sigma, iflag )
  if (Prd != 0 & (Prd < 0.01 | Prd > 2)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("S04_SubInter_Common014", m=as.single(Mag), Rrup=as.single(Rrup), ztor=as.single(Ztor),
                      specT=as.single(Prd), lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(0))

  names(retvals) <- c("mag", "Rrup", "Ztor", "specT", "lnY", "sigma", "iflag")

  return(retvals)
}


#' GMPE function for Subduction interface common form 015 (2017)
#'
#' \code{Sub.Inter.Com.015} returns the ground-motion prediction with it sigma of Subduction interface Common form 015 GMPE.
#'
#'Subduction interface common form 015
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Ztor Depth to the top of the finite rupture model (km).
#' @param Prd Period of spectral acceleration.
#'
#' @return A list will be return, including mag, Rrup, Ztor, pecT, lnY, sigma, iflag.
#'
#' @examples
#' Sub.Inter.Com.015(6, 20, 5, 0)
#' Sub.Inter.Com.015(7, 20, 2, 0)
#'
#' @export
Sub.Inter.Com.015 <- function(Mag, Rrup, Ztor, Prd){
  # Subroutine S04_SubInter_Common015 ( m, Rrup, ztor, specT, lnY, sigma, iflag )
  if (Prd != 0 & (Prd < 0.01 | Prd > 2)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("S04_SubInter_Common015", m=as.single(Mag), Rrup=as.single(Rrup), ztor=as.single(Ztor),
                      specT=as.single(Prd), lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(0))

  names(retvals) <- c("mag", "Rrup", "Ztor", "specT", "lnY", "sigma", "iflag")

  return(retvals)
}


#' GMPE function for Subduction interface common form 016 (2017)
#'
#' \code{Sub.Inter.Com.016} returns the ground-motion prediction with it sigma of Subduction interface Common form 016 GMPE.
#'
#'Subduction interface common form 016
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Ztor Depth to the top of the finite rupture model (km).
#' @param Prd Period of spectral acceleration.
#'
#' @return A list will be return, including mag, Rrup, Ztor, pecT, lnY, sigma, iflag.
#'
#' @examples
#' Sub.Inter.Com.016(6, 20, 5, 0)
#' Sub.Inter.Com.016(7, 20, 2, 0)
#'
#' @export
Sub.Inter.Com.016 <- function(Mag, Rrup, Ztor, Prd){
  # Subroutine S04_SubInter_Common016 ( m, Rrup, ztor, specT, lnY, sigma, iflag )
  if (Prd != 0 & (Prd < 0.01 | Prd > 2)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("S04_SubInter_Common016", m=as.single(Mag), Rrup=as.single(Rrup), ztor=as.single(Ztor),
                      specT=as.single(Prd), lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(0))

  names(retvals) <- c("mag", "Rrup", "Ztor", "specT", "lnY", "sigma", "iflag")

  return(retvals)
}


#' GMPE function for Subduction interface common form 017 (2017)
#'
#' \code{Sub.Inter.Com.017} returns the ground-motion prediction with it sigma of Subduction interface Common form 017 GMPE.
#'
#'Subduction interface common form 017
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Ztor Depth to the top of the finite rupture model (km).
#' @param Prd Period of spectral acceleration.
#'
#' @return A list will be return, including mag, Rrup, Ztor, pecT, lnY, sigma, iflag.
#'
#' @examples
#' Sub.Inter.Com.017(6, 20, 5, 0)
#' Sub.Inter.Com.017(7, 20, 2, 0)
#'
#' @export
Sub.Inter.Com.017 <- function(Mag, Rrup, Ztor, Prd){
  # Subroutine S04_SubInter_Common017 ( m, Rrup, ztor, specT, lnY, sigma, iflag )
  if (Prd != 0 & (Prd < 0.01 | Prd > 2)) {
    stop("Period out of range! \n\n")
  }
  retvals <- .Fortran("S04_SubInter_Common017", m=as.single(Mag), Rrup=as.single(Rrup), ztor=as.single(Ztor),
                      specT=as.single(Prd), lnY=as.single(0.1), sigma=as.single(0.1), iflag=as.integer(0))

  names(retvals) <- c("mag", "Rrup", "Ztor", "specT", "lnY", "sigma", "iflag")

  return(retvals)
}
