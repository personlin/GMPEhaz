% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PhungCru18.R
\name{PhungCru18}
\alias{PhungCru18}
\title{GMPE function for Phung et al.(2018)}
\usage{
PhungCru18(Mag, Rrup, Rjb, Rx, Prd, Vs30, Dip, Ztor, ftype = 0, Z1.0,
  Vs30_class = 1, regionflag = 1, hwflag = 0)
}
\arguments{
\item{Mag}{Earthquake momnet magnitude, Numeric.}

\item{Rrup}{Rupture distance(km), Numeric.}

\item{Rjb}{Joyner and Boore distance(km), Numeric.}

\item{Rx}{Horizontal distance(km) from top edge of rupture. Measured perpendicular to the fault strike.}

\item{Prd}{Period of spectral acceleration.}

\item{Vs30}{Vs30(m/s).}

\item{Dip}{Dip angle of the fault plane.}

\item{Ztor}{Depth to the top of the finite rupture model (km).}

\item{ftype}{style of faulting.}

\item{Z1.0}{the depth to the shear-wave velocity horizon of 1.0(km/s).}

\item{Vs30_class}{1 measured, 0 estimated.}

\item{regionflag}{0 Global, 1 Taiwan}

\item{hwflag}{hanging-wall flag, 1 for hanging-wall.}
}
\value{
A list will be return, including m, Rrup, Rjb, Rx, specT, period2, lnY, sigma, iflag,
   vs, Delta, DTor, Ftype, depthvs10, vs30_class, regionflag, phi, tau, HWflag, Rx.
}
\description{
\code{PhungCru18} returns the ground-motion prediction with it sigma of Phung et al.(2018) GMPE.
}
\details{
Phung et al.(2018) Crustal GMPE
}
\examples{
PhungCru18(6, 20, 20, 20, 0, 760, 90, 0, 0, 0.5, 1, 1, 0)
PhungCru18(7, 20, 20, 20, 0, 760, 90, 0, 0, 0.5, 1, 1, 0)

}
