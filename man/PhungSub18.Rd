% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PhungSub18.R
\name{PhungSub18}
\alias{PhungSub18}
\title{GMPE function for Phung et al.(2018)}
\usage{
PhungSub18(Mag, Rrup, Prd, ftype = 0, Vs30, Z1.0, Ztor, regionflag = 1)
}
\arguments{
\item{Mag}{Earthquake momnet magnitude, Numeric.}

\item{Rrup}{Rupture distance(km), Numeric.}

\item{Prd}{Period of spectral acceleration.}

\item{ftype}{fytpe=0 for interface, ftype=1 for intraslab.}

\item{Vs30}{Vs30(m/s).}

\item{Z1.0}{the depth to the shear-wave velocity horizon of 1.0(km/s).}

\item{Ztor}{Depth to the top of the finite rupture model (km).}

\item{regionflag}{0 Japan, 1 Taiwan}
}
\value{
A list will be return, including mag, ftype, Rrup, Vs30, lnY, sigma, specT, period, iflag, forearc, depth, Rhypo.
}
\description{
\code{PhungSub18} returns the ground-motion prediction with it sigma of Phung et al.(2018) GMPE.
}
\details{
Phung et al.(2018) Subduction GMPE
}
\examples{
PhungSub18(6, 20, 0, 0, 760, 0.5, 0, 1)
PhungSub18(7, 10, 0, 1, 760, 0.5, 0, 1)

}
