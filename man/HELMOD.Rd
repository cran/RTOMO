\name{HELMOD}
\alias{HELMOD}
\docType{data}
\title{Tomographic Model of Mt. Saint Helens subsurface}
\description{
 Three-dimensional Tomographic Model of Mt. Saint Helens subsurface
}
\usage{data(HELMOD)}
\format{
  \describe{
    \item{name}{name of file}
    \item{A}{list origin information }
    \item{lat}{latitude, signed decimal degree (West is negative)}
    \item{lon}{longitude, signed decimal degree}
    \item{nx}{dimension or number of nodes in x-direction}
    \item{ny}{dimension or number of nodes in y-direction}
    \item{nz}{dimension or number of nodes in z-direction}
    \item{dx}{delta x}
    \item{dy}{delta y}
    \item{skip}{skipping flag}
    \item{D}{depths in Z, km}
    \item{V}{background model for 1D structure}
    \item{MOD}{3D model structure}
    \item{x}{x divisions (nodes in x, km)}
    \item{y}{y divisions (nodes in y, km) }
  }

}

\references{
   Lees, J. M.  (1992): The magma system of Mount St. Helens: Non-linear
  high resolution P-wave tomography,
  J. Volc. Geoth. Res., 53(1-4), 103-116.
 
}
\examples{
data(HELMOD)
## maybe str(HELMOD) ; plot(HELMOD) ...
}
\keyword{datasets}
