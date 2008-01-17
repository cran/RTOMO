\name{HELsta}
\alias{HELsta}
\docType{data}
\title{Station locations near Mt. Saint Helens, WA}
\description{
 Station locations near Mt. Saint Helens, WA: LAT, LON, Elevation
}
\usage{data(HELsta)}
\format{
  \item{name}{character, station name}
  \item{lat}{latitude, signed decimal degree (West is negative)}
  \item{lon}{longitude, signed decimal degree}
  \item{z}{elevation, km}
}

\references{
  Lees, J. M.  (1992): The magma system of Mount St. Helens: Non-linear
  high resolution P-wave tomography,
  J. Volc. Geoth. Res., 53(1-4), 103-116.
 
}
\examples{
data(HELsta)
## maybe str(HELsta) ; plot(HELsta) ...
}
\keyword{datasets}
