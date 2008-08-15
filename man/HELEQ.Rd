\name{HELEQ}
\alias{HELEQ}
\docType{data}
\title{Mount Saint Helens Earthquake Cluster associated with the 1980 Eruption}
\description{
 Mount Saint Helens Earthquake Cluster associated with the 1980 Eruption
}
\usage{data(HELEQ)}
\format{
  List:
  \describe{
    \item{yr}{year}
    \item{mo}{Month}
    \item{dom}{Day of Month}
    \item{hr}{Hour}
    \item{mi}{Minute}
    \item{sec}{Second}
    \item{jd}{Julian Day}
    \item{lat}{latitude, signed decimal degree (West is negative)}
    \item{lon}{longitude, signed decimal degree}
    \item{z}{depth, km}
    \item{mag}{magitude}
    \item{gap}{gap}
    \item{delta}{distance to nearest station}
    \item{rms}{root, mean square error}
    \item{hozerr}{horrizontal uncertainty, km}
  }

}
\details{
 This is a small selection of the events associated with the eruption.
}

\references{

  Lees, J. M.  (1992): The magma system of Mount St. Helens: Non-linear
  high resolution P-wave tomography,
  J. Volc. Geoth. Res., 53(1-4), 103-116.
 
  
  Lees, J. M. and R. S. Crosson (1989):
  Tomographic inversion for three-dimensional velocity structure at
  Mount St. Helens using earthquake data, J. Geophys. Res., 94(B5),
  5716-5728.

  



  
}
\examples{
data(HELEQ)
## maybe str(HELEQ) ; plot(HELEQ) ...
}
\keyword{datasets}
