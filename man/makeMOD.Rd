\name{makeMOD}
\alias{makeMOD}
\title{Make a 3D model}
\description{
  Create a three-dimensional synthetic model for
  use in predicting travel-times.
}
\usage{
makeMOD(xo, yo, ztop, x, y, z, r, v, bg)
}
\arguments{
  \item{xo}{x-nodes}
  \item{yo}{y-nodes}
  \item{ztop}{tops of layers}
  \item{x}{x-coordinates of balls}
  \item{y}{y-coordinates of balls }
  \item{z}{z-coordinates of balls }
  \item{r}{radii of balls}
  \item{v}{velocity of balls}
  \item{bg}{background velocity for 1-D model}
}
\details{
  Balls are spherical - this may change in future implementations
  to ellipsoids.
}
\value{
 Model List
}
\references{}
\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{GXMA3D, SHOWTOMO}
\examples{


data(HELMOD)
data(HEL1D)

xo = HELMOD$x
yo = HELMOD$y
ztop=HEL1D$zp[1:15]
bg = HEL1D$vp[1:15]
x = 10
y = 15
z = 6
r = 6
v = 3




TM1 = makeMOD(xo, yo, ztop , x, y, z, r, v , bg )
SHOWTOMO(TM1)

}
\keyword{misc}
