\name{get3Drayblox}
\alias{get3Drayblox}

\title{Get blocks from 3D ray path}
\description{
 Get blocks from 3D ray path
}
\usage{
get3Drayblox(XNOD, YNOD, ZNOD, xo, yo, ztop, slowness = NULL)
}

\arguments{
  \item{XNOD}{x-coordinates along raypath}
  \item{YNOD}{y-coordinates along raypath}
  \item{ZNOD}{z-coordinates along raypath}
  \item{xo}{x block divisions}
  \item{yo}{y block divisions}
  \item{ztop}{vector, topsof layers}
  \item{slowness}{vector, Slowness model}
}
\details{
 
}
\value{
  \item{ix}{x index}
  \item{iy}{y index}
  \item{iz}{layer index}
  \item{r}{length in each block}
  \item{tt}{travel time along whole raypath}
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}
\note{ 
}
\seealso{get2Drayblox}
\examples{


xo = seq(from=0.5, by=1, length=100)
yo = seq(from=0.5, by=1, length=100)
 ztop = seq(from=0, to=24, by=4)




Xp = c(11.5, 70.2)
Yp = c(3.5, 50.2)
Zp = c(18.4, 0.0)

dee = sqrt(  (Xp[2]-Xp[1])^2 + (Yp[2]-Yp[1])^2 +  (Zp[2]-Zp[1])^2 )
deexy = sqrt(  (Xp[2]-Xp[1])^2 + (Yp[2]-Yp[1])^2  )

fi  = findInterval(Zp, ztop)

ZNOD = c(Zp[1], ztop[fi[1]:fi[2]])


alpha = asin(deexy/dee)

RN = deexy-ZNOD*tan(alpha)

XNOD = Xp[1]+RN*(Xp[2]-Xp[1])/deexy
YNOD = Yp[1]+RN*(Yp[2]-Yp[1])/deexy

IYZ = get3Drayblox(XNOD, YNOD, ZNOD, xo, yo, ztop, slowness = NULL)




}

\keyword{misc}
