\name{SHOWTOMO}
\alias{SHOWTOMO}
\title{Show many layers of tomoggraphic model}
\description{
  Plots a row of layers for quick view of tomographic results with map overlay.
}
\usage{
SHOWTOMO(MOD, colmap = topo.colors(100), zlim = NULL, MAP = NULL, I = 1, J = 2, bkgr="white")
}
\arguments{
  \item{MOD}{ MODEL list}
  \item{colmap}{color palette }
  \item{zlim}{Limit }
  \item{MAP}{Map list (GEOmap) }
  \item{I}{first lay index }
  \item{J}{last layer index}
  \item{bkgr}{background color}
  
}

\value{
 Graphical Side effects
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{FANCY.TOMO}
\examples{
data(HELMOD)
data(HELMAP)
SHOWTOMO(HELMOD, MAP=HELMAP,  bkgr="beige", I=1, J=15)

}
\keyword{hplot}
