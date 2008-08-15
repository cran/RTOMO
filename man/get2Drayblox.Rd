\name{get2Drayblox}
\alias{get2Drayblox}

\title{Get 2D block from model description and ray path}
\description{
  Uses x-y coordinates of points to determine the indecies and lengths
  of rays penetrating the model.
}
\usage{
get2Drayblox(x1, y1, x2, y2, xo, yo, NODES = FALSE, PLOT = FALSE)

}

\arguments{
  \item{x1}{x coordinate of starting block}
  \item{y1}{y coordinate of starting block}
  \item{x2}{x coordinate of ending block}
  \item{y2}{y coordinate of ending block}
  \item{xo}{x block divisions}
  \item{yo}{y block divisions}
  \item{NODES}{logical, whether xo, yo ae nodes or edges}
  \item{PLOT}{logical, TRUE=plot ray}
}

\value{
  \item{ix}{x index}
  \item{iy}{y index}
  \item{iz}{layer index}
  \item{lengs}{length in each block}
  \item{mids}{midpoints of sections}
  \item{nodes}{nodes of a 2D vector representation of layer}
   \item{LX}{x-divisions}
  \item{LY}{y-divisions}
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}
\note{ 
}
\seealso{get3Drayblox}
\examples{

xo = seq(from=0.5, by=1, length=100)
yo = seq(from=0.5, by=1, length=100)

IYZ = get2Drayblox(10, 4, 64, 50, xo, yo , NODES=FALSE, PLOT=FALSE)


}

\keyword{misc}
