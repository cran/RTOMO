\name{TOMOXSEC}
\alias{TOMOXSEC}
\title{Plot Tomographic Cross Section}
\description{
  Plot Tomographic Cross Section (non-interactive)
}
\usage{
TOMOXSEC(MOD, x1, y1, x2, y2, zmax = 100, depth = c(-25, 0), COL = rainbow(100), LIM = NULL, STA = NULL, PLOT = TRUE)
}
\arguments{
  \item{MOD}{Model List}
  \item{x1}{x-coordinate for point 1}
  \item{y1}{y-coordinate for point 1}
  \item{x2}{x-coordinate for point 2}
  \item{y2}{y-coordinate for point 2}
  \item{zmax}{maximum depth}  
  \item{depth}{depth for scale}
  \item{COL}{color palette}
  \item{LIM}{Value limits for colors}                                      
  \item{STA}{station list}
  \item{PLOT}{logical, TRUE=plot}
}

\value{
  \item{xz}{list of a cross section}
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{PLOT.TOMOXSEC, FANCY.TOMO}
\examples{


data(HELMOD)

### after L = locator(2)
L=list()
L$x=c( 4.21883807095,23.99298268599)
L$y=c(15.8014536521,11.4951858659)

XZ = TOMOXSEC(HELMOD, L$x[1], L$y[1], L$x[2],  L$y[2] , zmax=20, COL=tomo.colors(100), PLOT=TRUE)


}
\keyword{hplot}
