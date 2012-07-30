\name{PLOT.TOMOXSEC}
\alias{PLOT.TOMOXSEC}
\title{Plot a tomographic cross section}
\description{
  Plot a tomographic cross section that has been extracted from the
  model previously with TOMO3D.drive
}
\usage{
PLOT.TOMOXSEC(XZSEC, depth = c(-25, 0), COL = NULL, LIM = NULL, STA = NULL, ADD = FALSE)
}
\arguments{
  \item{XZSEC}{Cross section list}
  \item{depth}{Depth range}
  \item{COL}{color palette}
  \item{LIM}{limits for values in image}
  \item{STA}{stations to be projected}
  \item{ADD}{logical, TRUE=add to existing plot}
}

\value{
Graphical Side Effects
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{TOMOXSEC, XSEC.drive }
\examples{
data(HELMOD)

### after L = locator(2)
L=list()
L$x=c( 4.21883807095,23.99298268599)
L$y=c(15.8014536521,11.4951858659)

###  create the cross section:
XZ = TOMOXSEC(HELMOD, L$x[1], L$y[1], L$x[2],  L$y[2] , zmax=20, COL=TOMO.colors(100), PLOT=FALSE)


###  Now plot the cross section
PLOT.TOMOXSEC(XZ)

}
\keyword{hplot}
