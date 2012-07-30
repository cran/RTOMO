\name{XSEC.drive}
\alias{XSEC.drive}
\title{Interactive Cross Section View}
\description{
  Plot a cross section of a tomogram and interact
}
\usage{
XSEC.drive(MOD, x1, y1, x2, y2, zmax = 100,
COL = TOMO.colors(100), LIM = NULL, STA = NULL,
 TOP =NULL , STDLAB = c("DONE", "QUIT"))
}
\arguments{
  \item{MOD}{Model List}
  \item{x1}{x-coordinate for point 1}
  \item{y1}{y-coordinate for point 1}
  \item{x2}{x-coordinate for point 2}
  \item{y2}{y-coordinate for point 2}
  \item{zmax}{maximum depth}
  \item{COL}{color}
  \item{LIM}{Limits for colors}
  \item{STA}{stations}
  \item{TOP}{Topography}
  \item{STDLAB}{labels}
}

\value{
Graphical Side Effects
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{PLOT.TOMOXSEC, TOMOXSEC, FANCY.TOMO}

\keyword{misc}
