\name{RTOMO-package}
\alias{RTOMO-package}
\alias{RTOMO}
\docType{package}
\title{
Plot and Interact with Tomographic Images
}
\description{
  Created mainly for use with seismic tomography, this program
  plots tomographic images, and allows one to interact and query
  three-dimensional tomographic models.
  Vertical cross-sectional cuts can be extracted by mouse click.
  Geographic information can be added easily.
}
\details{
\tabular{ll}{
Package: \tab RTOMO\cr
Type: \tab Package\cr
Version: \tab 1.0-9\cr
Date: \tab 2012-07-30-15\cr
License: \tab GPL\cr
}
Visualization for seismic tomography, includes:
\describe{
\item{TOMO3D.drive}{Interactive tomography exploration}
\item{SHOWTOMO}{show tomographic model in multi-panel display}
\item{FANCY.TOMO}{Tomographic plot of one layer}
\item{pltomo}{plot one horizontal slice of tomogram}
\item{XSEC.drive}{Interactive plot of vertical cross section}
\item{PLOT.TOMOXSEC}{plot Cross section of tomographic model}
\item{TOMOXSEC}{Vertical Cross section through model}
}
Model I/O:
\describe{
\item{GXMA3D}{Get 3D perturbation model in geotouch format}
\item{GXMA3DV}{Get 3D velocity model in geotouch format}
\item{makeMOD}{Make 3D synthetic model}
}

Utilities:
\describe{
\item{tomo.colors}{color palette for tomography}
\item{TOMOinfo}{Information about the tomographic layers}

\item{MOD2VEC}{Convert 3D model to single long vector}
\item{VEC2MOD}{Convert single long vector to 3D model}

\item{get2Drayblox}{get 2D ray blocks}
\item{get3Drayblox}{get 3D ray blocks}
}

}
\author{
Jonathan M. Lees
Maintainer: Jonathan M. Lees <jonathan.lees@unc.edu>
}
\references{
 J. M.  Lees.  Geotouch: Software for three and four dimensional
  GIS in the earth sciences.   \emph{Computers and  Geosciences} ,
  26(7):751--761, 2000.


}

\keyword{ package }
\seealso{
RSEIS
}
\examples{
data(HELMOD)
data(HELMAP)

TOMO3D.drive(HELMOD, MAP=HELMAP)


}
