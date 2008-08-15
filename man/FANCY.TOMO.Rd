\name{FANCY.TOMO}
\alias{FANCY.TOMO}
\title{Show horizontal tomographic section layer by layer}
\description{
  Show horizontal tomographic section layer by layer
}
\usage{
FANCY.TOMO(MOD, i, COL = NULL, LIM = NULL, MAP = NULL, MAPLIM = NULL,
STA = NULL, staparams = list(col = "green", pch = 6, cex = 0.8, name =
FALSE), PTS = NULL, ptsparams = list(col = "green", pch = 6, cex = 0.8,
name = FALSE), TIT = "Layer", mainTIT="Layer", UNITS = "", bkgr = "DarkSlateGray4")
}
\arguments{
  \item{MOD}{MODEL list}
  \item{i}{layer number}
  \item{COL}{color palette}
  \item{LIM}{Limit}
  \item{MAP}{Map list (GEOmap) }
  \item{MAPLIM}{Geographic limits on map}
  \item{STA}{station list (name, lat lon z)}
  \item{staparams}{graphical parameters for plotting stations}
  \item{PTS}{points list, e.e. earthquakes (lat lon z) }
  \item{ptsparams}{graphical parameters for plotting points }
  \item{TIT}{Title}
  \item{mainTIT}{Main Title}
  \item{UNITS}{units}
  \item{bkgr}{background color for NA in image}
}

\value{
  Graphical Side effects
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{SHOWTOMO}
\examples{

data(HELMOD)
data(HELMAP)

for(i in 1:length(HELMOD$MOD))
{
##  i = 4
FANCY.TOMO(HELMOD, i, MAP=HELMAP, bkgr="beige")

locator()


}

}
\keyword{misc}
