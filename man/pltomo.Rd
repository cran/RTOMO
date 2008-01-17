\name{pltomo}
\alias{pltomo}
\title{plot a layer in 3D tomogram}
\description{
  Simple plot of a layer in 3D tomogram
}
\usage{
pltomo(x,y,MOD,i, colmap=rainbow(100), zlim=NULL, bkgr="DarkSlateGray4", ...)
}
\arguments{
  \item{x}{x nodes}
  \item{y}{y nodes }
  \item{MOD}{Model Structure}
  \item{i}{layer to plot}
  \item{colmap}{color palette}
  \item{zlim}{vector (v1, v2) limit of z values}
  \item{bkgr}{background color for NA values}
  \item{\dots}{graphical parameters from par()}
}
\details{
  Does not set the projection, does not add any markup - all this does
  is start the plotting set up and puts the image on the plot.
}
\value{
 Graphical Side effects
}\references{}
\author{Jonathan M. Lees<jonathan.lees@unc.edu>}
\note{
This routine is used by SHOWTOMO
}
\seealso{FANCY.TOMO, SHOWTOMO, GXMA3DV}
\examples{

data(HELMOD)
 pltomo(HELMOD$x,HELMOD$y,HELMOD$MOD,5, col=tomo.colors(100), zlim=NULL,  bkgr="white", axes=FALSE, ann=FALSE )
      


}
\keyword{misc}
