\name{VEC2MOD}
\alias{VEC2MOD}
\alias{MOD2VEC}
\title{Convert representation of a 3D models}
\description{
  Convert a single vector representation of a 3D model to a list,
  or a list to a single vector.
}
\usage{
VEC2MOD(VEC)
MOD2VEC(MOD)
}
\arguments{
  \item{VEC}{vector with attributes x,y,D describing the coordinates}
  \item{MOD}{List model}
}
\details{
  The two functions are used to convert models for different uses.
}
\value{
  \item{MOD}{List model}
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{GXMA3D}
\examples{

data(HELMOD)

names(HELMOD)

VV = MOD2VEC(HELMOD)

NMOD = VEC2MOD(VV)

}
\keyword{misc}
