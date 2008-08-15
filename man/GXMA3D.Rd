\name{GXMA3D}
\alias{GXMA3D}
\alias{GXMA3DV}

\title{Read a geotouch image file}
\description{Read a geotouch image file
}
\usage{
GXMA3D(name)
}
\arguments{
  \item{name}{file name}
}

\value{
  \item{MOD}{Model Structure}
  
  
}
\details{
  \describe{
    \item{name}{Model Structure}
    \item{A}{location information list: lat lon nx ny nz dx dy skip}
    \item{D}{vector, tops of layers}
    \item{V}{vector, velocity of layers}
    \item{MOD}{3D MODEL list}
    \item{x}{x nodes}
    \item{y}{y nodes}
  }
  

  
}



\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{FANCY.TOMO}

\examples{

\dontrun{
HELMOD = GXMA3DV("H4vel.xmco")
}
}


\keyword{misc}
