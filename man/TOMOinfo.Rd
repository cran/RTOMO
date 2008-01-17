\name{TOMOinfo}
\alias{TOMOinfo}
\title{Dump  tomogram information }
\description{Dump  tomogram information
}
\usage{
TOMOinfo(MOD, PLOT=FALSE)
}
\arguments{
  \item{MOD}{Model Structure }
   \item{PLOT}{logical, TRUE=plot boxplot }
  
}

\value{
 Side Effects. Shows velocity, tops, mean value and slowness
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}


\examples{
data(HELMOD)
TOMOinfo(HELMOD)
}
\keyword{misc}
