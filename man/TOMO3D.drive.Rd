\name{TOMO3D.drive}
\alias{TOMO3D.drive}
\title{Interactive Exploration of 3D tomographic inversion}
\description{
  Interactive Exploration of 3D tomographic inversio
}
\usage{
TOMO3D.drive(MOD, COL = NULL, LIM = NULL, MAP = NULL, MAPLIM = NULL, STA = NULL, TOPO = NULL, STDLAB = c("DONE", "QUIT"))
}
\arguments{
  \item{MOD}{Model List }
  \item{COL}{color palette }
  \item{LIM}{ Value limits for colors}
  \item{MAP}{Map strcuture (GEOmap) }
  \item{MAPLIM}{Boundary for map limits}
  \item{STA}{Station File}
  \item{TOPO}{Topographic Structure}
  \item{STDLAB}{vector of menu items}
}
\details{
  Interactive display of tomographic model
}
\value{
 Graphical Side Effect
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{SHOWTOMO}
\examples{

data(HELMOD)
data(HELMAP)

TOMO3D.drive(HELMOD, MAP=HELMAP)

}
\keyword{misc}
