\name{HEL1D}
\alias{HEL1D}
\docType{data}
\title{One dimensional velocity model for Mt. St. Helens}
\description{
  1D velocity model used for earthquake locations at Mt. St. Helens
  in the 1980's
}
\usage{data(HEL1D)}
\format{
  The format is:
  List of 8
   \describe{
     \item{zp}{depths to tops of P-wave model, km}
     \item{vp}{P velocity in layers, km/s}
     \item{ep}{uncertainty in P velocity}
     \item{zs}{depths to tops of S-wave model, km}
     \item{vs}{S velocity in layers, km/s}
     \item{es}{uncertainty in S velocity}
     \item{name}{anme of file where model is stored}
     \item{descriptor}{text desription of model}
   }

}


\references{
  Lees, J. M.  (1992): The magma system of Mount St. Helens: Non-linear
  high resolution P-wave tomography,
  J. Volc. Geoth. Res., 53(1-4), 103-116.
 
}
\examples{
data(HEL1D)
## maybe str(HEL1D) ; plot(HEL1D) ...
}
\keyword{datasets}
