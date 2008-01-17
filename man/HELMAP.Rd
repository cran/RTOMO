\name{HELMAP}
\alias{HELMAP}
\docType{data}
\title{Geographic map of Mt. St. Helens Summit region}
\description{
 Geographic map of Mt. St. Helens Summit region. Lnes showing water shed
 and outline of tree line prior to 1980 eruption.
}
\usage{data(HELMAP)}
\format{
  \item{STROKES}{list of meta data for map}
   \item{nam}{name of stroke}
   \item{num}{number of points}
   \item{index}{starting index}
   \item{col}{color}
   \item{style}{style, 1,2,3}
   \item{code}{geographic code}
   \item{LAT1}{Lower left latitude }
   \item{LAT2}{upper right latitude }
   \item{LON1}{Lower left  longitude}
   \item{LON2}{upper right longitude}
\item{POINTS}{points structure}
   \item{lat}{latitudes}
   \item{lon}{longitudes}
\item{PROJ}{projection list}
   \item{type}{type of projection, 2=UTM}
   \item{LAT0}{origin latitude}
   \item{LON0}{origin longitude }
   \item{LAT1}{other parameters that depend on projection}
   \item{LAT2}{other parameters that depend on projection}
   \item{LATS}{other parameters that depend on projection}
      \item{S}{other parameters that depend on projection}
      \item{N}{other parameters that depend on projection}
   \item{LONS}{other parameters that depend on projection}
      \item{E}{other parameters that depend on projection}
      \item{W}{other parameters that depend on projection}
   \item{DLAT}{other parameters that depend on projection}
   \item{DLON}{other parameters that depend on projection}
   \item{FE}{false easting}
   \item{FN}{false northing}
   \item{name}{name of projection}

}

\references{
   Lees, J. M.  (1992): The magma system of Mount St. Helens: Non-linear
  high resolution P-wave tomography,
  J. Volc. Geoth. Res., 53(1-4), 103-116.
 
}
\examples{
data(HELMAP)
## maybe str(HELMAP) ; plot(HELMAP) ...
}
\keyword{datasets}
