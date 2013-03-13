\name{lxy.proj.add}
\alias{lxy.proj.add}
\title{Add ancillary variable(s) to a LoCoH-xy object from a raster}
\usage{
  lxy.proj.add(lxy, proj4string = CRS(as.character(NA)))
}
\arguments{
  \item{lxy}{\link{LoCoH-xy} object}

  \item{proj4string}{An object of class "CRS", projection
  string}
}
\value{
  A \link{LoCoH-xy} object
}
\description{
  Add ancillary variable(s) to a LoCoH-xy object from a
  raster
}
\examples{
lxy <- lxy.proj.add(lxy, CRS("+proj=utm +south +zone=34"))
}
\seealso{
  \code{\link{CRS-class}}
}

