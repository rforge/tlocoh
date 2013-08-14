\name{lxy.proj.add}
\alias{lxy.proj.add}
\title{Specify the projection system for a LoCoH-xy object}
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
  Specify the projection system for a LoCoH-xy object
}
\details{
  This function will add projection information to a
  \link{LoCoH-xy} object. Note this does not reproject
  coordinates, it only adds or changes the projection
  information. Any existing projection information will be
  overwritten.
}
\examples{
# lxy <- lxy.proj.add(lxy, CRS("+proj=utm +south +zone=34"))
}
\seealso{
  \code{\link{CRS-class}} for arguments accepted by the
  \code{\link{CRS}} function.
}

