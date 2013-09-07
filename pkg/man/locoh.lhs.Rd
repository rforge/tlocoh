\name{locoh.lhs}
\alias{LoCoH-hullset}
\alias{locoh.lhs}
\alias{locoh.lhs-class}
\title{Class for a hullsets}
\description{
  Class for a collection of hullsets and associated objects
  including hull metrics and isopleths
}
\details{
  An object of class locoh.lhs is a list (S3) containing
  one or more sets of hulls (local MCPs) for a set of
  points. One hull is saved for each point if the method
  used to create the hulls returned at least two nearest
  neighbors for each point, otherwise the number of hulls
  might be less than the number of points. In addition to
  hulls (saved as a SpatialPolygonsDataFrame), a locoh.lhs
  object may also contain:

  \itemize{ \item the original points and their attribute
  table \item an index of the points a) used to construct
  the hull and b) enclosed by each hull \item hull metrics
  \item isopleths (progressive unions of hulls) as both
  vector (SpatialPolygonsDataFrame) and raster (utilzation
  distribution) formats.  \item saved parameters for
  scatterplots of hull metrics.  }

  In code examples, objects of class locoh.lhs are often
  named 'lhs'. Functions that are designed to work on
  LoCoH-hullset objects typically start with 'lhs'. For a
  description of the data structure, see the vignette
  'hulls and hull metrics'.
}
\seealso{
  \code{\link{lxy.lhs}}, \code{\link{summary.locoh.lhs}},
  \code{\link{plot.locoh.lhs}}, \code{\link{lhs.merge}},
  \code{\link{lhs.iso.add}}, \code{\link{lhs.save}},
  \code{\link{lhs.exp.shp}}
}

