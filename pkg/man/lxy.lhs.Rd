\name{lxy.lhs}
\alias{lxy.lhs}
\title{Create a LoCoH-hullset object from a LoCoH-xy object}
\usage{
lxy.lhs(lxy, id = NULL, s = 0, a = NULL, r = NULL, k = NULL,
  kmin = 0, anv.copy = TRUE, decimal.places = 1, offset.dups = 1,
  velocity.metrics = TRUE, ud = NULL, iso.add = FALSE,
  iso.levels = c(0.1, 0.25, 0.5, 0.75, 0.95), pbo.style = 3, beep = FALSE,
  status = TRUE, save.hulls = TRUE, save.enc.pts = TRUE)
}
\arguments{
  \item{lxy}{A \link{LoCoH-xy} object}

  \item{id}{The id value(s) of \code{lxy} for which
  hullsets will be created. If \code{NULL} all ids will be
  used.}

  \item{s}{Value(s) for the s term in the
  time-scaled-distance equation for point-to-point
  distance. When \code{s=0} time is excluded and TSD is
  equivalent to Euclidean distance. Numeric vector or
  comma-separated string. Can also be 'all'}

  \item{a}{Value(s) for the adaptive method. Numeric vector
  or comma-separated string. \code{a} also be the results
  of the \code{\link{auto.a}} function, see notes.}

  \item{r}{Value(s) for the r method. Numeric vector or
  comma separate list of values.}

  \item{k}{Value(s) for the k method. Numeric vector or
  comma separate list of values.}

  \item{kmin}{A minimum number of nearest neighbors for
  each parent point regardless of the method used. Set
  \code{k=2} to ensure that every point is part of a hull.
  Integer.}

  \item{decimal.places}{The number of decimal places for
  the k/r/a parameter value used when constructing the name
  of a hullset. Note this has no effect on hull creation or
  the precision of values used, only the number of digits
  that appear in the hullset name.}

  \item{offset.dups}{A number of map units to randomly
  offset duplicate points. Set \code{offset.dups=0} to
  ignore duplicate points}

  \item{anv.copy}{Copy the ancillary variables data frame
  (if exists), T/F}

  \item{velocity.metrics}{Compute the velocity hull
  metrics}

  \item{ud}{Deprecated (no longer used). Use \code{iso.add}
  instead}

  \item{iso.add}{Whether to also create density isopleths,
  T/F}

  \item{iso.levels}{Isopleth levels (see also
  \code{\link{lhs.iso.add}}), numeric vector. Ignored if
  |code{iso.add=FALSE}.}

  \item{pbo.style}{Progress bar style (see pbapply
  package)}

  \item{beep}{Beep when done. T/F}

  \item{status}{Show messages. T/F}

  \item{save.hulls}{Whether to save the hulls. T/F}

  \item{save.enc.pts}{Whether to save the enclosed points.
  T/F}
}
\value{
An object of class \link{locoh.lhs}
}
\description{
Creates a LoCoH-hullset object (class "locoh.lhs")
containing one or more sets of hulls.
}
\note{
This function creates a \link{LoCoH-hullset} object from a
\link{LoCoH-xy} object. Other functions allow you to do
things with hullsets, including computing additional hull
metrics, constructing isopleths, directional routes,
generating scatterplots, exporting, etc. Note that before
you use this function, nearest neighbors must have already
been identified and saved in the input \link{LoCoH-xy}
object (see \code{\link{lxy.nn.add}}).

If \code{iso.add=TRUE}, after the hulls are created the
function will create density isopleths using the default
sort order (area for \emph{k-method}, number of enclosed
points for the \emph{a} and \emph{r-methods}). You can
control which isopleth levels are created using the
\code{iso.levels} argument.

When working with a large dataset where memory limts may
affect performance, you can choose to not save the hulls or
enclosed points by setting \code{save.hulls=FALSE} and
\code{save.enc.pts=FALSE}. See the vignette on working with
large datasets.
}
\seealso{
\code{\link{xyt.lxy}} for creating LoCoH-xy objects
\code{\link{lhs.iso.add}} for adding isopleths
\code{\link{lhs.ellipses.add}} for computing bounding
ellipses \code{\link{lhs.visit.add}} for computing time-use
metrics \code{\link{lhs.plot.scatter}} for creating
scatterplots of hull metrics \code{\link{plot.locoh.lhs}}
for plotting a LoCoH-hullset \code{\link{hulls}} for
extracting hulls as a SpatialPolygonsDataFrame
\code{\link{lhs.exp.shp}} to export hulls as shapefile
}

