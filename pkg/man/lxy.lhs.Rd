\name{lxy.lhs}
\alias{lxy.lhs}
\title{Create a LoCoH-hullset object from a LoCoH-xy object}
\usage{
  lxy.lhs(lxy, id = NULL, s = 0, a = NULL, r = NULL,
    k = NULL, kmin = 0, anv.copy = TRUE,
    decimal.places = 1, offset.dups = 1,
    velocity.metrics = TRUE, ud = FALSE,
    iso.levels = 0.15 + 0:8 * 0.1, pbo.style = 3,
    beep = FALSE, status = TRUE, save.hulls = TRUE,
    save.enc.pts = TRUE)
}
\arguments{
  \item{lxy}{A \code{\link{LoCoH-xy}} object}

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

  \item{decimal.places}{Yhe number of decimal places for
  the k/r/a parameter value used when constructing the name
  of a hullset. Note this has no effect on hull creation or
  the precision of values used, only the name of the
  hullset.}

  \item{offset.dups}{A number of map units to randomly
  offset duplicate points. Set \code{offset.dups=0} to
  ignore duplicate points}

  \item{anv.copy}{Copy the ancillary variables data frame
  (if exists), T/F}

  \item{velocity.metrics}{Compute the velocity hull
  metrics}

  \item{ud}{Whether to also create the default utilization
  distributions (density isopleths), T/F}

  \item{iso.levels}{Isopleth levels (see also
  \code{\link{lhs.iso}}), numeric vector. Ignored if
  |code{ud=FALSE}.}

  \item{beep}{Beep when done. T/F.}

  \item{status}{Show messages. T/F.}
}
\value{
  A LoCoH-hullset object (class "locoh.lhs")
}
\description{
  Creates a LoCoH-hullset object (class "locoh.lhs")
  containing one or more sets of hulls.
}
\note{
  This is main function that creates a
  \code{\link{LoCoH-hullset}} object. Other functions allow
  you to do things with the these hulls, including
  computing additional hull metrics, constructing
  isopleths, directional routes, generating scatterplots,
  etc. Note that a nearest neighbors points must have
  already been identified and saved in the input
  \code{\link{LoCoH-lxy}} object.
}
\seealso{
  \code{\link{xyt.lxy}} for creating LoCoH-xy objects
  \code{\link{lhs.iso}} for adding isopleths
  \code{\link{lhs.ellipses.add}} for computing bounding
  ellipses \code{\link{lhs.visit.add}} for computing
  time-use metrics \code{\link{lhs.plot.scatter}} for
  creating scatterplots of hull metrics
  \code{\link{plot.locoh.lhs}} for plotting a LoCoH-hullset
}

