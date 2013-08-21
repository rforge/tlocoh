\name{lhs.iso.add}
\alias{lhs.iso.add}
\title{Compute isopleths}
\usage{
  lhs.iso.add(lhs, id = NULL, k = NULL, r = NULL, a = NULL,
    s = NULL, hs.names = NULL,
    sort.metric = c("auto", hm.expr(names.only = T, desc = F, print = F))[1],
    iso.levels = c(0.1, 0.25, 0.5, 0.75, 0.95),
    iso.method = c("pt.quantiles", "hm.vals")[1],
    iso.cap.method = c(">=", "<=")[1],
    scale.iso.levels.to.hm.vals = (iso.method == "hm.vals" && max(iso.levels <= 1)),
    subset.metric = NULL, subset.vals = NULL,
    allow.gpc = TRUE, beep = FALSE, status = TRUE, ...)
}
\arguments{
  \item{lhs}{A LoCoH-hullset object}

  \item{id}{The id(s) of the hullsets to create isopleths
  for}

  \item{k}{The k value of hullsets to create isopleths for}

  \item{r}{The r value of hullsets to create isopleths for}

  \item{a}{The a value of hullsets to create isopleths for}

  \item{s}{The s value of hullsets to create isopleths for}

  \item{hs.names}{The name(s) of saved hullsets to create
  isopleths for}

  \item{sort.metric}{The name of a hull metric that will be
  used to sort the hulls prior to merging into isopleths}

  \item{iso.method}{The method used to define isopleths.
  Default is "pt.quantiles" which defines isopleths as
  containing a quantile of points (e.g., the 0.1th isopleth
  contains 10\% of the points). Can also be "hm.vals", in
  which case the isopleth level represents not a proportion
  of points enclosed but a value of the hull metric (e.g.,
  the 0.1 isopleth contains hulls whose hull metric is <=
  0.1)}

  \item{iso.levels}{A numeric vector of the levels of the
  isopleths. See details.}

  \item{iso.cap.method}{A character object specifying how
  isopleths defined by proportions of enclosed points will
  be 'capped'. Ignored when \code{iso.method="hm.vals"}.
  See details.}

  \item{scale.iso.levels.to.hm.vals}{Whether to linearly
  scale iso.levels (presumed to be 0..1) to the minimum and
  maximum of the sort.metric value. This is used in
  conjunction iso.method='hm.vals', to produce isopleths as
  the aggregation of hulls whose sort.metric value is >=
  i\% of the maximum, where i is the isopleth level. T/F}

  \item{subset.metric}{The name of a hull metric that will
  be used to create subsets of isopleths}

  \item{subset.vals}{A two-column data frame (or matrix)
  containing the lower and upper bounds of the hull metric
  in subset.metric, from which subsets of hulls will be
  extract for separate isopleth construction. Ignored if
  subset.metric is not provided. Can also be a character
  string in the form of "equal intervals, n", or
  "quantitles, n" where n is the number of strata desired
  (shorthand versions of these commands are "ei,n" and
  "q,n" where n is a number). The script will compute the
  break points either spread equally between the minimum
  and maximum subset metric value (equal interval) or so an
  equal number of hulls is in each strata}

  \item{allow.gpc}{Allow functions from the gpclib package
  if the functions from rgeos fail. See details. T/F}

  \item{beep}{Beep when done. T/F}

  \item{status}{Show status messages. T/F}

  \item{...}{Additional auxillary parameters for the hull
  sort metric}
}
\value{
  A \link{LoCoH-hullset} object
}
\description{
  Adds isopleth(s) to a LoCoH-hullset object
}
\details{
  This function creates isopleth(s) for a LoCoH-hullset
  object. This involves sorting hulls by one of the hull
  metrics, and then cummulative unioning them together
  until you reach a certain level at which point the union
  is saved as an isopleth. By default, hulls will be sorted
  according to the area (for k-method) or number of
  enclosed points (r- and a-methods), producing density
  isopleths (i.e., utilization distributions).

  When \code{iso.method=="pt.quantiles"} (the default),
  isopleths will be defined by the proportion of points
  they enclose (given in \code{iso.levels}). Thus for
  example the 50% isopleth contains 50% of the total
  points. The default isopleth levels are 0.15, 0.25, ...,
  0.95. To get an isopleth that encloses all points in the
  dataset, \code{iso.levels} must include '1'.

  When \code{iso.method=="hm.vals"}, isopleths will be
  defined by the value of the hull metrics (provided in
  \code{iso.levels}). So if for example
  \code{sort.metric="scg.nn.mean"} (average speed of all
  points identified as nearest neighbors), and
  \code{scg.nn.mean} varied from 0.01 to 3.2, the 0.6
  isopleth would be the union of all hulls whose average
  nearest neighbor speed was 0.6 or less. If you would like
  hulls to be identified by hull metric values, but don't
  know the range of hull metric values, you can set
  isopleth levels between 0..1 and set
  \code{scale.iso.levels.to.hm.vals=TRUE}.

  By default, isopleths defined by quantiles of enclosed
  points are 'capped' as the smallest number of
  progressively unioned hulls that enclose a number of
  points equal to or greater than the isopleth level. For
  example, if there are 1000 points in the dataset, the 50%
  isopleth would be the smallest union of hulls that
  encloses 500 or more points. Alternately, when
  \code{iso.cap.method = "<-"}, the isopleth will be the
  largest number of hulls which enclose up to but not more
  than the isopleth level. The actual number of points
  enclosed by each isopleth is included in the data table
  for the isopleth (which you can view by setting
  \code{iso.details=TRUE} when calling
  \code{\link{summary.locoh.lhs}}).

  The hull metric used for sorting must already have been
  computed. Several hull metrics (e.g., hull area, number
  of enclosed points) are 'automatically' computed when a
  hullset is initially generated (see
  \code{\link{lxy.lhs}}). Other hull metrics must be
  created separately with functions such as
  \code{\link{lhs.ellipses.add}} and
  \code{\link{lhs.visit.add}}. You can use the
  \code{\link{summary.locoh.lhs}} function to see which
  isopleths have been saved in a LoCoH-hullset object.

  Hulls are unioned using the gUnion and gUnaryUnion
  functions from the rgeos package, which in general is
  very fast. These functions occasionally fail when lines
  are too close together or there are other topological
  conditions. This is not a problem with the data, but an
  inherent limitation of the algorithms / processing. If
  \code{allow.gpc=TRUE}, functions from the gpclib package
  will be used as backup.
}
\examples{
# Create 0.5 and 0.95 isopleths. By not specifying the sort.metric, density isopleths will be created
# by default, with hulls sorted by area (k-method) or number of enclosed points (r and a method)
# lhs <- lhs.iso.add(lhs, iso.levels=c(0.5, 0.95))

# Compute hull metrics for a 24 hour inter-visit gap, then produce isopleths with hulls sorted by
# the number of separate visits (e.g., visitation)
# lhs <- lhs.visit.add(lhs, ivg=3600.24)
# lhs <- lhs.iso.add(lhs, sort.metric="nsv", ivg=3600*24)
}

