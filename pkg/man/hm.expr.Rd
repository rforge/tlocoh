\name{hm.expr}
\alias{hm.expr}
\title{Hull metrics expressions}
\usage{
  hm.expr(names.only = TRUE, desc = names.only,
    print = names.only)
}
\arguments{
  \item{names.only}{Return only the names of hull metrics.
  T/F.}
}
\value{
  A named list with the following elements: \enumerate{
  \item ufat. User-friendly axis title \item desc.
  Description (used to build plot captions) \item def.
  Definition. If NULL, then the definition is the same as
  $desc \item ufipt. User-friendly isopleth plot title.
  Used when plotting isopleths constructed by sorting hulls
  on the metric. \item nhv. No hull value (the value to use
  for parent points that have no hulls, used to highlight
  the presence of points without hulls in histograms \item
  iso.dec. Sort hulls in descending order when used to
  create isopleths). T/F \item expr Expression object that
  when evaulated returns the hull metric values \item
  zero2na Whether 0 values should be colored as NA (i.e.,
  hidden) when classifying hull parent points, T/F \item
  discrete Whether the metric assumes discrete (integer)
  values, used in classifying hull parent points T/F \item
  auto.jiggle The maximum value of a uniform distribution
  used to apply a random offset to the metric when plotted
  on scatterplots to better see point density \item spao.x
  include the metric as a default for the x-axis when the
  'auto' option is used in \code{\link{scatter.plot.auto}}
  \item spao.y include the metric as a default for the
  y-axis when the 'auto' option is used in
  \code{\link{scatter.plot.auto}} \item req.metrics The
  underlying metric(s) that need to be saved in the hullset
  for successful computation. Character vector. This will
  usually simply be the name of the metric itself, but may
  be different in the case of derived metrics such as
  perimeter-area ratio. \item req.ap The name(s) of
  required auxillary variables that are needed to extract
  the metric. Character vector. \item req.ap.def A named
  list of default values of any required auxillary
  variables \item req.ap.subtitle A named list of
  expression objects (one for each hmap) that returns a
  string of hmap values that will be used in the subtitle
  of plots \item req.ap.desc An named list of expression
  objects (one for each hmap) that returns a string of hmap
  values that will be used in the description of plots
  \item fun The name of the function that creates the hull
  metric \item iso.hm2 Second hull metric used to break
  ties when creating isopleths (used mostly when the metric
  returns discrete values) \item iso.hm2.dec Sort second
  hull metric descending }
}
\description{
  Returns a list of expression objects that when evaluated
  in functions will return various hull metrics
}
\details{
  Hull metrics are used in many tlocoh functions. This
  function serves as a central source for expressions that
  return hull metrics. About Hull Metrics...
}
\seealso{
  \code{\link{lhs.ellipses.add}},
  \code{\link{lhs.visit.add}},
  \code{\link{lhs.revisit.add}}
}

