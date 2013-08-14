\name{lhs.rast.add}
\alias{lhs.rast.add}
\title{Convert isopleths to rasters}
\usage{
  lhs.rast.add(lhs, id = NULL, k = NULL, r = NULL,
    a = NULL, s = NULL, hs.names = NULL,
    sort.metric = c("auto", hm.expr(names.only = T, desc = F, print = F))[1],
    iso.method = c("pt.quantiles", "hm.vals")[1],
    cell.size = NULL, ll.round = TRUE, status = TRUE)
}
\arguments{
  \item{lhs}{A \link{LoCoH-hullset} object}

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

  \item{status}{Show status messages. T/F}
}
\value{
  A LoCoH-hullset object
}
\description{
  Adds rasterized version of isopleth(s) to a LoCoH-hullset
  object
}
\details{
  This will take exising isopleths and create raster
  versions of them.
}
\examples{
\dontrun{
lhs <- lhs.iso.add(lhs)
lhs <- lhs.rast.add(lhs)
}
}
\seealso{
  \code{\link{lhs.iso.add}}
}

