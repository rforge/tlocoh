% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lhs.dr.add.R
\name{lhs.dr.add}
\alias{lhs.dr.add}
\title{Define directional routes}
\usage{
lhs.dr.add(lhs, metric = c("ecc", "par")[2], thresh.val = 0.95,
  thresh.type = c("q", "v")[1], smooth = 1, status = TRUE,
  show.elong.hist = FALSE)
}
\arguments{
\item{lhs}{A LoCoH-hullset object}

\item{metric}{The name of the hull metric used as a proxy for directionality}

\item{thresh.val}{The threshhold above which a hull is considered part of a directional route}

\item{thresh.type}{The type of thresshold used. If \code{'q'} for quantile, then \code{thresh.val} is taken to be a percentile of the 
full range of the directionality metric values (i.e., 0  < \code{thresh.val} < 1). If \code{'v'}, \code{thresh.val} is taken to be an actual value of the threshhold metric.}

\item{smooth}{The amount of temporal smoothing applied, expressed as the number of points on either side (temporally) of the parent 
point whose average of the directionality metric is used for determining if the hull constructed around the parent point should be considered a part of a directional route.
For no smoothing, set \code{smooth = 0}.}

\item{status}{Display status messages}

\item{show.elong.hist}{Whether to display histogram(s) of the distribution of the directionality metric before and after smoothing. T/F}
}
\value{
A LoCoH-hullset object
}
\description{
Defines 'directional routes' by identifying parent points that are 1) temporally contiguous and 2) have hulls in the top n% of elongation values
}
\note{
Directional routes are line segments defined by connecting temporally contiguous hull parent-points that are in the top N percent of hull elongation.
Hull elongation is proxied by one of two hull metrics: perimeter:area ratio ('par') or the eccentricity of the bounding ellipse ('ecc'). Hull perimeter:area ratios
are automatically computed when a hullset is created; bounding ellipses must be computed separately using \code{\link{lhs.ellipses.add}}. 
Once computed, you can plot directional routes by passing \code{dr=TRUE} to \code{\link{plot.locoh.lhs}}.
}
