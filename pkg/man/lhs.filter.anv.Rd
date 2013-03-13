\name{lhs.filter.anv}
\alias{lhs.filter.anv}
\title{Define subsets of hulls based on the location of the parent point in hull scatterplot space}
\usage{
  lhs.filter.anv(lhs, id = NULL, k = NULL, r = NULL,
    a = NULL, s = NULL, hs.names = NULL, label = NULL,
    col = NULL, anv.var = NULL, anv.val = NULL,
    status = TRUE)
}
\arguments{
  \item{lhs}{A LoCoH-hullset object}

  \item{id}{The name(s) of the individual(s) to include in
  the plot. Character vector or comma-delimited character.}

  \item{k}{The k value of hullsets to create isopleths for}

  \item{r}{The r value of hullsets to create isopleths for}

  \item{a}{The a value of hullsets to create isopleths for}

  \item{s}{The s value of hullsets to create isopleths for}

  \item{hs.names}{The name(s) of saved hullsets to create
  isopleths for objects have been saved), or a list of
  objects of class \code{locoh.hsp} (i.e., the return value
  of \code{\link{lhs.plot.scatter}}.}
}
\value{
  A list that defines subsets of hulls, with one element
  for each region in \code{hsp}. Each element is a list
  with three elements: \describe{ \item{idx}{indices of the
  hull parent points} \item{label}{the label for the
  subset} \item{col}{the color } }
}
\description{
  Define subsets of hulls based on the location of the
  parent point in hull scatterplot space
}
\details{
  This will return a list that defines subsets of hulls
  grouped according to which manually-digitized region in
  scatterplot space the hull parent point falls. This can
  be passed to several functions, including
  \code{\link{plot.locoh.lhs}} (future), and
  \code{\link{lhs.plot.scatter}}, to create plots of
  subsets of hulls.

  Note that this function can only return subsets for *one*
  hullset. This means that either \code{lhs} must contain a
  single hullset, or other parameters (e.g., \code{id},
  \code{hs.names}, \code{k}, \code{r}, and/or \code{a}) are
  passed to select one and only one of the hullsets in
  \code{lhs}.
}

