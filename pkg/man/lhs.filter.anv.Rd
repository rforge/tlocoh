\name{lhs.filter.anv}
\alias{lhs.filter.anv}
\title{Define subsets of hulls based on an ancillary variable}
\usage{
  lhs.filter.anv(lhs, id = NULL, k = NULL, r = NULL,
    a = NULL, s = NULL, hs.names = NULL, anv.var = NULL,
    anv.val = NULL, label = NULL, col = NULL,
    status = TRUE)
}
\arguments{
  \item{lhs}{A \code{\link{LoCoH-hullset}} object}

  \item{id}{The name(s) of the individual(s) to include.
  Character vector or comma-delimited character.}

  \item{k}{The k value of hullsets to include}

  \item{r}{The r value of hullsets to include}

  \item{a}{The a value of hullsets to include}

  \item{s}{The s value of hullsets to include}

  \item{hs.names}{The name(s) of saved hullsets to include}

  \item{anv.var}{The name of a single ancillary variable in
  the LoCoH-hullset object}

  \item{anv.val}{The value(s) of the ancillary variable
  that define each group}

  \item{label}{Character vector for the labels for each
  subset. If omitted the value of the ancillary variable
  will be used}

  \item{col}{Vector of color values (one per group). If
  omitted colors drawn from a rainbow pallete will be used}

  \item{status}{Show status messages. T/F}
}
\value{
  A list that defines subsets of hulls, with one element
  for each region in \code{hsp}. Each element is a list
  with three elements: \describe{ \item{idx}{indices of the
  hull parent points} \item{label}{the label for the
  subset} \item{col}{the color } }
}
\description{
  Define subsets of hulls based on an ancillary variable
}
\details{
  This will return a list that defines subsets of hulls
  based on the hull parent point's value of an ancillary
  variable. This can be passed to several functions,
  including \code{\link{plot.locoh.lhs}} (future), and
  \code{\link{lhs.plot.scatter}}, to create plots of
  subsets of hulls.

  \code{anv.val} is a vector of values of \code{anv.var}
  that will be used to define the group(s). For a hull to
  be included in a group, its value of \code{anv.var} must
  equal one of the values in \code{anv.val}. If
  \code{anv.val} is omitted, one group will be created for
  each unique value of \code{anv.var}. Defining groups
  based on a range of values (lower and upper limits) is
  not yet supported, but you could create a new ancillary
  variable that classifies ranges of values into discrete
  groups.

  Note that this function can only return subsets for *one*
  hullset. This means that either \code{lhs} must contain a
  single hullset, or other parameters (e.g., \code{id},
  \code{hs.names}, \code{k}, \code{r}, and/or \code{a}) are
  passed to select one and only one of the hullsets in
  \code{lhs}.
}

