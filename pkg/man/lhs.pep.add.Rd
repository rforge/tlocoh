\name{lhs.pep.add}
\alias{lhs.pep.add}
\title{Add hull metrics for proportion of enclosed points by each hull for an ancillary variable}
\usage{
  lhs.pep.add(lhs, pep.var, pep.val = NULL, npep = TRUE,
    status = TRUE)
}
\arguments{
  \item{lhs}{A LoCoH-hullset object}

  \item{pep.var}{Name(s) of ancillary variable(s) saved
  with the LoCoH-hullset.}

  \item{pep.val}{Vector of value(s) for which the
  percentage of enclosed points will be calculated. If
  NULL, all unique values of pep.var will be used.}

  \item{npep}{Whether to compute the normalized proportion
  of enclosed points hull metric (normalized by the
  proportion of points in the entire dataset), T/F}

  \item{status}{Show messages, T/F}
}
\value{
  A LoCoH-hullset object
}
\description{
  Computes proportion of enclosed points of a second set of
  points
}
\note{
  Normally \code{pep.var} should be a factor or character
  vector. To see which ancillary variables are saved in a
  LoCoH-hullset use \code{\link{summary.locoh.lhs}}
}

