\name{lxy.repair}
\alias{lxy.repair}
\title{Repair a LoCoH-xy object}
\usage{
  lxy.repair(lxy, fix.dup.ptid = FALSE, fix.anv = FALSE,
    dt.int.round.to = 0.1, tau.diff.max = 0.02)
}
\arguments{
  \item{lxy}{A LoCoH-xy object}

  \item{dt.int.round.to}{The proportion of the median
  sampling frequency that time intervals will be rounded to
  when computing the frequency table of sampling intervals
  (no change is made to the time stamps)}

  \item{tau.diff.max}{The maximum deviation from tau (the
  median delta.t of the entire dataset), expressed as a
  proportion of tau, that time difference between two
  points must fall for the distance between those two
  points to be included in the calculation of the median
  step length}
}
\value{
  A LoCoH-xy object
}
\description{
  Recreates a LoCoH-xy object
}
\details{
  This will return a new lxy object containing the original
  xys, dt, and ptid from lxy. All other items and
  attributes (including the comment) will be recreated or
  set to NULL

  \code{tau.diff.max} exists to eliminate the inclusion of
  temporal outliers in the computation of the median step
  length. The time difference betwen points must be withint
  \code{tau.diff.max} of tau for that pair of points to be
  included in step length calculation.
}
\seealso{
  \code{\link{lxy.id.new}}
}

