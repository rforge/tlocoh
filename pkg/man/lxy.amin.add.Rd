\name{lxy.amin.add}
\alias{lxy.amin.add}
\title{Finds the value of a such that p percent of points are a nearest neighbor for at least one hull}
\usage{
  lxy.amin.add(lxy, id = NULL, s = NULL, ptp = 0.98,
    nnn = 2, prec = NULL, max.iter = 20, status = TRUE)
}
\arguments{
  \item{lxy}{A \link{LoCoH-xy} object}

  \item{id}{The id value(s) to analyze. If \code{NULL} all
  ids will be used.}

  \item{s}{Value(s) for the s term in the
  time-scaled-distance equation for point-to-point
  distance}

  \item{ptp}{The proportion of total points that should be
  a nearest neighbor for at least one hull (0..1]}

  \item{nnn}{The minimum number of nearest neighbors each
  point should have (can pass a vector of several values)}

  \item{prec}{A numeric value in map units to which the
  value of 'a' will be found. If \code{NULL}, will default
  to one-half of the median step legnth of the entire
  dataset}

  \item{max.iter}{The maximum number of iterations to try
  to get within \code{prec} of the minimum value}

  \item{status}{Show messages, T/F}
}
\description{
  Finds the value of a such that p percent of points are a
  nearest neighbor for at least one hull
}
\details{
  This function finds the value of 'a' (within a specified
  threshhold \code{prec}) such that the proportion
  \code{ptp} of all points will be a nearest neighbor for
  at least one hull. This value of 'a' is intended to be a
  reasonable lower bound for a home range that includes as
  many points as desired, but minimizes areas where the
  individual was not observed. This assumes that duplicate
  points will be offset by a random amount when creating
  hullset(s).

  Note that the value of 'a' such that \code{ptp} of points
  are nearest neighbors does not mean that \code{ptp}
  points are enclosed. Points can be enclosed by hulls that
  are not a nearest neighbor of any hull parent point.
}

