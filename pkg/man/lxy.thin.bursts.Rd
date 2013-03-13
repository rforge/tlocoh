\name{lxy.thin.bursts}
\alias{lxy.thin.bursts}
\title{Thins out the 'bursts' in a GPS dataset}
\usage{
  lxy.thin.bursts(lxy, id = NULL, thresh = NULL,
    replace = c("mean", "median")[2], info.only = FALSE,
    status = TRUE, dt.int.round.to = 0.1,
    tau.diff.max = 0.02)
}
\arguments{
  \item{lxy}{A \code{\link{LoCoH-xy}} object}

  \item{id}{The id value(s) to be thinned}

  \item{thresh}{The threshhold for delta.t below which a
  pair of points is considered to be part of a burst,
  expressed either as a proportion of the median sampling
  frequency (0..1) or an absolute unit of time (in
  seconds)}

  \item{replace}{The burst replacement method ('mean' or
  'median')}

  \item{info.only}{Show information about number of bursts
  (only)}

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
  a \code{\link{LoCoH-xy}} object
}
\description{
  Thin out closely-timed bursts of locations
}
\note{
  This function processes 'bursts' of locations, where a
  'burst' is a series of locations captured close together
  in time. Each group of points in a burst is replaced with
  a single point. This of course presumes the burst of
  locations is an artifact of data collection and not
  desirable.

  Many GPS devices have a feature to save 'bursts' of
  points close together in time (relative to the dominant
  sampling frequency) The 'burst' feature should not be
  confused with point averaging, whereby a GPS device
  internally averages locations for a period of time (e.g.,
  2 minutes) but saves a single location.

  \code{thresh} is a value for the sampling interval for
  identifying which points should be considered part of a
  burst. \code{thresh} can be a proportion of the median
  sampling frequency (0..1) or an absolute unit of time (in
  seconds).

  To identify whether there are bursts in a
  \code{\link{LoCoH-xy}} dataset, and the sampling
  frequency of those bursts (i.e., the value

  The two replacement methods are /code{replace="mean"}, in
  which case a burst of locations is replaced by a single
  point consisting of the spatial and temporal average of
  the burst, or /code{replace="median"} in which case a
  burst of locations is replaced by the location at the
  temporal median of the burst. If /code{replace="mean"},
  then any ancillary variables for the 'new' points will be
  set to \code{NA}.

  Because this function deletes points, the nearest
  neighbor lookup table of the \code{\link{LoCoH-xy}}
  object (if any) will be deleted, and the parameters for
  random walk model will be recomputed.
}
\seealso{
  \code{\link{lxy.plot.freq}}, \code{\link{lxy.thin.freq}}
}

