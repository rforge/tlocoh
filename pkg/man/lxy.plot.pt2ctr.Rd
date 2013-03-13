\name{lxy.plot.pt2ctr}
\alias{lxy.plot.pt2ctr}
\title{Plot distance to centroid over time}
\usage{
  lxy.plot.pt2ctr(lxy, id = NULL, idx = NULL, vline = NULL,
    vline.trunc.units.to = c("secs", "mins", "hours", "days")[4],
    figs.per.page = NULL, ...)
}
\arguments{
  \item{lxy}{A \code{\link{locoh-xy}} object}

  \item{idx}{An optional vector of indices of points that
  the plot will be restricted to. See notes}

  \item{vline}{An interval of time (in seconds) for the
  vertical grid lines}

  \item{vline.trunc.units.to}{A character value: "secs",
  "mins", "hours", or "days". Determines where the first
  vertical grid lines will appear}

  \item{...}{Additional parameters that will be passed to
  the \code{\link{plot}} function}
}
\description{
  Produces a plot of the distance of each point to the
  centroid over time to see 'natural' periodicities in the
  data
}
\note{
  The purpose of this graph is to help see the 'natural'
  movement cycles in a dataset, when no \emph{a priori}
  information exists that helps identify the time scale of
  interest. In T-locoh, defining the timescale of interest
  helps to select a value for \code{s}.

  To 'zoom in' to a section of the timeline, pass a value
  for \code{idx}. For example, if there are 5000 points in
  the dataset

  If a value is passed for vline, the first vertical line
  will coincide with a multiple of
  \code{vline.trunc.units.to}. For example, if
  \code{vline=3600*24}(24 hours), and
  \code{vline.trunc.units.to="days"}, the first vertical
  line will be the beginning of a day regardless of when
  the point series started. This can be helpful for seeing
  cycles of a specific frequency.
}
\seealso{
  \code{\link{lxy.sfinder}}
}

