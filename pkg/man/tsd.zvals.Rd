\name{tsd.zvals}
\alias{tsd.zvals}
\title{Space-time transformation term for TSD}
\usage{
tsd.zvals(delta.t, sVal, type, d.bar = NULL, tau = NULL, vmax = NULL)
}
\arguments{
  \item{delta.t}{is the time difference in seconds}

  \item{sVal}{is the value of S}

  \item{type}{The type of space-time transformation: 'vamx'
  for the maximum velocity transformation, or 'dif' for
  diffusion}

  \item{d.bar}{is the median step length (for the entire
  dataset)}

  \item{tau}{is the median sampling interval (for the
  entire dataset)}

  \item{vmax}{The maximum velocity parameter (used only
  when \code{type='vmax'})}
}
\description{
Returns the values for the "time axis" of the TSD distance
metric
}

