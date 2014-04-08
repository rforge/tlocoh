\name{secs.fmt}
\alias{secs.fmt}
\title{Format a number of seconds as minutes, hours or days}
\usage{
secs.fmt(secsVal, round.pos = 1)
}
\arguments{
  \item{secsVal}{a time value (in seconds)}

  \item{round.pos}{The number of decimal places in the
  formatted string}
}
\value{
a character string
}
\description{
Returns a formated version of a time value
}
\note{
This function only works with a single value. To format a
multiple time values stored in a vector, use sapply or
lapply

Based on the value of secsVal, the formatted version will
be converted to days, hours, minutes, or seconds
}

