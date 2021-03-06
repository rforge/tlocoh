% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lhs.visit.add.R
\name{lhs.visit.add}
\alias{lhs.visit.add}
\title{Add time use hull metrics to a LoCoH-hullset object}
\usage{
lhs.visit.add(lhs, ivg = NULL, check_samp_int = TRUE, status = TRUE)
}
\arguments{
\item{lhs}{A LoCoH-hullset object}

\item{ivg}{Value(s) for inter-visit gap (in seconds) (numeric vector)}

\item{check_samp_int}{Whether or not to check whether \code{ivg} is greater than the median sampling interval. T/F.}

\item{status}{Show status messages. T/F.}
}
\value{
A LoCoH-hullset object
}
\description{
Computes visitation and duration hull metrics based on inter-visit gap value(s)
}
\note{
The inter-visit gap is the period of time (in seconds) which must pass before another occurrence in the hull can be considered a separate visit.
Occurences in the hull are considered a separate visit only if the animal was absent from the hull for a period of time >= ivg.

For each ivg value, the function examines each hull and computes the number of separate visits to the hull (visitation, hull metric name = "nsv")
and the mean number of locations per visit (visit duration, hull metric name = "mnlv")
}
