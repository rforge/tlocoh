\name{lhs.to.add}
\alias{lhs.to.add}
\title{Add hull metrics for association analysis}
\usage{
  lhs.to.add(lhs, id = "all", hs2.id = "all",
    maxdt = "auto", save.hto = TRUE, status = TRUE)
}
\arguments{
  \item{lhs}{A LoCoH-hullset object}

  \item{save.hto}{Whether to save the list of hull indices
  that temporally overlap, T/F}

  \item{maxdt}{The maximum difference in time (in seconds)
  for two hulls to be considered 'overlapping' in time. Can
  also be \code{auto}, in which case half ot the smallest
  median sampling interval will be used.}
}
\value{
  A LoCoH-hullset object
}
\description{
  Add hull metrics for association analysis
}

