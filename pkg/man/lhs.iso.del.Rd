\name{lhs.iso.del}
\alias{lhs.iso.del}
\title{Delete an isopleth}
\usage{
  lhs.iso.del(lhs, iso.idx = "all")
}
\arguments{
  \item{lhs}{A LoCoH-hullset object}

  \item{iso.idx}{The index(s) of isopleths to delete
  (numeric vector or 'all')}
}
\value{
  A LoCoH-hullset object
}
\description{
  Delete isopleth(s) from a LoCoH-hullset object
}
\note{
  To see the indices of isopleths, run
  \code{\link{summary.locoh.lhs}}
}

