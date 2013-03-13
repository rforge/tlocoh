\name{lhs.dr.add}
\alias{lhs.dr.add}
\title{Define directional routes}
\usage{
  lhs.dr.add(lhs, metric = c("ecc", "par")[2],
    thresh.val = 0.95, thresh.type = c("q", "v")[1],
    smooth = 1, status = TRUE, show.elong.hist = FALSE)
}
\arguments{
  \item{lhs}{A LoCoH-hullset object}
}
\value{
  A LoCoH-hullset object
}
\description{
  Defines 'directional routes' by identifying parent points
  that are 1) temporally contiguous and 2) have hulls in
  the top n% of elongation values
}
\note{
  Directional routes are line segments defined by
  connecting temporally contiguous hull parent-points that
  are in the top N percent of hull elongation
}

