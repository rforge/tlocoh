\name{lhs.anv.del}
\alias{lhs.anv.del}
\title{Delete ancillary variables from a LoCoH-hullset}
\usage{
  lhs.anv.del(lhs, anv = NULL, id = NULL, k = NULL,
    r = NULL, a = NULL, s = NULL, hs.names = NULL,
    status = TRUE)
}
\arguments{
  \item{lhs}{A LoCoH-hullset object}

  \item{anv}{The name(s) of ancillary variables to remove}

  \item{id}{The name(s) of individuals to process}

  \item{k}{The k value of hullsets to process}

  \item{r}{The r value of hullsets to process}

  \item{a}{The a value of hullsets to process}

  \item{s}{The s value of hullsets to process}

  \item{hs.names}{The name(s) of hullsets to process}

  \item{status}{Show status messages (T/F)}
}
\value{
  A LoCoH-hullset object
}
\description{
  Delete ancillary variables from a LoCoH-hullset
}
\note{
  To see which ancillary variables a hullset contains, use
  the \code{summary()} function.

  Copying ancillary variables is optional when creating a
  hullset with \code{\link{lxy.lhs}}.
}
\seealso{
  \code{\link{summary.locoh.lhs}}
}

