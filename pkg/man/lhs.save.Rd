\name{lhs.save}
\alias{lhs.save}
\title{Save a LoCoH-hullset object to disk}
\usage{
  lhs.save(lhs, fn = NULL, dir = ".", save.as = NULL,
    suf = NULL, compress = TRUE, auto.num.files = TRUE,
    width = 2)
}
\arguments{
  \item{lhs}{A LoCoH-hullset object}

  \item{fn}{Optional filename. If omitted a sensible
  filename will be constructed}

  \item{dir}{Directory for the output file (relative to the
  working directory), ignored if fn passed}

  \item{suffix}{A suffix that will be used in the
  construction of the filename, ignored if fn is passed}

  \item{compress}{Compress file. T/F.}

  \item{auto.num.files}{Use a number as part of the
  constructed filename. T/F.}

  \item{width}{The number of digits of the auto-number
  token in the filename (ignored if auto.num.files=F)}
}
\description{
  Saves a LoCoH-hullset object to disk, constructing a
  filename if needed that reflects the contents
}
\seealso{
  \code{\link{lxy.save}}
}

