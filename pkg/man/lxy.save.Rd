\name{lxy.save}
\alias{lxy.save}
\title{Save a LoCoH-xy object to disk}
\usage{
  lxy.save(lxy, fn = NULL, save.as = NULL, dir = ".",
    suf = NULL, compress = TRUE, auto.num.files = TRUE,
    width = 2)
}
\arguments{
  \item{lxy}{A LoCoH-xy object}

  \item{fn}{Optional filename}

  \item{save.as}{The name of the saved object, if NULL the
  original object name will be used}

  \item{dir}{Directory for the output file (relative to the
  working directory), ignored if fn passed}

  \item{suffix}{A suffix that will be used in the
  construction of the filename, ignored if fn is passed}

  \item{compress}{Compress file (T/F)}

  \item{auto.num.files}{Use a numeric token as part of the
  constructed filename to get a unique filename(T/F)}

  \item{width}{The number of digits in auto.num.files}
}
\description{
  Saves a LoCoH-xy object to disk, constructing a filename
  if needed that reflects the contents
}
\seealso{
  \code{\link{xyt.lxy}}, \code{\link{lhs.save}}
}

