\name{lxy.save}
\alias{lxy.save}
\title{Save a LoCoH-xy object to disk}
\usage{
lxy.save(lxy, file = NULL, save.as = NULL, dir = ".", suf = NULL,
  compress = TRUE, auto.num.files = TRUE, width = 2)
}
\arguments{
  \item{lxy}{A \link{LoCoH-xy} object}

  \item{file}{Optional file name}

  \item{save.as}{The name of the saved object, if NULL the
  original object name will be used}

  \item{dir}{Directory for the output file (relative to the
  working directory), ignored if \code{file} passed}

  \item{suf}{A suffix that will be used in the construction
  of the file name, ignored if \code{file} is passed}

  \item{compress}{Compress file (T/F)}

  \item{auto.num.files}{Use a numeric token as part of the
  constructed file name to get a unique file name(T/F)}

  \item{width}{The number of digits in auto.num.files}
}
\description{
Saves a LoCoH-xy object to disk, constructing a file name
if needed that reflects the contents
}
\seealso{
\code{\link{xyt.lxy}}, \code{\link{lhs.save}}
}

