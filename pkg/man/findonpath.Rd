\name{findonpath}
\alias{findonpath}
\title{findonpath}
\usage{
findonpath(fn, status = TRUE)
}
\arguments{
  \item{fn}{The filename to search for (without a path)}

  \item{status}{Show messages}
}
\value{
The full path and name of the found file, or NULL if not
found
}
\description{
Searches for a file on the system environment path
}
\note{
This will return the first found occurence of file
\code{fn}, searching 1) the current working directory, 2)
the user's R 'home' directory, then 3) the directories on
the operating system environment 'path'
}

