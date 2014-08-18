\name{lxy.exp.shp}
\alias{lxy.exp.shp}
\title{Export a LoCoH-xy to shapefile}
\usage{
lxy.exp.shp(lxy, id = NULL, dir = ".", file.base = "", anv = "all",
  idsSeparate = FALSE, autoName = c("long", "short", "none")[1],
  status = TRUE)
}
\arguments{
  \item{lxy}{A \link{LoCoH-xy} object}

  \item{id}{The name(s) of individuals to export. If
  \code{NULL} all individuals will be exported}

  \item{dir}{The directory where the shapefiles will be
  placed (use "." for the working directory, and "~" for
  the home directory)}

  \item{file.base}{The base of the file name without any
  extension (the script will append to this)}

  \item{anv}{The name of ancillary variables(s) to be
  exported in the attribute table. Character vector or
  comma separated string.}

  \item{idsSeparate}{Whether to export the locations for
  each individual in separate shapefiles. T/F}

  \item{autoName}{The rule for constructing a filename:
  \code{short} (consisting of the id(s) only), \code{long}
  (id + num points), or \code{none}}

  \item{status}{Show messages. T/F}
}
\value{
A character vector of the shapefile(s) created
}
\description{
Export a LoCoH-xy object to point shapefile
}
\details{
Filenames are automatically generated, but the user can
specify the directory and a base (\code{file.base}). If a
shapefile already exists, a unique filename will be
constructed using two-digit numeric extension. For shorter
filenames, pass \code{autoName="short"}.

Use the \code{anv} argument to specify which ancillary
variables (i.e., attribute fields) should be exported. To
not export any ancillary variables, pass an empty string.

Only the individual locations and their ancillary values
are exported. The nearest neighbor table is not exported.
}
\seealso{
\code{\link{lxy.exp.kml}}, \code{\link{lxy.exp.mov}}
}

