\name{lhs.exp.shp}
\alias{lhs.exp.shp}
\title{Export a LoCoH-hullset to shapefile}
\usage{
  lhs.exp.shp(lhs, id = NULL, k = NULL, r = NULL, a = NULL,
    s = NULL, hs.names = NULL, hpp = FALSE, hulls = FALSE,
    iso = FALSE, nn = FALSE, ellipses = FALSE,
    allpts = FALSE, iso.idx = NULL, iso.metric = NULL,
    dir = ".", file.base = "", file.base.auto = TRUE,
    avl.file = NULL, status = TRUE, show.time = TRUE,
    hm = "all", anv = NULL, hsp = NULL, metadata = TRUE)
}
\arguments{
  \item{lhs}{A LoCoH-hullset object}

  \item{id}{The name(s) of individuals to export}

  \item{k}{The k value of hullsets to export}

  \item{r}{The r value of hullsets to export}

  \item{a}{The a value of hullsets to export}

  \item{s}{The s value of hullsets to export}

  \item{hs.names}{The name(s) of saved hullsets to export}

  \item{hulls}{Export hulls (as a polygon shapefile). T/F}

  \item{hpp}{Export hull parent points (as a point
  shapefile). T/F}

  \item{iso}{Export isopleths (as a polygon shapefile).
  T/F}

  \item{iso.metric}{Hull sort metric(s) for the isopleths
  that will be exported (acts a filter). Character vector.}

  \item{iso.idx}{Numeric vector of the indices of the
  isopleths that will be exported (acts a filter)}

  \item{ellipses}{Export ellipses (as a polygon shapefile).
  T/F}

  \item{allpts}{Export all points (as a point shapefile).
  T/F}

  \item{dir}{The directory where the shapefiles will be
  placed (use "." for the working directory, and "~" for
  the user directory)}

  \item{file.base}{The base of the file name without any
  extension (the script will append to this)}

  \item{file.base.auto}{Whether to automatically generate
  the file name base from the id, s-value, and k-a-r value.
  Ignored if \code{file.base} is passed. T/F}

  \item{projargs}{A character string that contains info
  about the coordinate system the data are projected in}

  \item{prj.file}{The name of an existing ArcView *.prj
  file, which contains projection info}

  \item{avl.file}{The name of an existing avl (ArcView
  legend) file}

  \item{status}{Show messages. T/F}

  \item{show.time}{Report time for script to complete. T/F}

  \item{hm}{The name of hull metric(s) that will be
  included in the attribute table. Default is 'all'.
  Applicable only to hulls and hull parent points.
  Character vector or comma separated string.}

  \item{anv}{The name of ancillary variables(s) of the
  parent point that will be included in the attribute
  table. Default is NULL. Applicable only to hulls and hull
  parent points. Character vector or comma separated
  string.}

  \item{hsp}{A list containing one hull scatterplot object
  with regions saved (i.e., object returned by
  \code{\link{lhs.plot.scatter}}), or the index of a hull
  scatter plot saved in the hullset (see
  \code{\link{lhs.hsp.add}}). If passed, a column for the
  region number will be included in the attribute table.
  Applicable only to hulls and hull parent points.}

  \item{metadata}{Export field descriptions in a meta data
  file (not yet supported)}
}
\description{
  Export hulls, hull parent points, and/or isopleths from a
  locoh-hullset object to ESRI's shapefile format
}
\note{
  Filenames are automatically generated, but the user can
  specify the directory. Constructed filenames will consist
  of the \code{file.base} (if provided) followed by the
  name of the hullset and a suffix indicating the type of
  feature saved (e.g., '.hulls', '.pts', ".iso", etc. ). If
  a shapefile already exists with this, a unique filename
  will be constructed using two-digit numeric extension

  Most of the time, exporting hull parent points
  (\code{hpp=T}) is the same as exporting all points
  (\code{allpts=T}), however in some cases not all points
  have enough neighbors to make a hull so they wouldn't be
  included in exported of hull parent points. Only hull
  parent points will have the corresponding hull metrics
  included in the attribute table.

  Passing a value for \code{projargs} will not re-project
  the data, only describe the existing projection. The
  projarg string must follow the format exactly as in the
  PROJ.4 documentation, in particular there cannot be any
  white space in the syntax '+<arg>=<value> strings', and
  successive such strings can only be separated by blanks.
  An example is \code{"+proj=utm +south +zone=33"}. See
  also
  \link{http://hosho.ees.hokudai.ac.jp/~kubo/Rdoc/library/sp/html/CRS-class.html}
  and the main users manual for PROJ available at
  \link{http://trac.osgeo.org/proj}.

  In lieu of projargs, you can supply an argument for
  \code{prj.file} (but not both). The script will make a
  copy of prj.file with the same base name as the
  shapefile. Prj.file is presumed to be relative to the
  working directory.

  avl.file is an ArcView 3.x file that contains symbology
  info. If a value is passed, the script will make a copy
  of prj.file with the same base name as the shapefile, so
  that symbology will automatically created in ArcMap.
  avl.file is presumed to be relative to the working folder
}
\seealso{
  \code{\link{lhs.exp.csv}}
}

