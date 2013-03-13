\name{lxy.gridanv.add}
\alias{lxy.gridanv.add}
\title{Add ancillary variable(s) to a LoCoH-xy object from a raster}
\usage{
  lxy.gridanv.add(lxy, layer = 1, dtfn = NULL, fn = NULL,
    anv.name = NULL, anv.desc = NULL, overwrite = FALSE)

  lxy.gridanv.add(lxy, layer = 1, dtfn = NULL, fn = NULL,
    anv.name = NULL, anv.desc = NULL, overwrite = FALSE)
}
\arguments{
  \item{lxy}{\link{LoCoH-xy} object}

  \item{band}{Numeric vector of which band(s) to read}

  \item{dtfn}{A two-column data frame containing a date and
  filename}

  \item{anv.name}{The name(s) of the ancillary variable(s)
  (one per band)}

  \item{anv.desc}{A character vector of descriptions}

  \item{overwrite}{Whether to overwrite existing variables
  with the same names (T/F)}

  \item{lxy}{\link{LoCoH-xy} object}

  \item{anv}{vector, named list or data frame with the same
  number of values as points}

  \item{anv.desc}{A character vector of descriptions}

  \item{overwrite}{Whether to overwrite existing variables
  with the same names (T/F)}
}
\value{
  A \link{LoCoH-xy} object

  A \link{LoCoH-xy} object
}
\description{
  Add ancillary variable(s) to a LoCoH-xy object from a
  raster

  Add ancillary variable(s) to a LoCoH-xy object
}
\note{
  This function will create new ancillary variables and
  fill it with the value of the pixel at each point. The
  image should be in the same coordinate system as the
  points. Current the script does not check this. Any
  points that fall beyond the edges of the image will be
  given 'NA' values.

  The image / raster must be in a format supported by
  \code{\link{readGDAL}} in the rdgal package. Multi-band
  formats are support. If the image has multiple bands, you
  can specify which band(s) to read using the \code{band}
  parameter. A separate ancillary variable will be created
  for each band. You can provide name(s) for the ancillary
  variable(s) with the \code{anv.name parameter}.

  You can also pass a time-series of images, and the
  function will use the image that is the closest match in
  time for each location. To do this, pass a two column
  data frame to the parameter \code{dtfn}. The first column
  of \code{dtfn} must be a date (one of the POSIXt
  date-time classes), and the second column must be a valid
  raster filename. If each image is a time composite,
  (e.g., two-week NDVI), the date should either be the
  mid-point or the beginning date. Two options for time
  matching are available: \code{closest} = select the image
  whose date is closest in time, and \code{before} = pick
  the closest image that comes before the location.
}

