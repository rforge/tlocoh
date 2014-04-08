\name{plot.locoh.lxy}
\alias{plot.locoh.lxy}
\title{Plot LoCoH-xy object}
\usage{
\method{plot}{locoh.lxy}(lxy, id = NULL, cex = 0.8, show.start = TRUE,
  show.end = TRUE, col = c("auto", "gray80")[1], connect.dots = TRUE,
  overlay = FALSE, status = TRUE, title = NULL, title.show = TRUE,
  axes.show = TRUE, axes.titles = axes.show, axes.ticks = axes.show,
  mar = c(if (axes.titles || axes.ticks) 3.3 else 0.5, if (axes.titles ||
  axes.ticks) 3.2 else 0.5, if (title.show) 3.2 else 0.5, 0.5), mgp = c(2,
  0.7, 0), lo.save = TRUE, panel.num = NULL,
  panel.num.inside.plot = !title.show, png.fn = NULL, png.dir = NULL,
  png.dir.make = TRUE, png.width = 800, png.height = png.width,
  png.overwrite = TRUE, png.pointsize = 12 + (png.width - 480)/80,
  tiff.fn = NULL, tiff.bands = c(3, 2, 1), tiff.pct = FALSE,
  tiff.buff = 500, tiff.fill.plot = TRUE, layers = NULL, shp.csv = NULL,
  xlim = NULL, ylim = NULL, legend = NULL, ...)
}
\arguments{
  \item{lxy}{A \link{LoCoH-xy} object}

  \item{id}{A vector of the id value(s) to plot}

  \item{cex}{Character expansion factor for the points}

  \item{show.start}{Whether to highlight the starting
  location (if time stamps are present) in green. T/F.}

  \item{show.end}{Whether to highlight the end location (if
  time stamps are present) in red. T/F.}

  \item{col}{A single value or vector of color values. Can
  also be 'auto' in which case the colors saved in
  \code{lxy} will be used (which are rainbow by default).}

  \item{connect.dots}{Whether to draw line segments between
  consecutive locations. T/F.}

  \item{overlay}{Whether to overlay the plots of all
  individuals in \code{lxy} on one pair of axes (map).
  T/F.}

  \item{status}{Display status messages. T/F.}

  \item{title}{The title to be displayed. Character. If
  NULL a title will be constructed.}

  \item{title.show}{Whether to show the title. T/F.}

  \item{axes.show}{Whether to show the axes. T/F.}

  \item{axes.titles}{Whether to show axes titles. T/F.}

  \item{axes.ticks}{Whether to show the tick marks and
  labels on the axes. T/F.}

  \item{mar}{The plot margins. A four item numeric vector}

  \item{mgp}{The distance away from the edge of the plot
  for the 1) label, 2) tick marks, and 3) axis line. A
  three-item numeric vector}

  \item{lo.save}{Whether to save and reset the plot device
  margin settings (some wrapper functions that call this
  function don't want device settings reset). T/F.}

  \item{panel.num}{A number or letter to display in the
  upper left hand corner of the plot when the plot will be
  used as part of a multi-frame graphic (as in
  publications). Character}

  \item{panel.num.inside.plot}{Whether to display panel.num
  inside the plot area itself, as opposed to the title
  area. T/F}

  \item{png.fn}{The path and name of the PNG file to create
  (instead of displaying in a plot window)}

  \item{png.dir}{The directory for a PNG file (filename
  will be constructed automatically). Ignored if png.fn is
  passed}

  \item{png.dir.make}{Whether to create png.dir if it
  doesn't exist. T/F}

  \item{png.width}{The width of the PNG image}

  \item{png.height}{The height of the PNG image}

  \item{png.overwrite}{Whether to overwrite an existing PNG
  file if it exists. T/F}

  \item{png.pointsize}{The pointsize (in pixels) for the
  PNG image, equivalent to the height or width of a
  character in pixels (increase to make labels appear
  larger)}

  \item{tiff.fn}{The path and name of a GeoTIFF file (e.g.,
  satellite image) that will be displayed in the
  background. See notes.}

  \item{tiff.bands}{A vector of threee integers
  corresponding to the bands of the GeoTIFF image that will
  be mapped to the red, green and blue color guns
  respectively.}

  \item{tiff.pct}{Whether or to convert the GeoTIFF to an
  indexed 256 color RGB image, which may speed up drawing.
  T/F.}

  \item{tiff.buff}{A numeric buffer distance in map units
  that the range of the plot will be expanded so the points
  are not right on the edge of the GeoTIFF.}

  \item{tiff.fill.plot}{Whether to fill the entire plot
  area with the GeoTIFF. T/F.}

  \item{layers}{The name(s) of layers in shp.csv to display
  in the background. Will be displayed using the symbology
  in shp.csv. Character vector or comma delimited string}

  \item{shp.csv}{The path and filename of a csv file that
  contains information about shapefiles, including layer
  names, file, and symbology.}

  \item{xlim}{The lower and upper limit of the x-axis,
  two-element numeric vector}

  \item{ylim}{The lower and upper limit of the y-axis,
  two-element numeric vector}

  \item{legend}{One of the following keywords specifying
  where to put a legend when overlaying the locations of
  multiple individuals: \code{bottomright}, \code{bottom},
  \code{bottomleft}, \code{left}, \code{topleft},
  \code{top}, \code{topright}, \code{right} or
  \code{center}}

  \item{...}{Additional parameters that will be passed to
  the \code{\link{plot}} function}
}
\description{
Multi-purpose plotting function for a LoCoH-xy object
}
\details{
This is a multi-purpose plotting function for a LoCoH-xy
object, with several custom arguments useful for plotting a
movement trajectory. You may also extract the
SpatialPointsDataFrame element of the LoCoH-xy object
directly by referencing the \code{pts} element (e.g.,
\code{x$pts}, where \code{x} is a LoCoH-xy object).
}
\seealso{
Vignette on T-LoCoH data classes, Tutorial Vignette (for
details on displyaing GIS layers and raster images in the
background)
}

