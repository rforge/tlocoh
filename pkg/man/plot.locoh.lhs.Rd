\name{plot.locoh.lhs}
\alias{plot.locoh.lhs}
\title{Plot a LoCoH-hullset object}
\usage{
\method{plot}{locoh.lhs}(lhs, id = NULL, k = NULL, r = NULL, a = NULL,
  s = NULL, hs.names = NULL, iso = FALSE, rast = FALSE, hulls = FALSE,
  hpp = FALSE, dr = FALSE, nn = FALSE, ellipses = FALSE,
  allpts = FALSE, ptid = NULL, ptid.highlight = TRUE, add = FALSE,
  aoi = NULL, iso.idx = NULL, iso.sort.metric = NULL, iso.legend = TRUE,
  legend.space = 0.25, dr.metric = NULL, dr.thresh.val = NULL,
  dr.thresh.type = NULL, dr.smooth = NULL, lwd.dr = 2, pch.allpts = 16,
  cex.nn = 2, cex.hpp = 0.6, cex.allpts = 0.5, cex.pp = 2,
  cex.axis = 0.8, cex.legend = 0.8, col.hpp = "gray50",
  col.hpp.na = "yellow", col.hulls.border = "gray50", col.hulls.fill = NA,
  col.ellipses = "red", col.allpts = c("auto", "gray")[1],
  col.nn = "black", col.nn.pp = "blue", col.iso.fill = 1,
  col.iso.border = NA, col.iso.scale = TRUE, col.dr = "red",
  hpp.classify = c("none", "hsp", hm.expr(names.only = TRUE, print = FALSE,
  desc = FALSE))[1], hpp.classify.bins = 10, hpp.classify.chop = 0.01,
  hpp.classify.legend = TRUE, hpp.classify.common.scale.discrete = TRUE,
  col.ramp = c("gray10,gray90", "rainbow")[1], col.ramp.bins = 10,
  hsp = NULL, record = FALSE, figs.per.page = 1, same.axes.4all = NULL,
  ufat = TRUE, ufipt = TRUE, axes.show = TRUE, axes.ticks = axes.show,
  axes.titles = axes.show, title = NULL, title.show = TRUE,
  title.inc = c("title", "hs.name", "id", "kar", "features", "hpp.classify",
  "hmap", "ptid")[if (is.null(title)) 3:4 else 1], subtitle.inc = c("title",
  "hs.name", "id", "kar", "features", "hpp.classify", "hmap", "ptid")[if
  (is.null(title)) 5:8 else 0], mar = c(if (axes.titles || axes.ticks) 3.3
  else 0.5, if (axes.titles || axes.ticks) 3.2 else 0.5, if (title.show) 3.2
  else 0.5, 0.5), mgp = c(2, 0.7, 0), lo.save = TRUE,
  lo.margins.set = TRUE, desc = c(NONE <- 0, BOTTOM <- 1, TOP <-
  3)[ifelse(figs.per.page == 1, 2, 1)], cex.desc = 0.8,
  col.desc = "darkgreen", tiff.fn = NULL, tiff.pct = FALSE,
  tiff.bands = c(4, 3, 2), tiff.col = gray(0:255/255), tiff.buff = 0,
  tiff.fill.plot = TRUE, shp.csv = NULL, layers = NULL, png.fn = NULL,
  png.dir = NULL, png.dir.make = TRUE, png.fn.pre = NULL,
  png.fn.mid = NULL, png.fn.suf = NULL, png.fn.incld.hs.name = TRUE,
  png.each.plot.separate = TRUE, png.width = 800, png.height = png.width,
  png.pointsize = 12 + (png.width - 480)/80, png.overwrite = TRUE,
  status = TRUE, panel.num = NULL, panel.num.inside.plot = !title.show,
  hmap = NULL, iso.level = NULL, xlim = NULL, ylim = NULL,
  check.ap.value.in.hmparams = TRUE, ...)
}
\arguments{
  \item{lhs}{A \link{LoCoH-hullset} object}

  \item{id}{The names of the individual(s) to include in
  the plot.}

  \item{k}{The k value(s) of the hullset(s) to include in
  the plot. Numeric vector or comma-delimited character
  object.}

  \item{r}{The r value(s) of the hullset(s) to include in
  the plot. Numeric vector or comma-delimited character
  object.}

  \item{a}{The a value(s) of the hullset(s) to include in
  the plot. Numeric vector or comma-delimited character
  object.}

  \item{s}{The s value(s) of the hullset(s) to include in
  the plot. Numeric vector or comma-delimited character
  object.}

  \item{hs.names}{The name(s) of saved hullsets to include
  in the plot}

  \item{iso}{Whether to display isopleths. T/F.}

  \item{rast}{Whether to display rasterized isopleths.
  T/F.}

  \item{hulls}{Whether to display hulls. T/F.}

  \item{hpp}{Whether to display hull parent-points. T/F.}

  \item{dr}{Whether to display directional routes. T/F.}

  \item{nn}{Whether to display nearest neighbors (requires
  that a value for ptid is passed and nearest neighbors
  were saved when creating the hullset object). T/F.}

  \item{ellipses}{Whether to display bounding ellipses
  (requires that ellipses were saved when computing
  ellipses for the hullset). T/F.}

  \item{allpts}{Whether to display all points. T/F.}

  \item{ptid}{One or more ptid (point id) values. A
  separate plot will be drawn 'zoomed in' to each point
  indicated. Can also be "auto", in which case ptid will be
  selected at random. Used primarily to inspect the hull,
  nearest neighbors, and/or ellipse for specific hulls.}

  \item{ptid.highlight}{Whether to highlight the point
  specified by ptid. T/F.}

  \item{add}{Whether to add to the existing plot. T/F.}

  \item{aoi}{An area-of-interest object (e.g., box), used
  to 'zoom in' to specific parts of the plot. aoi objects
  may be created by the function \code{aoi()}.}

  \item{iso.idx}{The index(s) of the isopleths to plot. Use
  the summary() function to see the indices of the
  isopleths.}

  \item{iso.sort.metric}{The sort metric(s) of the
  isopleths that will be displayed. Character.}

  \item{iso.legend}{Whether to include a legend for the
  isopleths.}

  \item{legend.space}{An expansion factor for the x-axis
  that will be used to make room for the legend.}

  \item{dr.metric}{The name of the metric for the
  directional routes to be displayed (acts to filter on
  which directional routes are displayed). See
  \code{\link{lhs.dr.add}}}

  \item{dr.thresh.val}{The threshhold value for the
  directional routes to be displayed (acts to filter on
  which directional routes are displayed). See
  \code{\link{lhs.dr.add}}}

  \item{dr.thresh.type}{The threshhold type for the
  directional routes to be displayed (acts to filter on
  which directional routes are displayed). See
  \code{\link{lhs.dr.add}}}

  \item{dr.smooth}{The smoothing factor for the directional
  routes to be displayed (acts to filter on which
  directional routes are displayed). See
  \code{\link{lhs.dr.add}}}

  \item{lwd.dr}{The line width of directional routes.}

  \item{pch.allpts}{The plot character for all points}

  \item{cex.nn}{The expansion factor for nearest neighbor
  points.}

  \item{cex.hpp}{The expansion factor for hull
  parent-points.}

  \item{cex.allpts}{The expansion factor for all points.}

  \item{cex.pp}{The expansion factor for the parent point.}

  \item{cex.axis}{The expansion factor for axis labels.}

  \item{cex.legend}{The expansion factor for the legend.}

  \item{col.hpp}{The color of hull parent points. Either a
  single color value or a vector of color values of the
  same length as the number of hulls.}

  \item{col.hpp.na}{The color of hull parent points that
  have no value of the metric specified by
  \code{hpp.classify}. Single color value.}

  \item{col.hulls.border}{The outline color of hulls.
  Either a single color value or a vector of color values
  of the same length as the number of hulls.}

  \item{col.hulls.fill}{The fill color of hulls.}

  \item{col.ellipses}{The outline color of bounding
  ellipses. Either a single color value or a vector of
  color values of the same length as the number of hulls.}

  \item{col.allpts}{The color of all points. Single color
  value.}

  \item{col.nn}{The color of nearest neighbors. Single
  color value.}

  \item{col.nn.pp}{The color of parent-points. Used only
  when ptid is passed. Single color value.}

  \item{col.iso.fill}{A number corresponding to a preset
  color ramp that will be used to display the isopleths.
  1=red to blue, 2=yellow to red, 3=blue to red, 4=red to
  yellow. Alternately, a character vector of color values
  of the same length as the number of isopleth levels.}

  \item{col.iso.border}{Color value of the isopleth border.
  Color value (use NA for no border).}

  \item{col.iso.scale}{When plotting a subset of isopleths
  (by ecc or par), whether to scale the isopleth colors to
  the values of the subset. Not being used.}

  \item{col.dr}{The color of directional routes.}

  \item{hpp.classify}{How to classify hull parent points.
  May be "none", the name of a hull metric, or "hsp" (hull
  scatterplot)}

  \item{hpp.classify.common.scale.discrete}{Whether to
  classify hull parent points using a common scale when
  there are multiple maps produced. Ignored if hpp.classify
  is 'none' or 'hsp'. T/F.}

  \item{hpp.classify.bins}{The number of equal-interval
  bins of the hull metric value to create when classifying
  hull parent-points by a hull metric.}

  \item{hpp.classify.chop}{The proportion of hull
  parent-points at the tails of the distribution to remove
  when calculating the bins.}

  \item{hpp.classify.legend}{Whether to include a legend
  for the classification of hull parent-points. T/F.}

  \item{col.ramp}{The first and last values of the color
  ramp to use when classifying hull parent-points by a hull
  metric. Two-item vector with color values.}

  \item{col.ramp.bins}{The number of bins in the color ramp
  when classifying hull parent-points by a hull-metric}

  \item{hsp}{The hull scatterplot object to use when
  displaying hull parent points by a hull scatterplot. May
  be a hull scatterplot object or the index of a hull
  scatterplot saved in the hullset.}

  \item{record}{Whether to open a new plot window and turn
  on recording. T/F.}

  \item{figs.per.page}{The number of plots per page.}

  \item{same.axes.4all}{Whether to use the same axes ranges
  for all plots. T/F.}

  \item{ufat}{Whether to use user-friendly-axis-titles.
  T/F.}

  \item{ufipt}{Whether to use
  user-friendly-isopleth-plot-titles. T/F.}

  \item{axes.show}{Whether to show the axes. T/F.}

  \item{axes.ticks}{Whether to show the tick marks and
  labels on the axes. T/F.}

  \item{axes.titles}{Whether to show axes titles. T/F.}

  \item{title}{The title to be displayed. Character. If
  NULL a title will be constructed.}

  \item{title.show}{Whether to show the title. T/F.}

  \item{title.inc}{The element(s) to include in the title
  (ignored if \code{title} is passed)}

  \item{subtitle.inc}{The element(s) to include in the
  subtitle (ignored if \code{title} is passed)}

  \item{mar}{The plot margins. A four item numeric vector.}

  \item{mgp}{The distance away from the edge of the plot
  for the 1) label, 2) tick marks, and 3) axis line. A
  three-item numeric vector.}

  \item{lo.save}{Whether to save and reset the plot device
  margin settings (some wrapper functions that call this
  function don't want device settings reset). T/F.}

  \item{lo.margins.set}{Whether to save and reset the plot
  device margin settings (some wrapper functions that call
  this function don't want device settings reset). T/F.}

  \item{desc}{Which side to display automatically generated
  desciptive text (e.g. caption). 0=none, 1=bottom, 3=top.}

  \item{cex.desc}{The expansion factor for the descriptive
  text. Numeric value.}

  \item{col.desc}{The color of the descriptive text. Color
  value.}

  \item{tiff.fn}{The path and name of a GeoTIFF file (e.g.,
  satellite image) that will be displayed in the
  background. See notes.}

  \item{tiff.pct}{Whether or to convert the GeoTIFF to an
  indexed 256 color RGB image, which may speed up drawing.
  T/F.}

  \item{tiff.bands}{A vector of exactly one (for a single
  band image) or exactly three integers corresponding to
  the bands of the GeoTIFF image that will be mapped to the
  red, green and blue color guns respectively,. Ignored if
  \code{tiff.fn} only contains one band.}

  \item{tiff.col}{A vector of color values for plotting
  single-band images in the background. Ignored if using
  three bands.}

  \item{tiff.buff}{A numeric buffer distance that the range
  of the plot will be expanded so the points are not right
  on the edge of the GeoTIFF.}

  \item{tiff.fill.plot}{Whether to fill the entire plot
  area with the GeoTIFF. T/F.}

  \item{shp.csv}{The path and filename of a csv file that
  contains information about shapefiles, including layer
  names, file, and symbology.}

  \item{layers}{The name(s) of layers in shp.csv to display
  in the background. Will be displayed using the symbology
  in shp.csv. Character vector or comma delimited string}

  \item{png.fn}{The path and name of the PNG file to create
  (instead of displaying in a plot window)}

  \item{png.dir}{The directory for a PNG file (filename
  will be constructed automatically). Ignored if png.fn is
  passed}

  \item{png.dir.make}{Whether to create png.dir if it
  doesn't exist. T/F}

  \item{png.fn.pre}{A prefix that will be used in the
  construction of the PNG filename. Ignored if png.fn is
  passed}

  \item{png.fn.mid}{A mid-fix that will be used in the
  construction of the PNG filename. Ignored if png.fn is
  passed}

  \item{png.fn.suf}{A suffix that will be used in the
  construction of the PNG filename. Ignored if png.fn is
  passed}

  \item{png.fn.incld.hs.name}{Whether to include the
  hullset name as part of the PNG filename. (T/F). Ignored
  if png.fn is passed}

  \item{png.each.plot.separate}{Whether to make each plot
  in a separate plot / PNG. T/F. Ignored if png.fn is
  passed}

  \item{png.width}{The width of the PNG image. Ignored if
  png.fn is passed}

  \item{png.height}{The height of the PNG image. Ignored if
  png.fn is passed}

  \item{png.pointsize}{The pointsize (in pixels) for the
  PNG image (increase to make labels appear larger).
  Equivalent to the height or width of a character in
  pixels.}

  \item{png.overwrite}{Whether to overwrite an existing PNG
  file if it exists. T/F}

  \item{status}{Whether to show messages. T/F}

  \item{panel.num}{A number or letter to display in the
  upper left hand corner of the plot when the plot will be
  used as part of a multi-frame graphic (as in
  publications). Character}

  \item{panel.num.inside.plot}{Whether to display panel.num
  inside the plot area itself, as opposed to the title
  area}

  \item{hmap}{A named list of hull metric auxillary
  parameters, the name of each list element is the name of
  the variable}

  \item{iso.level}{The isopleth levels to plot, numeric
  vector}

  \item{xlim}{The lower and upper limit of the x-axis,
  two-element numeric vector}

  \item{ylim}{The lower and upper limit of the y-axis,
  two-element numeric vector}

  \item{check.ap.value.in.hmparams}{Whether to check if
  hull metrics exist for the auxillary parameters passed.
  T/F}

  \item{...}{Other parameters, including any auxillary
  parameters required by certain hull metrics}
}
\value{
If png.fn or png.dir is passed, a list object where each
element is a three-element list of the properties of the
PNG file(s) created: $fn, $dim, $desc. Otherwise, returns a
vector of desc
}
\description{
Multi-purpose plotting function for a LoCoH-hullset object
}
\note{
This is a multi-purpose plotting function for
\link{LoCoH-hullset} objects. You specify which objects in
the hullset to include on the plot by passing parameters.

All hullsets have hulls and hull parent points that can be
plotted (e.g., code{hulls=TRUE, hpp=TRUE}. All hullsets
also have the original locations saved and these can be
added to the plot by setting \code{allpts=TRUE}. Depending
how the hullset was created (see \code{\link{lxy.lhs}}),
some hullsets may also have the nearest neighbors for each
parent point saved, which can also be plotted
(\code{nn=TRUE}).

Hullsets for which have bounding ellipses
(\code{\link{lhs.ellipses.add}}) or directional routes
(\code{\link{lhs.dr.add}}) have been computed can have
these elements plotted by setting \code{ellipses=TRUE}) and
\code{dr=TRUE}) respectively. If isopleths have been
constructed (\code{\link{lhs.iso.add}}), these can be
plotted by setting \code{iso=TRUE}. If you only want to
plot some of the isopleths, you can specify which one(s) to
plot with the parameters \code{iso.idx} (the index(s) of
the isopleths to plot, see \code{\link{summary.locoh.lhs}})
or \code{iso.sort.metric}.

The plot title can be manually set with the \code{title}
parameter. If no value for \code{title} is passed, a title
will be constructed. You can define what elements will go
in the title and subtitle (i.e., the second line of the
title) with the \code{title.inc} and \code{subtitle.inc}
parameters. To omit the plot title completely, set
\code{title.show=FALSE}.

\code{tiff.fn}, \code{tiff.pct}, \code{tiff.buff},
\code{tiff.bands}, \code{tiff.col}, and
\code{tiff.fill.plot} control the display of a GeoTIFF
image in the plot background. The GeoTIFF image must be
georeferenced in the same coordinate system as the
locoh-hullset object, and the pixel values must be
'prestretched' for display. \code{tiff.bands} controls
which bands in the TIFF file will be displayed using the
red, green, and blue color guns if using three bands. In a
Landsat TM image, for example, the first four bands are
blue, green, red, and infrared. To display a TM GeoTIFF
image as 'natural colors', you would set
\code{tiff.bands=c(3,2,1)}. If \code{tiff.pct=T}, the
script will create a indexed 256-color version of the
image, which may result in quicker drawing time
particularly if several plots are being drawn. If using a
single-band, for example a DEM or classified image, the
colors are set by \code{tiff.col}. \code{tiff.buff} can be
used to expand the range of values on the x and y axis so
that you see a bit of the background image beyond the
extent of the points.
}

