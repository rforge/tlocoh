\name{lhs.plot.scatter}
\alias{lhs.plot.scatter}
\title{Create scatterplot of hull metrics}
\usage{
  lhs.plot.scatter(lhs, id = NULL, k = NULL, r = NULL,
    a = NULL, s = NULL, hs.names = NULL, x.axis = NULL,
    y.axis = NULL, limx = NULL, limy = NULL,
    trans.x = NULL, trans.y = NULL, jiggle.x = "auto",
    jiggle.y = "auto", filter = NULL,
    filter.label.in.subtitle = TRUE,
    filter.sampsize.in.subtitle = TRUE,
    filter.col.use = TRUE, filter.axes.uniform = TRUE,
    title = NULL, title.show = TRUE,
    title.hs.name.include = TRUE, title.two.id = FALSE,
    title.axes = TRUE, col = c("gray50", "spiral")[1],
    bg = NULL,
    mar = c(4, 3.2, if (title.show) (if (title.hs.name.include) 3.9 else 3.2) else 0.5, 0.5),
    mgp = c(2.1, 0.8, 0), lo.save = TRUE,
    lo.margins.set = TRUE, lo.colors.set = TRUE,
    lo.bg.as.box = FALSE, sat.base = NULL, val.base = NULL,
    hue.offset = NULL,
    center.method = c("bbox", "mean")[2], cex = 0.6,
    type = c("p", "l", "b")[1], ufat = NULL,
    figs.per.page = 1, add = FALSE, regions = NULL,
    prompt.labels = TRUE, hsp = NULL, hsp.reg.col = TRUE,
    hsp.reg.out = FALSE, hsp.reg.lbl = FALSE,
    hsp.override = TRUE, png.fn = NULL, png.dir = NULL,
    png.dir.make = TRUE, png.width = 800,
    png.height = png.width,
    png.pointsize = 12 + (png.width - 480)/80,
    png.fn.pre = NULL, png.fn.mid = NULL,
    png.fn.suf = NULL,
    png.exists = c("overwrite", "skip", "abort")[1],
    status = TRUE,
    desc = c(NONE <- 0, BOTTOM <- 1, TOP <- 3)[ifelse(figs.per.page == 1, 2, 1)],
    cex.desc = 0.8, col.desc = NULL, panel.num = NULL,
    panel.num.inside.plot = !title.show, hmap = NULL,
    hmap.in.subtitle = TRUE,
    check.ap.value.in.hmparams = TRUE, ...)
}
\arguments{
  \item{lhs}{A LoCoH-hullset object}

  \item{id}{The name(s) of the individual(s) to include in
  the plot. Character vector or comma-delimited character.}

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
  in the plot.}

  \item{x.axis}{The name of a hull metric (see
  \code{\link{hm.expr}})}

  \item{y.axis}{The name of a hull metric}

  \item{limx}{The lower and upper limits of the x-axis.
  Two-element numeric vector.}

  \item{limy}{The lower and upper limits of the y-axis.
  Two-element numeric vector.}

  \item{trans.x}{The name of a function that will be used
  to transform the x-axis values. Can be any R function
  (e.g., "log", "sqrt"), also "square" and "cube"}

  \item{trans.y}{The name of a function that will be used
  to transform the y-axis values}

  \item{jiggle.x}{"auto" or a numeric value which will be
  used to add a normally distributed stochastic value (mean
  0, sd=jiggle.x) to the x-axis values for better
  visualization of the number of points in each group
  (helpful when the x-values take on discrete values)}

  \item{jiggle.y}{"auto" or a numeric value which will be
  used to add a normally distributed stochastic value (mean
  0, sd=jiggle.y) to the y-axis values}

  \item{title}{The title to be displayed. Character. If
  NULL a title will be constructed.}

  \item{title.show}{Whether to show the title. T/F.}

  \item{title.hs.name.include}{Whether to include the
  hullset name as part of the title. Ignored if title is
  passed. T/F.}

  \item{title.axes}{Whether to include the names of the
  hull metrics on the axes as part of the subtitle. Ignored
  if title is passed. T/F.}

  \item{title.two.id}{An ad-hoc way to construct a title
  consisting of the ids of the hullset and hs2}

  \item{filter}{NULL or a list of filter parameters. The
  code will loop through these parameters and generate
  scatterplots for each subsets of points. Each element of
  the filter list must be another list with the following
  elements: $idx = a vector of the indices of the hulls;
  $label = a label.}

  \item{filter.label.in.subtitle}{Whether to add the filter
  label to the plot subtitle. Ignored if title is passed or
  filter is NULL. T/F.}

  \item{filter.sampsize.in.subtitle}{Whether to add the
  filter sample size to the plot subtitle. Ignored if title
  is passed or filter is NULL. T/F.}

  \item{filter.col.use}{Whether the points will be
  displayed with the color(s) saved in the filter
  (overriding the 'col' parameter). Ignored if filter is
  NULL. T/F.}

  \item{filter.axes.uniform}{Whether the upper and lower
  limits of the axes for each plot will be set as the limit
  for the entire combined dataset. Ignored if filter is
  NULL or values are passed for limx or limy. T/F.}

  \item{col}{A single color value, or a vector of color
  values of the same length as the number of hulls, or
  "spiral" (see additional parameters below). Ignored if
  filter.col.use=T or something is passed for hsp (in which
  case col will be extracted from hsp).}

  \item{bg}{Background color}

  \item{lo.colors.set}{Whether to set the background,
  foreground color on the plot device. T/F.}

  \item{lo.bg.as.box}{Whether to display the background
  color as a box on the plot (needed by some wrapper
  function that want the axes and margins to be a different
  color). This overrides the setting of lo.colors.set.
  T/F.}

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

  \item{sat.base}{A number between 0 and 1 for the base
  staturation (how much of the color is present) of the
  center color (s=0.4 gives pastels), ramping up to 1 (full
  color) at edges.}

  \item{val.base}{A number between 0 and 1 controlling how
  much black is in the center color (0 is total black),
  ramping out to no blackness at the edges}

  \item{hue.offset}{A number between 0 and 2*pi for the
  color wheel rotation (in radians), which controls the
  color of points directly to the right of the center.}

  \item{center.method}{Determines how the center of the
  color will be computed. "bbox" = the center of the
  bounding box of the full range of points; "mean" = the
  mean of the data splot}

  \item{cex}{Expansion factor for the points on the
  scatterplot}

  \item{ufat}{Whether to substitute user-friendly axis
  titles. T/F.}

  \item{figs.per.page}{The number of plots per page}

  \item{add=}{Whether to add to the current plot device.
  T/F.}

  \item{regions}{Determines whether the user will be
  prompted to draw regions (polygons) on the scatterplot
  window with the mouse, that will be saved as part of the
  scatterplot objecr returned. Values can be 1) a number
  whereby the user will be prompted to create N regions
  with randomly assigned colors, or 2) a vector of color
  values in which case the regions will be assigned those
  colors.}

  \item{prompt.labels}{Whether the user should be prompted
  to also enter labels for each of the regions drawn.
  Ignored if regions = NULL. T/F.}

  \item{hsp}{Either the index(s) of a hull scatterplot(s)
  saved in lhs (use summary() command to which how many hsp
  objects have been saved), or a list of objects of class
  locoh.hsp. When passed, the parameters in hsp will be
  used to create the scatterplot}

  \item{hsp.reg.col}{Whether to use the colors assigned to
  the regions in hsp. Ignored if hsp is NULL. T/F.}

  \item{hsp.reg.out}{Whether to display the outline of the
  regions in hsp. Ignored if hsp is NULL. T/F.}

  \item{hsp.reg.lbl}{Whether to display the labels of the
  regions in hsp. Ignored if hsp is NULL. T/F.}

  \item{hsp.override}{Whether the parameters in hsp should
  override other parameters passed. Ignored if hsp is NULL.
  T/F.}

  \item{png.fn}{The path and name of the PNG file to create
  (instead of displaying in a plot window).}

  \item{png.dir}{The directory for a PNG file (filename
  will be constructed automatically). Ignored if png.fn is
  passed.}

  \item{png.dir.make}{Whether to create png.dir if it
  doesn't exist. T/F.}

  \item{png.width}{The width of the PNG image. Ignored if
  png.fn is passed.}

  \item{png.height}{The height of the PNG image. Ignored if
  png.fn is passed.}

  \item{png.pointsize}{The pointsize (in pixels) for the
  PNG image (increase to make labels appear larger).
  Equivalent to the height or width of a character in
  pixels.}

  \item{png.fn.pre}{A prefix that will be used in the
  construction of the PNG filename. Ignored if png.fn is
  passed.}

  \item{png.fn.mid}{A mid-fix that will be used in the
  construction of the PNG filename. Ignored if png.fn is
  passed.}

  \item{png.fn.suf}{A suffix that will be used in the
  construction of the PNG filename. Ignored if png.fn is
  passed.}

  \item{png.exists}{What to do if a PNG with the same
  filename already exists: "overwrite", "skip", or "abort"}

  \item{status}{Whether to show messages. T/F.}

  \item{panel.num}{A number or letter to display in the
  upper left hand corner of the plot, used when the plot
  will be part of a multi-frame graphic (as in
  publications). Character.}

  \item{panel.num.inside.plot}{Whether to display panel.num
  inside the plot area itself (as opposed to the title
  area). T/F.}

  \item{hmap}{A named list of hull metric auxillary
  parameters, the name of each list element is the name of
  the variable.}

  \item{hmap.in.subtitle}{Include the hmap value(s) in the
  plot subtitle. T/F.}

  \item{...}{Other parameters, including any auxillary
  parameters required by certain hull metrics}
}
\value{
  If png.fn or png.dir is passed, exports the plots to a
  PNG file(s) and returns a list of file names, dimensions
  of the images produced. Otherwise returns a named list of
  objects of class 'locoh.hsp'.
}
\description{
  Multi-purpose scatterplot function for the hull metrics
  in a LoCoH-hullset
}
\note{
  This function has two main purposes. 1) To make
  scatterplot graphics either in a plot window or PNG file
  2) To create a list of objects of class "locoh.hsp",
  which are basically a collection of the parameters that
  were used to create the scatterplot and any manually
  digitized regions created by the user. When not being
  used to create PNG files, this function returns a list of
  "locoh.hsp" objects which can then be 'permanently' saved
  in the LoCoH-hullset object using
  \code{\link{lhs.hsp.add}}, and/or fed into other
  functions such as \code{\link{plot.locoh.lhs}}. Note that
  hull metrics must already have been computed. Several
  hull metrics are 'automatically' computed when the
  hullset is defined (e.g., hull area, number of enclosed
  points). Other hull metrics must be created separately
  with functions such as \code{\link{lhs.ellipses.add}} and
  \code{\link{lhs.visit.add}}.

  Auxillary parameters required for hull metrics can be
  passed either as individual parameters (e.g.,
  \code{ivg=3600*12}) or as a list element
  \code{hmap=list(ivg=86400)}
}
\seealso{
  \code{\link{lhs.plot.scatter.auto}},
  \code{\link{hm.expr}}, \code{\link{lhs.hsp.add}}
}

