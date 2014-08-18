\name{hist.locoh.lhs}
\alias{hist.locoh.lhs}
\title{Plot histograms of hull metrics}
\usage{
\method{hist}{locoh.lhs}(lhs, id = NULL, k = NULL, r = NULL, a = NULL,
  s = NULL, hs.names = NULL, metric = "area",
  include.missing.hulls = TRUE, hmap = NULL, hmap.in.title = TRUE,
  hs.name.in.title = TRUE, title = NULL, title.show = TRUE,
  figs.per.page = NULL, mar = c(3, 3, if (title.show) 2.8 else 0.7, 0.5),
  mgp = c(1.8, 0.5, 0), same.axes.for.all = FALSE, ufat = TRUE,
  breaks = "Sturges", col = "gray80", png.dir = NULL,
  png.dir.make = TRUE, png.width = 800, png.height = png.width,
  png.overwrite = TRUE, png.pointsize = 12 + (png.width - 480)/80,
  title.two.id = FALSE, indicate.missing.hulls.in.axis.lbl = TRUE,
  panel.num = NULL, panel.num.inside.plot = !title.show, ...)
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

  \item{metric}{The name(s) of hull metric(s); one
  histogram will be created for each hull metric.}

  \item{hmap}{A named list of hull metric auxillary
  parameters, the name of each list element is the name of
  the variable.}

  \item{hmap.in.title}{Include the hmap value(s) in the
  plot subtitle. T/F}

  \item{hs.name.in.title}{Include the name of the hullset
  in the plot subtitle. T/F}

  \item{title}{A title for the plot (over-writes the
  construction of a title)}

  \item{title.show}{Whether to add a title on the plot.
  T/F}

  \item{include.missing.hulls}{Whether to include missing
  hulls on the histogram (using the default value assigned
  to missing hulls by the hull metrics). T/F}

  \item{figs.per.page}{Number of plots per page}

  \item{mar}{The plot margins. A four item numeric vector}

  \item{mgp}{The distance away from the edge of the plot
  for the 1) label, 2) tick marks, and 3) axis line. A
  three-item numeric vector}

  \item{same.axes.for.all}{Whether to use the same range on
  the axes for all histograms generated (helps to visually
  see differences)}

  \item{ufat}{User-friendly axis title. T/F}

  \item{breaks}{Number of breaks or a clustring function.
  See \code{\link{hist}}.}

  \item{col}{Color value for the bars}

  \item{png.dir}{The directory for a PNG file (filename
  will be constructed automatically).}

  \item{png.dir.make}{Whether to create png.dir if it
  doesn't exist. T/F}

  \item{png.width}{The width of the PNG image. Ignored if
  png.fn is passed.}

  \item{png.height}{The height of the PNG image. Ignored if
  png.fn is passed.}

  \item{png.overwrite}{Whether to overwrite an existing PNG
  file if it exists. T/F}

  \item{title.two.id}{Construct a title with the names of
  two ids (for so.* and to.* assocation metrics). T/F}

  \item{indicate.missing.hulls.in.axis.lbl}{Specify whether
  missing hulls are excluded in the axes label, T/F}

  \item{panel.num}{A number or letter to display in the
  upper left hand corner of the plot when the plot will be
  used as part of a multi-frame graphic (as in
  publications). Character}

  \item{panel.num.inside.plot}{Whether to display panel.num
  inside the plot area itself, as opposed to the title
  area. Ignored if panel.num is NULL. T/F}

  \item{png.pointsize}{The pointsize (in pixels) for the
  PNG image (increase to make labels appear larger).
  Determines the height or width of a character in pixels.}

  \item{...}{Other parameters, including any auxillary
  parameters to specify particular hull metrics}
}
\value{
A named list of values of the hull metrics. There is one
list element for each metric.
}
\description{
Plot histograms of hull metrics
}
\note{
To see the names of hull metrics, type \code{hm.expr()}.
Only hull metrics which have been computed can be plotted.
}

