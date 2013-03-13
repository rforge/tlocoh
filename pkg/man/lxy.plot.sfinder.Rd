\name{lxy.plot.sfinder}
\alias{lxy.plot.sfinder}
\title{Plot distributions of 's' such that the vmax/diffusion term in TSD are equivalent to the actual displacement}
\usage{
  lxy.plot.sfinder(lxy, id = NULL, delta.t = "auto",
    delta.t.auto.n = 8, delta.t.err = 0.01,
    outline = FALSE, debug = FALSE, desc = c(0, 1, 3)[2],
    cex.desc = 0.8, col.desc = "darkgreen",
    time.term = c("vmax", "dif")[1],
    mar = c(3, 3, if (title.show) 2.8 else 0.7, 0.5),
    mgp = c(1.8, 0.5, 0), figs.per.page = NULL,
    title = NULL, title.show = TRUE, panel.num = NULL,
    panel.num.inside.plot = !title.show, png.dir = NULL,
    png.dir.make = TRUE, png.width = 800,
    png.height = png.width, png.overwrite = TRUE,
    png.pointsize = 12 + (png.width - 480)/80, ...)
}
\arguments{
  \item{lxy}{A LoCoH-xy object}

  \item{id}{The id(s) of the individual(s) to include in
  the plot}

  \item{delta.t}{numeric vector of delta.t values (in
  seconds) for which a s-values will be computed. Can also
  be 'auto' in which case \code{delta.t.auto.n} delta.t
  values will be computed ranging from 3 * tau to the 80th
  percent point-to-point interval for the entire dataset}

  \item{delta.t.auto.n}{The number of delta-ts to use when
  delta.t="auto"}

  \item{delta.t.err}{The proportion of delta.t within which
  the interval between two points must fall to be included
  in the plot}

  \item{outline}{Show outliers in the box plots T/F}

  \item{desc}{Which side to display automatically generated
  desciptive text (e.g. caption). 0=none, 1=bottom, 3=top.}

  \item{cex.desc}{The expansion factor for the descriptive
  text. Numeric value.}

  \item{time.term}{The type of time term to use ('vmax' is
  computed as deltaT * maximum possible velocity, while
  'dif' computes Gauusian diffusion distance)}

  \item{col.desc}{The color of the descriptive text. Color
  value.}

  \item{mar}{The plot margins. A four item numeric vector}

  \item{mgp}{The distance away from the edge of the plot
  for the 1) label, 2) tick marks, and 3) axis line. A
  three-item numeric vector}

  \item{figs.per.page}{The number of plots per page.}

  \item{title}{The title to be displayed. Character. If
  NULL a title will be constructed.}

  \item{title.show}{Whether to show the title. T/F.}

  \item{panel.num}{A number or letter to display in the
  upper left hand corner of the plot when the plot will be
  used as part of a multi-frame graphic (as in
  publications). Character}

  \item{panel.num.inside.plot}{Whether to display panel.num
  inside the plot area itself, as opposed to the title
  area. T/F}

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

  \item{...}{Additional parameters that will be passed to
  the \code{\link{plot}} function}
}
\value{
  A list of lists, one for each plot containing the
  filename (NULL if no png made), the image dimensions (or
  NULL), the descriptive text, the id, and a list of the
  svals
}
\description{
  This will find the value of s that will result in the
  time term of TSD being approximately equal to the
  distance term
}

