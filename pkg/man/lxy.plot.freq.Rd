\name{lxy.plot.freq}
\alias{lxy.plot.freq}
\title{Plots of sampling frequency}
\usage{
  lxy.plot.freq(lxy, id = NULL, deltat.by.date = F,
    samp.by.date = F, cp = F, cp.min = 0, cp.max = 1,
    time.unit = c("auto", "sec", "min", "hour", "day", "week", "month", "year")[1],
    xlim = NULL, ylim = NULL,
    ylim.deltat.by.date = c(0, 0.999), sbd.y.jiggle = 0.1,
    sbd.vline = NULL,
    sbd.vline.trunc.units.to = c("secs", "mins", "hours", "days")[4],
    cex.pts = 0.3, desc = c(0, 1, 3)[2], cex.desc = 0.8,
    col.desc = "darkgreen", title = NULL,
    title.show = TRUE, subtitle = "auto",
    mar = c(2.8, 2.8, if (title.show) 2.8 else 0.5, 0.5),
    mgp = c(1.8, 0.5, 0), figs.per.page = NULL,
    status = TRUE, panel.num = NULL,
    panel.num.inside.plot = !title.show, png.dir = NULL,
    png.dir.make = TRUE, png.width = 800,
    png.height = png.width, png.overwrite = TRUE,
    png.pointsize = 12 + (png.width - 480)/80, ...)
}
\arguments{
  \item{lxy}{A LoCoH-xy object}

  \item{id}{The id(s) of the individual(s) to include in
  the plot}

  \item{deltat.by.date}{Produce a plot of the sampling
  interval by date, T/F}

  \item{cp}{Produce a plot of the cummulative proportion
  curve of sampling frequencies, T/F}

  \item{cp.min}{The minimum sampling frequency (expressed
  as a proportion of tau) shown on the plot}

  \item{cp.max}{The maximum sampling frequency (expressed
  as a proportion of tau) shown on the plot}

  \item{time.unit}{A unit of time that will be used on the
  y-axis when plotting frequency by date}

  \item{xlim}{The range for the x-axis. Can be a
  two-element vector (min, max), NULL (range will be
  computed from the data), or "same" (same range will be
  used for all individuals).}

  \item{ylim}{The range for the y-axis.}

  \item{ylim.deltat.by.date}{A two-element numeric vector
  with values 0..1. Will interpret these values as the
  first and last percentiles of delta.t to plot}

  \item{sbd.y.jiggle}{An amount to 'jiggle' the y values to
  better see point density}

  \item{sbd.vline}{An interval of time (in seconds) for
  which vertical lines will be included on the sampling
  interval by date plot, to help see the dates}

  \item{sbd.vline.trunc.units.to}{A character "secs",
  "mins", "hours" or "days" specifying where to draw the
  first vertical lines. quantiles of delta.t when setting
  the range of ylim when \code{deltat.by.date=TRUE}}

  \item{cex.pts}{The expansion factor for points on the
  plot}

  \item{desc}{Which side to display automatically generated
  desciptive text (e.g. caption). 0=none, 1=bottom, 3=top.}

  \item{cex.desc}{The expansion factor for the descriptive
  text. Numeric value.}

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
  filename (NULL if no png made), the image dimensions
  (NULL), descriptive text, and id
}
\description{
  Creates plots of the temporal extent and sampling
  frequency of a LoCoH-xy object
}
\note{
  Plotting the cummulative percentage of sampling
  frequencies (code{cp=TRUE}) can help determine a
  threshhold below which pairs of points should be
  considered to be part of the same 'burst' (see
  \code{\link{lxy.thin.bursts}}). For example, if most
  points are sampled 1 hour apart, but there are a few
  bursts of points that were only 2 minutes apart, the
  cumuulative percentage of sampling frequencies will
  reveal a cluster of sampling intervals around 2 minutes
  and the majority around 1 hour.

  Plotting the sampling frequency over time can help
  identify gaps in the data as well as frequencies for
  harmonising sampling frequency
}
\seealso{
  \code{\link{lxy.thin.bursts}},
  \code{\link{lxy.thin.byfreq}}
}

