% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lhs.plot.revisit.R
\name{lhs.plot.revisit}
\alias{lhs.plot.revisit}
\title{Plot distribution of revisit times}
\usage{
lhs.plot.revisit(lhs, id = NULL, k = NULL, r = NULL, a = NULL,
  s = NULL, hs.names = NULL, ta.min = "auto", ta.max = NULL,
  breaks = 40, xaxis.vals = NULL, ta.min.auto.tau = 4,
  col = "gray80", figs.per.page = 1, title = NULL,
  title.show = TRUE, subtitle = TRUE, mar = c(2.8, 3.2, if
  (title.show) (if (subtitle) 3.2 else 2.3) else 0.5, 0.5), mgp = c(1,
  0.7, 0), png.fn = NULL, png.dir = NULL, png.dir.make = TRUE,
  png.width = 800, png.height = png.width, png.pointsize = 12 +
  (png.width - 480)/80, png.fn.pre = NULL, png.fn.suf = NULL,
  png.overwrite = TRUE, panel.num = NULL,
  panel.num.inside.plot = !title.show, bg = "white",
  legend.space = if (legend) 0.05 else 0, ...)
}
\arguments{
\item{lhs}{A LoCoH-hullset object}

\item{id}{The id(s) of the individual(s) to include in the plot}

\item{k}{A k-value for the number of nearest neighbors around each point to include in the plot}

\item{r}{A r-value for the number of nearest neighbors around each point to include in the plot}

\item{a}{A a-value for the number of nearest neighbors around each point to include in the plot}

\item{s}{The s value(s) of nearest neighbor sets to include in the plot. If NULL, all values will be used}

\item{hs.names}{The name(s) of saved hullsets to plot}

\item{ta.min}{The minimum time away (in seconds) to include on the histogram , can also be 'auto'}

\item{ta.max}{The maximum time away (in seconds) to include. If NULL then no upper limit will be imposed}

\item{breaks}{A value for 'breaks' that will be passed to the \code{hist} function (see help function for breaks)}

\item{xaxis.vals}{A numeric vector of time-away values (in seconds) that will be labeled on the x-axis.}

\item{ta.min.auto.tau}{The minimum time away to include in 'auto' expressed as the number of median sampling intervals. Ignored if \code{ta.min} is not \code{'auto'}.}

\item{col}{A color value for the histogram bars}

\item{figs.per.page}{Number of plots per page}

\item{title}{The title to be displayed. Character. If NULL a title will be constructed.}

\item{title.show}{Whether to show the title. T/F.}

\item{subtitle}{Whether to add a subtitle to the automatically constructed title (when \code{title=NULL}, otherwise ignored)}

\item{mar}{The plot margins. A four item numeric vector}

\item{mgp}{The distance away from the edge of the plot for the 1) label, 2) tick marks, and 3) axis line. A three-item numeric vector}

\item{png.fn}{A filename for a PNG file}

\item{png.dir}{The directory for a PNG file (filename will be constructed automatically). Ignored if png.fn is passed}

\item{png.dir.make}{Whether to create png.dir if it doesn't exist. T/F}

\item{png.width}{The width of the PNG image}

\item{png.height}{The height of the PNG image}

\item{png.pointsize}{The pointsize (in pixels) for the PNG image, equivalent to the height or width of a character in pixels (increase to make labels appear larger)}

\item{png.fn.pre}{A prefix that will be used in the construction of the PNG filename. Ignored if png.fn is passed.}

\item{png.fn.suf}{A suffix that will be used in the construction of the PNG filename. Ignored if png.fn is passed.}

\item{png.overwrite}{Whether to overwrite an existing PNG file if it exists. T/F.}

\item{panel.num}{A number or letter to display in the upper left hand corner of the plot when the plot will be used as part of a multi-frame graphic (as in publications). Character}

\item{panel.num.inside.plot}{Whether to display panel.num inside the plot area itself, as opposed to the title area. Ignored if panel.num is NULL. T/F}

\item{bg}{A color value for the plot background}

\item{legend.space}{The amount of additional space on the lower end of the x-axis to make room for the legend. Expressed as a proportion of the range of the x-axis values}

\item{...}{Additional parameters that will be passed to the \code{\link{hist}} function}

\item{legend}{Whether to include a legend. T/F.}
}
\value{
A list of objects of class histogram (one for each hullset plotted)
}
\description{
Plots a histogram of the revisit times a LoCoH-hullset area
}
\details{
This function will plot a histogram of the revisitation times for a hullset. Revisitation time is simply the time between points enclosed in a hull. Basically, all points enclosed by each hull are examined and their time intervals computed. This may help reveal where there are natural temporal cycles in revisitation, for example if you see a spike in revisitation around 24 hours there may be daily revisitation pattern in some of the hulls. You can specify the minimum amount of time-away to show in the histogram (e.g., if enclosed points that are separated by the median sampling interval are of little interest), as well as the maximum time-away period. Note that revisition metrics do *not* have to be computed for the hullset for the histogram to be computed.
}
\seealso{
\code{\link{lhs.visit.add}}
}
