\name{lxy.plot.ptsh}
\alias{lxy.plot.ptsh}
\title{Plots the proportion of time-selected hulls over 's'}
\usage{
  lxy.plot.ptsh(lxy, id = NULL, ptsh.idx = NULL,
    use.nn = FALSE, k = NULL, r = NULL, a = NULL,
    slim = NULL, desc = c(0, 1, 3)[2], cex.desc = 0.8,
    col.desc = "darkgreen", title = NULL,
    title.show = TRUE,
    legend = c("none", "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right", "center")[2],
    mar = c(3, 3, if (title.show) 2.8 else 0.7, 0.5),
    mgp = c(1.8, 0.5, 0), figs.per.page = NULL,
    panel.num = NULL, panel.num.inside.plot = !title.show,
    png.dir = NULL, png.dir.make = TRUE, png.width = 800,
    png.height = png.width, png.overwrite = TRUE,
    png.pointsize = 12 + (png.width - 480)/80, ...)
}
\arguments{
  \item{lxy}{A LoCoH-xy object}

  \item{id}{The name(s) of individuals to plot}

  \item{ptsh.idx}{The index number of the saved ptsh table
  to use (ignored if \code{use.nn=TRUE})}

  \item{use.nn}{Whether to create the plot based on nearest
  neighbor sets (as opposed to saved ptsh tables, see
  Details). T/F.}

  \item{k}{Value for the k method if creating the plot
  based on nearest neighbor tables (ignored if
  \code{use.nn=FALSE})}

  \item{r}{Value for the r method if creating the plot
  based on nearest neighbor tables (ignored if
  \code{use.nn=FALSE})}

  \item{a}{Value for the a method if creating the plot
  based on nearest neighbor tables (ignored if
  \code{use.nn=FALSE})}

  \item{slim}{The lower and upper bounds for s, two-element
  numeric vector}

  \item{desc}{Which side to display automatically generated
  desciptive text (e.g. caption). 0=none, 1=bottom, 3=top.}

  \item{cex.desc}{The expansion factor for the descriptive
  text. Numeric value.}

  \item{col.desc}{The color of the descriptive text. Color
  value.}

  \item{title}{The title to be displayed. Character. If
  NULL a title will be constructed.}

  \item{title.show}{Whether to show the title. T/F.}

  \item{legend}{A character object specifying where to put
  the legend (ignored when \code{use.nn=TRUE})}

  \item{mar}{The plot margins. A four item numeric vector}

  \item{mgp}{The distance away from the edge of the plot
  for the 1) label, 2) tick marks, and 3) axis line. A
  three-item numeric vector}

  \item{figs.per.page}{The number of plots per page.}

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
  NULL), the descriptive text, the id, and a matrix of the
  values
}
\description{
  Plots the proportion of time-selected hulls over 's'
}
\details{
  This function will plot the proportion of total hulls
  that are 'time-selected'. Time-selected means all of the
  nearest neighbors are were sampled sequentially in time.
  This is one end of the spectrum as far as nearest
  neighbor identification goes, the other being
  space-selected (i.e., time has no bearing). The \code{s}
  parameter in the TSD equation determines the degree to
  which time plays a role in the point-to-point 'distance'.
  When \code{s=0}, time plays no role in TSD and TSD is
  equivalent to Euclidean distance. As \code{s} increases,
  time plays a bigger and bigger role until eventually
  nearest neighbor selection is equivalent to selection
  points based only on their separation in time.

  In order to plot the proportion of time-selected hulls
  (ptsh), nearest neighbors must have already been
  identified. This can be done in one of two ways. The
  \code{\link{lxy.ptsh.add}} function will compute ptsh for
  different values of s using a random sample of points (to
  save time), automatically picking values of s such that
  ptsh is close to target values provided by the user.
  Alternately, one can identify nearest neighbors for
  different values of \code{s} using
  \code{\link{lxy.nn.add}} and then run this function with
  \code{use.nn=TRUE}. The main difference between
  \code{\link{lxy.ptsh.add}} and \code{\link{lxy.nn.add}}
  is that \code{\link{lxy.ptsh.add}} finds nearest
  neighbors for a random sample of points, and doesn't
  actually save the nearest neighbor information for
  individual points, whereas \code{\link{lxy.nn.add}}
  identifies and saves nearest neighbor information for
  each and every point.
}
\examples{
data(toni.lxy)
toni.lxy <- lxy.ptsh.add(toni.lxy)
lxy.plot.ptsh(toni.lxy)
}
\seealso{
  \code{\link{lxy.ptsh.add}}, \code{\link{lxy.nn.add}}
}

