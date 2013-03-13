\name{plot.locoh.lxy}
\alias{plot.locoh.lxy}
\title{plot.locoh.lxy}
\usage{
  plot.locoh.lxy(lxy, id = NULL, cex = 0.8,
    show.start = TRUE, show.end = TRUE,
    col = c("auto", "gray80")[1], connect.dots = TRUE,
    overlay = FALSE, show.aux = TRUE, status = TRUE,
    title = NULL, title.show = TRUE, axes.show = TRUE,
    axes.titles = axes.show, axes.ticks = axes.show,
    mar = c(if (axes.titles || axes.ticks) 3.3 else 0.5, if (axes.titles || axes.ticks) 3.2 else 0.5, if (title.show) 3.2 else 0.5, 0.5),
    mgp = c(2, 0.7, 0), lo.save = TRUE, panel.num = NULL,
    panel.num.inside.plot = !title.show, png.fn = NULL,
    png.dir = NULL, png.dir.make = TRUE, png.width = 800,
    png.height = png.width, png.overwrite = TRUE,
    png.pointsize = 12 + (png.width - 480)/80,
    tiff.fn = NULL, tiff.bands = c(3, 2, 1),
    tiff.pct = FALSE, tiff.buff = 500,
    tiff.fill.plot = TRUE, layers = NULL, shp.csv = NULL,
    xlim = NULL, ylim = NULL, ...)
}
\description{
  Multi-purpose plotting function for a LoCoH-xy object
}

