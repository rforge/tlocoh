\name{lxy.exp.mov}
\alias{lxy.exp.mov}
\title{Create a Quicktime animation from a LoCoH-xy object}
\usage{
  lxy.exp.mov(lxy, id = NULL, all.ids.at.once = TRUE,
    all.ids.col.unique = all.ids.at.once,
    all.ids.col = NULL,
    all.ids.legend = c("bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right", "center")[5],
    all.ids.legend.cex = 0.8, dt.start = NULL,
    dt.end = NULL,
    frame.method = c("auto", "time", "location")[1],
    frame.rtd = "auto", xlim = NULL, ylim = NULL,
    dt.label = TRUE, dt.label.col = "black",
    dt.label.x = NULL, dt.label.y = NULL, title = NULL,
    title.show = TRUE, axes.show = TRUE,
    axes.ticks = axes.show, axes.titles = FALSE,
    mar.map = c(0.7 + (if (axes.ticks) 0.9 else 0) + (if (axes.titles) 1.3 else 0), 0.5 + (if (axes.ticks) 0.9 else 0) + (if (axes.titles) 1.3 else 0), if (title.show) 2.1 else 0.5, 0.5),
    mgp.map = c(0.4 + if (axes.ticks) 1.2 else 0, 0.4, 0),
    col.xys.active = "red", col.xys.background = "gray80",
    cex.xys.active = 1.5, cex.xys.background = 0.5,
    tz.local = NULL, tz.local.check = TRUE,
    col.by.hour.of.day = FALSE,
    col.hod = colorRampPalette(colors()[c(24, 30, 553, 121, 26, 121, 553, 30, 24)])(24),
    width = if (screen.test) 7 else 608, height = NULL,
    max.frames = NULL,
    png.pointsize = 16 + (width - 480)/80,
    screen.test = FALSE, tmp.dir = NULL,
    tmp.files.delete = TRUE, prompt.continue = TRUE,
    fn.mov = NULL, fn.mov.dir = getwd(),
    fn.mov.exists = c("auto.increment", "overwrite", "stop", "ask")[1],
    duration = NULL, fps = NULL, skip = NULL,
    ffmpeg = "ffmpeg.exe", create.mov = TRUE,
    info.only = TRUE, shp.csv = NULL, layers = NULL,
    bg2png = !is.null(layers),
    crop.layers.to.extent = TRUE, date.bar = 0.85,
    date.bar.bins = 12, col.db = "darkblue",
    cex.axis.db = 0.7, beep = FALSE, report.time = TRUE,
    status = TRUE)
}
\arguments{
  \item{lxy}{A \link{LoCoH-xy} object}

  \item{id}{The id value(s) to be on the plot}

  \item{all.ids.at.once}{Display all the individual ids
  simultaneously. If False, an animation will be created
  for each id. T/F.}

  \item{all.ids.col.unique}{Whether to use unique colors
  for each individual when plotting multiple individuals
  simultaneously, T/F}

  \item{all.ids.col}{A named list of color values; the
  element names must match the name(s) of the ids}

  \item{all.ids.legend}{Where to place a legend showing the
  color of each id (for an animation showing the movement
  of multiple individuals simultaneously): 'bottomright',
  'bottom', 'bottomleft', 'left', 'topleft', 'top',
  'topright', 'right', or 'center'. May also be
  \code{NULL}, in which case the legend will not be
  displayed. Ignored if \code{ids.legend.bln=FALSE} or
  \code{all.ids.col.unique = FALSE}.}

  \item{all.ids.legend.cex}{The character expansion factor
  for the id legend. See parameter \code{all.ids.legend}
  above.}

  \item{dt.start}{The starting date-time. An object of
  class POSIXt or one that can be coerced to POSIXt. If
  NULL, the earliest date-time in the series will be used.}

  \item{dt.end}{The ending date-time. An object of class
  POSIXt or one that can be coerced to POSIXt. If NULL, the
  last date-time in the series will be used.}

  \item{frame.method}{How each frame should be defined
  temporally, "time" - each frame represents a fixed amount
  of time, "location" each frame is a point in the series}

  \item{frame.rtd}{The real-time duration of each frame (in
  seconds). If "auto" (default), the lowest median sampling
  frequency will be used}

  \item{xlim}{A two-element numeric vector for the range of
  the x-axis (in map units)}

  \item{ylim}{A two-element numeric vector for the range of
  the y-axis (in map units)}

  \item{dt.label}{Add a label for the date of the frame}

  \item{dt.label.col}{A color value/name for the date label
  (ignored if \code{dt.label=FALSE})}

  \item{dt.label.x}{The x-coordinate for the date label
  (ignored if \code{dt.label=FALSE})}

  \item{dt.label.y}{The y-coordinate for the date label
  (ignored if \code{dt.label=FALSE})}

  \item{title}{A title for the map}

  \item{title.show}{Whether to show the title. T/F.}

  \item{axes.show}{Whether to show the axes (ticks, labels
  and titles). Can be over-written by \code{axes.ticks} and
  \code{axes.titles}. T/F.}

  \item{axes.ticks}{Whether to show the tick marks and
  labels on the axes. T/F.}

  \item{axes.titles}{Whether to show axes titles. T/F.}

  \item{mar.map}{Margin settings for the map, see
  \code{\link{par}}}

  \item{mgp.map}{Locations of the axes elements for the
  map, see \code{\link{par}}}

  \item{col.xys.active}{Color of the active point. Ignored
  if \code{col.by.hour.of.day = TRUE}.}

  \item{col.xys.background}{Color of the non-active points.
  To hide non-active points, set to \code{NA}.}

  \item{cex.xys.active}{The character expansion factor of
  the active point}

  \item{cex.xys.background}{The character expansion factor
  of non-active points}

  \item{tz.local}{The name of the time zone of the study
  area}

  \item{tz.local.check}{Check whether tz.local is a valid
  timezone name (not implemented) T/F.}

  \item{col.by.hour.of.day}{Whether to color the active
  point by the hour of day (i.e., dark colors at night,
  orange for day time locations). T/F}

  \item{col.hod}{A vector of 24 color values used to
  symbolize the color of the active point by the hour of
  day. Ignored if \code{col.by.hour.of.day = FALSE}.}

  \item{screen.test}{Create up to three sample frame(s) on
  the screen (instead of PNG files)}

  \item{width}{The width of each frame in pixels (if
  \code{screen.test=FALSE}) or inches (if
  \code{screen.test=TRUE}).}

  \item{height}{The height of each frame in pixels (if
  \code{screen.test=FALSE}) or inches (if
  \code{screen.test=TRUE}).}

  \item{max.frames}{The maximum number of frames to
  produce.}

  \item{png.pointsize}{The pointsize (in pixels) for the
  PNG image, equivalent to the height or width of a
  character in pixels (increase to make labels appear
  larger)}

  \item{tmp.dir}{A directory where temporary PNG files for
  each frame will be created, character.}

  \item{tmp.files.delete}{Delete the temporary PNG files
  when done, T/F}

  \item{prompt.continue}{Whether to present a summary of
  the encoding settings and get user confirmation before
  continuing, T/F}

  \item{fn.mov}{The path and filename of output Quicktime
  *.mov file. If NULL a filename will be automatically
  generated}

  \item{fn.mov.dir}{The directory where the animation will
  be saved (ignored if a value for \code{fn.mov} is
  passed)}

  \item{fn.mov.exists}{What to do if the animation file
  already exists: "auto.increment", "overwrite", "stop", or
  "ask".}

  \item{duration}{The desired duration of the animation (in
  seconds)}

  \item{fps}{A numeric value for frames per second}

  \item{skip}{Output every nth frame. To include every
  frame set skip=1. Integer.}

  \item{ffmpeg}{The name of the ffmpeg file. See notes.}

  \item{create.mov}{Whether to actually create the mov
  file. Set to FALSE preview a few frames without actually
  encoding them.}

  \item{info.only}{Only return info}

  \item{shp.csv}{The path and filename of a csv file that
  contains information about shapefiles, including layer
  names, file, and symbology.}

  \item{layers}{The name(s) of layers in shp.csv to display
  in the background. Will be displayed using the symbology
  in shp.csv. Character vector or comma delimited string.}

  \item{bg2png}{Save the plot background elements as a
  static raster image (to improve speed), ignored if
  \code{screen.test=TRUE}}

  \item{crop.layers.to.extent}{Whether to crop the
  shapefile layers to the view extent (may speed up drawing
  time)}

  \item{date.bar}{The height of the lower section of the
  plot to devote to the time bar, in inches. To hide the
  time bar completely, set \code{date.bar=0}.}

  \item{date.bar.bins}{The target number of bins (tick
  marks + 1) on the time bar (integer)}

  \item{col.db}{A single color value for the date bar axes
  / tick labels, character}

  \item{cex.axis.db}{Character expansion factor for the
  labels on the date bar axis.}

  \item{beep}{Beep when one, T/F}

  \item{report.time}{Show the time taken when done, T/F}

  \item{status}{Show progress bar and status messages}
}
\value{
  A list with information about each *.mov file created.
  Each element of the list is another list with two
  elements: \code{fn} (the full filename) and \code{dim} (a
  two-element numeric vector with the frame width and
  height). If no *.mov file(s) were created, returns NULL.
}
\description{
  Create a Quicktime animation from a LoCoH-xy object
}
\note{
  To create the animation, two and only two of the
  following parameters must be passed: \code{duration},
  \code{fps}, and \code{skip}. The third parameter will be
  computed based on the other two. To include every frame,
  pass \code{fps}, set \code{skip=1}, and leave
  \code{duration} out.

  Larger values for \code{fps} will result in the animation
  running 'faster'. Values between 10 and 20 often work
  well; beyond 30 fps the eye can't keep up with the motion
  Note if you pass values for \code{fps} and
  \code{duration}, an appropriate value for \code{skip}
  will be computed but the final duration of the animation
  may not be exactly equal to \code{duration} because only
  interger values of \code{skip} are allowed.

  One will normally want to run the function a few times
  without actually encoding to tweak the frame design
  (e.g., where the date label and legend appear). To see
  what a frame in the output will *approximately* look
  like, set \code{screen.test=TRUE}. Once the screen sample
  looks good, next set \code{max.frames=3},
  \code{tmp.dir="."} (or another folder),
  \code{create.mov=FALSE}, and
  \code{tmp.files.delete=FALSE}. This will generate a few
  sample frames as PNG files but not delete them so you can
  inspect them using an image viewer. Once these look good,
  create the full animation by setting
  \code{create.mov=TRUE} and \code{max.frames=NULL}.

  If frame.method is 'auto', the script will use time-based
  frames when multiple individuals are being animated
  simultaneously, and location-based frames otherwise.

  \code{duration} (the duration of the animation in
  seconds) should not be confused with \code{frame.rtd}
  which is the real-time duration of a single frame in
  seconds (e.g., \code{frame.rtd=3600} means each frame
  will represent 1 hour).

  If \code{date.bar} is too small or too large, you might
  get a 'margins too large' error. Try values around 1, or
  hide the date bar completely by setting
  \code{date.bar=0}.

  The output animation is encoded in QuickTime format. The
  Quicktime file is encoded using Quicktime's 'animation'
  codec, a lossless format that 'scrubs' well (i.e., you
  can drag the scroll bar to view frame by frame). Encoding
  requires installing the open source encoding program
  ffmpeg. ffmpeg is a command line program that Linux and
  Windows users can download from
  http://ffmpeg.org/download.html. Windows users should
  save the ffmpeg.exe file to the working directory or a
  directory on Window's path environment variable (e.g.,
  c:\\windows). Mac users can download ffmpegX from
  http://ffmpegx.com/download.html but this has not been
  tested (pass a value to \code{ffmpeg}).

  If ffmpeg is not available, you can still use this
  function to generate the individual frames and then use
  another utility (e.g., ImageMagick, Quicktime Pro) to
  combine the frames into a video file. For best results
  use a 'lossless' compression method in the encoding
  program. To create the individual frames only for
  encoding with another utility, set \code{tmp.dir="."}
  (the working directory) and
  \code{tmp.files.delete=FALSE}.

  If \code{fn.mov.exists = "auto.increment"}, a two-digit
  number will be appended to the *.mov filename to avoid
  overwriting an existing file
}

