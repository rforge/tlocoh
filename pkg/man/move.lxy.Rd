\name{move.lxy}
\alias{move.lxy}
\title{Converts a Move object to a LoCoH-xy object}
\usage{
  move.lxy(move.obj, use.utm = FALSE, xcoord = NULL,
    ycoord = NULL, proj = NULL, anv.flds = NULL,
    ptid = NULL, del.dup.xyt = TRUE, dup.dt.check = TRUE,
    dt.int.round.to = 0.1, tau.diff.max = 0.02,
    req.id = TRUE, warn.latlong = TRUE, status = TRUE)
}
\arguments{
  \item{move.obj}{A object of class Move or MoveStack.}

  \item{use.utm}{Import UTM coordinates if found (T/F).}

  \item{xcoord}{The name of a field in the data table for
  the x-coordinates. If NULL, the default coordinates will
  be used.}

  \item{ycoord}{The name of a field in the data table for
  the y-coordinates. If NULL, the default coordinates will
  be used.}

  \item{proj}{Projection object of class
  \code{\link{CRS-class}}. Used *only* to specify the
  coordinate system when the coordinates being imported are
  coming from the data table}

  \item{anv.flds}{The name of field(s) in the Move object
  data table that will be imported as ancillary variables.
  Can also be 'all'.}

  \item{ptid}{The name of a numeric field in the Move
  object data table containing unique numeric values for
  each point. If NULL new point id values will be created.}

  \item{del.dup.xyt}{Whether to delete duplicate rows with
  the same x, y, dt, and id value (T/F).}

  \item{dup.dt.check}{Whether to check to make sure there
  are no duplicate date values for the same id.}

  \item{dt.int.round.to}{The proportion of the median
  sampling frequency that time intervals will be rounded to
  when computing the frequency table of sampling intervals
  (no change is made to the time stamps).}

  \item{tau.diff.max}{The maximum deviation from tau
  (expressed as a proportion of tau) that a point-to-point
  time difference must fall within for the point-to-point
  distance to be included in the calculation of the median
  step length.}

  \item{req.id}{Require a value for id (T/F).}

  \item{warn.latlong}{Show a warning message if it detects
  coordinates may be in latitude-longitude (T/F).}

  \item{status}{Show status messages (T/F).}
}
\value{
  A \link{LoCoH-xy} object
}
\description{
  Converts a Move or MoveStack object from the move package
  to a LoCoH-xy object
}
\note{
  Move and MoveStack are formal classes from the
  \emph{Move} package for storage of movement data for a
  single (Move) or multiple (MoveStack) indviduals.
  \code{move.lxy} will convert these objects to
  \link{LoCoH-xy} objects. To import the attribute data
  associated with each point (termed ancillary variables in
  T-LoCoH), set \code{anv.flds} to the names of the
  field(s) you wish to import (or \code{anv.flds="all"}).
  Note that the Move and MoveStack classes store a lot more
  metadata than LoCoH-xy objects, including unused records,
  which are not supported by the \link{LoCoH-xy} data class
  and are not imported.
}
\examples{
require(move)
leroy.move <- move(system.file("extdata","leroy.csv.gz",package="move"))
class(leroy.move)
leroy.lxy <- move.lxy(leroy.move, anv.flds=c("ground.speed", "heading"))
summary(leroy.lxy)
}
\seealso{
  \code{\link{xyt.lxy}}
}

