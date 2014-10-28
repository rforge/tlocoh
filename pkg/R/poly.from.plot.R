poly.from.plot <- function(draw.poly=TRUE, status=TRUE) {
  if (dev.cur()==1) stop("To use this function, a plot window must be active")
  if (status) cat(cw("This will return a two-column data frame of the xy coordinates of points selected on the current plot window using a mouse. The polygon will be automatically 'closed' (first point repeated at the end). Click on the plot, and right-click when done \n"))
	flush.console()
  pts <- locator(type="l")
  if (is.null(pts)) stop("Didn't get a point, try again.")
  pts <- data.frame(x=pts$x, y=pts$y)
  pts <- rbind(pts, pts[1,])
  if (draw.poly) points(pts, type="l")
  return(pts)
}
