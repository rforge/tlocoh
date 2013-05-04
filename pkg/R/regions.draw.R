regions.draw <- function(n, col=NULL, draw.reg=TRUE, alpha=0.5, prompt.labels=TRUE) {
    ## Returns a list with n items where each item is a list containing two elements:
    ##     1) a data frame of points of a closed polygon drawn by the user on the current plot window
    ##     2) a color object (string)
    ## The user will be given the chance to picks points for the polygon from the plot window using the mouse
    ## This requires a plot window to be active
    ## The purpose of this function is to help build a "regions" object which is one piece of a scatter plot 
    
    if (dev.cur()==1) stop("To use this function, a plot window must be active")
    
    lst <- list()
    if (is.null(col)) {
        col <- rainbow(n, end=5/6)
        col.str <- rep("", n)
    } else {
        if (length(col) != n) stop("The number of colors must equal the number of regions to be drawn")
        col.str <- paste(" (", as.character(col), ")", sep="")
    }
    
    cat(cw("To define a polygon region, click on the active plot window using a mouse. When finished, right-click and select 'stop'. The polygon will be automatically 'closed'\n\n"))
    for (i in 1:n) {
        cat("Please draw polygon #", i, " of ", n, col.str[i], "\n", sep="")
      	flush.console()
        pts <- poly.from.plot(draw.poly=FALSE, status=FALSE)
        if (draw.reg) {
          col.use <- col2rgb(col[i])
          col.use <- rgb(red=col.use[1], green=col.use[2], blue=col.use[3], maxColorValue = 255, alpha=alpha * 255)
          polygon(pts, col=col.use)
        }
        if (prompt.labels) {
            bringToTop(-1)
            label <- readline(prompt = paste("Label for this region [", i, "]: ", sep=""))
            if (label=="") label <- as.character(i)
        } else {
            label <- as.character(i)
        }
        lst[[i]] <- list(poly.pts=pts, col=col[i], label=label)
    }
    return(lst)
}
