#' Get x and/or y values from a plot using the mouse
#'
#' @param axis Which axes to return values for, character vector
#' @param round.to An integer number that values will be rounded to. For example if \code{round.to=100}, values will be rounded to the nearest 100.
#' @param print Whether to print the values in the console, T/F
#'
#' @details This function allows you to get the x and/or y values by clicking on the active plot window. When you're done
#' clicking, right-click and select 'Stop' from the pop-up menu.
#'
#' @return A vector or matrix of the x and/or y values where the mouse was clicked.
#'
#' @export

get.vals <- function(axis=c("x","y")[1], round.to=1, print=TRUE) {

    if (FALSE %in% (axis %in% c("x","y"))) stop("Unknown value for axis")
    if (dev.cur() == 1) stop("No active plot window")
    cat("Click on the active plot to grab the ", paste(axis, collapse=" and ", sep=""), " values. When done, right-click and select 'Stop' \n");flush.console()
    xy <- locator()
    
    xyvals <- cbind(x=if ("x" %in% axis) xy$x else NULL, y=if ("y" %in% axis) xy$y else NULL)[ , , drop=TRUE]
    if (round.to > 0) xyvals <- round(xyvals / round.to) * round.to
    if (print) print(xyvals)
    bringToTop(-1)
    return(xyvals)
}
