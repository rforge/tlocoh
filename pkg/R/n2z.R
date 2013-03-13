#' Convert nulls to zero
#' @export

n2z <- function(x) {
    ## Converts nulls to zeros
    if (is.null(x)) {
        return(0)
    } else {
        return(x)
    }
}
