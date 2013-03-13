#' trim
#' @export

trim <-
function (str, side = "both") {
    ## Adapted from package 'stringr'
    #str <- check_string(str)
    stopifnot(length(side) == 1)
    side <- match.arg(side, c("left", "right", "both"))
    pattern <- switch(side, left = "^\\s+", right = "\\s+$", both = "^\\s+|\\s+$")
    gsub(pattern=pattern, replacement="", x=str)
}
