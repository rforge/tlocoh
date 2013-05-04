#' Trim leading and/or trailing blanks from a character object
#'
#' @param str Input string
#' @param side Which side(s) of the input string to trim: 'left', 'right', or 'both'
#' @export

trim <- function (str, side = "both") {
    ## Adapted from package 'stringr'
    stopifnot(length(side) == 1)
    side <- match.arg(side, c("left", "right", "both"))
    pattern <- switch(side, left = "^\\s+", right = "\\s+$", both = "^\\s+|\\s+$")
    gsub(pattern=pattern, replacement="", x=str)
}
