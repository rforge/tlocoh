#' Split a character object into multiple lines 
#'
#' Split a character object at a specific character to wrap to multiple lines for plotting
#'
#' @param object Input character vector (should be length 1)
#' @param size The maximum number of characters in one piece
#' @param char The character to split the input string at
#' @param separator The character to use as a separator in the returned object
#' @param ... Other arguments (unused)
#'
#' @details
#' This will take a long character object and split it into pieces at character \code{char} such that the length of each piece is <= \code{size}.
#' If \code{separator} is passed, the individual pieces will be concatenated using the \code{separator} character and returned as a character vector of length 1.
#' Otherwise each piece will be returned as a separate element of a character vector.
#'
#' @export


strSplitAtChar <- function(object, size, char=".", separator=paste("\n", char, sep=""), ...) {

    str <- object; rm(object)
    str.split <- strsplit(str, split=char, fixed=TRUE)
    str.split.cumlen <- cumsum(sapply(str.split[[1]], function(x) length(char) + nchar(x)))
    build <- NULL
    
    while (max(str.split.cumlen) > 0) {
        str.split.idx <- which(str.split.cumlen > 0 & str.split.cumlen <= size)
        build <- c(build, paste(str.split[[1]][str.split.idx], collapse=char, sep=""))
        str.split.cumlen <- str.split.cumlen - str.split.cumlen[max(str.split.idx)]
    }

    if (!is.null(separator)) build <- paste(build, collapse=separator, sep="")
    
    return(build)

}
