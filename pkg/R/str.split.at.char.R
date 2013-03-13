#' str.split.at.char
#' @export
str.split.at.char <-
function(str, size, char=".", separator=paste("\n", char, sep="")) {

    ## Takes a long string and splits it into pieces at character char such that the length of each piece is <= size
    ## If separator is passed, concatenates the pieces into a single character, else returns 
    ## each piece as a separate element of a character vector

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
