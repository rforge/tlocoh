#' Convert a comma-separated string to a vector
#'
#' @param x The vector or character string to be evaluated 
#' @param n2z Convert null values to zero or an empty string (depending on \code{type}
#' @param type The data type: \code{numeric} or \code{character} 
#' @param sort.res Whether to sort the result. T/F
#'
#' @return A vector
#'
#' @export

vectorize.parameter <- function (x, n2z=FALSE, type=c("numeric", "character")[1], sort.res=TRUE) {

    if (is.null(x)) {
       if (n2z) {
          return(if (type=="numeric") 0 else "")
       } else {
          return(x)
       }
    }
    if (is.character(x)) x <- get(paste("as.", type, sep=""))(unlist(strsplit(strTrim(x), " *, *")))
    
    return(if (sort.res) sort(x) else x)
}
