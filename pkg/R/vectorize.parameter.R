#' vectorize.parameter

#' @export

vectorize.parameter <- function (x, n2z=FALSE, type=c("numeric", "character")[1], sort.res=TRUE) {

    ## converts a comma delimited character string to a vector with option to sort
    ## returns a sorted version of the vector

    if (is.null(x)) {
       if (n2z) {
          return(if (type=="numeric") 0 else "")
       } else {
          return(x)
       }
    }
    if (is.character(x)) x <- get(paste("as.", type, sep=""))(unlist(strsplit(trim(x), " *, *")))
    
    return(if (sort.res) sort(x) else x)
}
