#' Save a LoCoH-xy object to disk
#'
#' Saves a LoCoH-xy object to disk, constructing a file name if needed that reflects the contents
#'
#' @param lxy A \link{LoCoH-xy} object
#' @param file Optional file name
#' @param save.as The name of the saved object, if NULL the original object name will be used
#' @param dir Directory for the output file (relative to the working directory), ignored if \code{file} passed
#' @param suf A suffix that will be used in the construction of the file name, ignored if \code{file} is passed
#' @param compress Compress file (T/F)
#' @param auto.num.files Use a numeric token as part of the constructed file name to get a unique file name(T/F)
#' @param width The number of digits in auto.num.files
#' @param overwrite Overwrite an existing file 
#'
#' @export
#' @seealso \code{\link{xyt.lxy}}, \code{\link{lhs.save}}

lxy.save <- function (lxy, file=NULL, save.as=NULL, dir=".", suf=NULL, compress=TRUE, auto.num.files=TRUE, width=2, overwrite=FALSE) {

    if (!inherits(lxy, "locoh.lxy")) stop("lxy should be of class \"locoh.lxy\"")

    if (!is.null(file)) {
        fn.full <- file
        ## Strip off extension
        fn.base.full <- file.path(dir, substr(file, 0, nchar(file) - 6))
        
    } else {
        ## Build the pieces of a file name
        strids <- paste(unique(lxy[["pts"]][["id"]]), collapse="-", sep="")
        if (is.null(lxy[["pts"]][["dt"]])) {
            strdts <- ""
        } else {
            strdts <- paste(".", format(lxy[["pts"]][["dt"]][1], format = "%Y-%m-%d", tz = ""), ".", format(lxy[["pts"]][["dt"]][length(lxy[["pts"]][["dt"]])], format = "%Y-%m-%d", tz = ""), sep="")
        }

        if (!is.null(suf) && substr(suf,1,1)!=".") suf <- paste(".", suf, sep="")

        ## Construct the base of the file name (without path)
        fn.base.fileonly <- paste(strids, ".n", nrow(lxy[["pts"]]), strdts, suf, ".lxy", sep="")
        if (nchar(fn.base.fileonly) > 87) fn.base.fileonly <- paste(substr(fn.base.fileonly, 1, 77), sep="")

        if (!file.exists(dir)) stop(paste("Directory not found:", dir))
        fn.base.full <- file.path(dir, fn.base.fileonly)
        fn.full <- paste(fn.base.full, if (auto.num.files) ".01" else NULL, ".RData", sep="")

    }

    ## See if the file already exists
    if (file.exists(fn.full)) {
       if (auto.num.files) {
           # Construct a new file name by incrementing the auto-number
           i <- 1
           while (file.exists(fn.full)) {
              i <- i + 1
              fn.full <- paste(fn.base.full, if (auto.num.files) paste(".", formatC(i, flag=0, width=width), sep="") else NULL, ".RData", sep="")
              if (i > 99) stop("i > 99")
           }
       } else if (!overwrite) {
           stop("File already exists. Please try a different file name.")
       }
    }

    if (is.null(save.as)) {
        save.as <- deparse(substitute(lxy))
    } else {
        assign(save.as, lxy)
    }
    
    save(list=save.as, file=fn.full, compress = compress)
    cat("LoCoH-xy ", deparse(substitute(lxy)), " saved as ", save.as, " to:\n  ", normalizePath(path.expand(fn.full)), "\n", sep="")

}
