#' Save a LoCoH-hullset object to disk
#'
#' Saves a LoCoH-hullset object to disk, constructing a filename if needed that reflects the contents
#'
#' @param lhs A LoCoH-hullset object
#' @param file Optional filename. If omitted a sensible filename will be constructed
#' @param dir Directory for the output file (relative to the working directory), ignored if \code{file} passed
#' @param suf A suffix that will be used in the construction of the filename, ignored if \code{file} is passed
#' @param compress Compress file. T/F
#' @param auto.num.files Use a number as part of the constructed filename. T/F
#' @param width The number of digits of the auto-number token in the filename (ignored if \code{auto.num.files=F})
#' @param save.as The name of the object when saved (default is the same as the original)
#'
#' @export
#' @seealso \code{\link{lxy.save}}

lhs.save <- function (lhs, file=NULL, dir=".", suf=NULL, compress=TRUE, auto.num.files=TRUE, width=2, save.as=NULL) {

    if (!inherits(lhs, "locoh.lhs")) stop("lhs should be of class \"locoh.lhs\"")
    if (!require(sp)) stop("package sp required")
    
    if (is.null(file)) {
        ## Build the pieces of a file name
        id.df <- unique(do.call(rbind, lapply(lhs, function(x) data.frame(id=x[["id"]], num.pts=length(x[["pts"]])))))
        fn.id <- tolower(paste(id.df[["id"]], ".n", id.df$num.pts, collapse = ".", sep=""))

        fn.params <- NULL
        for (param.str in c("s", "k", "a", "r")) {
            assign(param.str, unlist(unique(sapply(lhs, function(x) x[[param.str]]))))
            if (!is.null(get(param.str))) {
                fn.params <- paste(fn.params, ".", param.str, if (length(get(param.str))>5) paste(get(param.str)[1], "-x", length(get(param.str))-2, "-", get(param.str)[length(get(param.str))], sep="") else paste(sort(get(param.str)), collapse="-", sep=""), sep="")
            }
        }

        fn.save.nn <- if (is.null(lhs[[1]][["nn"]])) "" else ".nn"
        fn.save.ellipses <- ifelse(TRUE %in% sapply(lhs, function(x) !is.null(x[["ellipses"]])), ".elps", "")
        fn.iso <- ifelse(TRUE %in% sapply(lhs, function(x) !is.null(x$isos)), ".iso", "")
        int.num.hsp <- sum(sapply(lhs, function(x) length(x$hsp)))
        fn.hsp <- if (int.num.hsp == 0) "" else paste(".hsp", int.num.hsp, sep="")
         
        if (!is.null(suf) && substr(suf,1,1) != ".") suf <- paste(".", suf, sep="") 
         
        ## Construct the base of the file name (without path)
        fn.base.fileonly <- paste(fn.id, fn.params, fn.save.nn, fn.save.ellipses, fn.hsp, fn.iso, suf, ".lhs", sep="")
        if (nchar(fn.base.fileonly) > 87) fn.base.fileonly <- paste(substr(fn.base.fileonly, 1, 77), sep="")
        
        if (!file.exists(dir)) stop(paste("Directory not found:", dir))
        fn.base.full <- file.path(dir, fn.base.fileonly)
        fn.full <- paste(fn.base.full, if (auto.num.files) ".01" else NULL, ".RData", sep="")
    } else {
        fn.full <- file
    }

    ## See if the file already exists
    if (file.exists(fn.full)) {
        if (!is.null(fn) || !auto.num.files) stop("File already exists. Please try a different file name.")

       # Construct a new file name by incrementing the auto-number
       i <- 1
       while (file.exists(fn.full)) {
          i <- i + 1
          fn.full <- paste(fn.base.full, if (auto.num.files) paste(".", formatC(i, flag=0, width=width), sep="") else NULL, ".RData", sep="")
          if (i > 99) stop("i > 99")
       }       
    }

    if (is.null(save.as)) {
        save.as <- deparse(substitute(lhs))
    } else {
        assign(save.as, lhs)
    }

    save(list=save.as, file = fn.full, compress = compress)
    cat("LoCoH-hullset ", deparse(substitute(lhs)), " saved as '", save.as, "' to:\n  ", normalizePath(path.expand(fn.full)), "\n", sep="")
    return(invisible(fn.full))

}
