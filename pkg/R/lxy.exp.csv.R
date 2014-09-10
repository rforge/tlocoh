#' Export coordinates to a csv file
#'
#' Exports coordinates and date-stamps from a LoCoH-xy object to a csv file
#'
#' @param lxy A LoCoH-xy object
#' @param dir The output directory 
#' @param file Filename (with extension .csv)
#' @param id The id(s) of the locations to export
#' @param file.overwrite Overwrite existing file, T/F
#' @param quote Delimit strings in quotes, T/F
#'
#' @return A data frame containing the coordinates and attribute values of \code{lxy}
#'
#' @note This function will export the id value (i.e., animal name) point-id value (ptid), coordinates, and date stamp (if exists) of a LoCoH-xy object to a csv file
#'
#' If file is not passed, a filename will be automatically constructed. 
#' @export
#' @import sp

lxy.exp.csv <- function (lxy, dir=".", file=NULL, id=NULL, file.overwrite=FALSE, quote=FALSE) {

    if (!inherits(lxy, "locoh.lxy")) stop("lxy should be of class \"locoh.lxy\"")
    if (!file.exists(dir)) stop(paste("Output directory doesn't exist:", dir))
    
    #odig <- options("digits")
    #on.exit(options("digits"=odig)
    
    if (is.null(id)) {
        id <- levels(lxy[["pts"]][["id"]])
        idx <- 1:nrow(lxy[["pts"]])
    } else {
        if (FALSE %in% (id %in% levels(lxy[["pts"]][["id"]]))) stop("id value(s) not found")
        idx <- which(lxy[["pts"]][["id"]] %in% id)
    }
    
    if (is.null(file)) {
        file <- paste(paste(id, collapse=".", sep=""), ".csv", sep="")
    } else {
        if (substr(file, nchar(file)-3, nchar(file)) != ".csv") file <- paste(file, ".csv", sep="")
    }
    
    fnfull <- file.path(dir, file)
    if (file.exists(fnfull) && !file.overwrite) stop(paste(fnfull, " exists. Please specify a different file name.", sep=""))
    
    lxy.df <- data.frame(lxy[["pts"]][idx, , drop=FALSE])
    lxy.df[["id"]] <- lxy.df[["id"]][ , drop=TRUE] 
    write.csv(lxy.df, file=fnfull, quote=quote, row.names=FALSE)
    cat("LoCoH-xy", deparse(substitute(lxy)), "saved to:\n  ", normalizePath(path.expand(fnfull)), "\n")
    return(invisible(lxy.df))

}
