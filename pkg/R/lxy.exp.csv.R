#' Export coordinates to a csv file
#'
#' Exports coordinates and date-stamps from a LoCoH-xy object to a csv file
#'
#' @param lxy A LoCoH-xy object
#' @param file Filename (with extension .csv)
#' @param id The id(s) of the locations to export
#' @param file.overwrite Overwrite existing file, T/F
#' @param quote Delimit strings in quotes, T/F
#'
#' @note This function will export the id value (i.e., animal name) point-id value (ptid), coordinates, and date stamp (if exists) of a LoCoH-xy object to a csv file
#'
#' If file is not passed, a filename will be automatically constructed. 
#' @export

lxy.exp.csv <- function (lxy, file=NULL, id=NULL, file.overwrite=FALSE, quote=FALSE) {

    if (!inherits(lxy, "locoh.lxy")) stop("lxy should be of class \"locoh.lxy\"")
    if (!require(sp)) stop("package sp required")
    
    #odig <- options("digits")
    #on.exit(options("digits"=odig)
    
    if (is.null(id)) {
        id <- levels(lxy[["pts"]][["id"]])
        idx <- 1:nrow(lxy[["pts"]])
    } else {
        if (FALSE %in% (id %in% levels(lxy[["pts"]][["id"]]))) stop("id value(s) not found")
        idx <- which(lxy[["pts"]][["id"]] %in% id)
    }
    
    if (is.null(file)) file <- paste(paste(id, collapse=".", sep=""), ".csv", sep="")
    if (file.exists(file) && !file.overwrite) stop(paste(file, " exists. Please specify a different file name.", sep=""))
    if (substr(file, nchar(file)-3, nchar(file)) != ".csv") file <- paste(file, ".csv", sep="")
    
    #lxy.df <- data.frame(ptid=lxy[["pts"]][["ptid"]][idx], id=levels(lxy[["pts"]][["id"]])[lxy[["pts"]][["id"]][idx]], 
    #                     x=coordinates(lxy[["pts"]])[idx,1,drop=FALSE], y=coordinates(lxy[["pts"]])[idx,2,drop=FALSE])
    #if (!is.null(lxy[["pts"]][["dt"]])) lxy.df <- transform(lxy.df, dt=lxy[["pts"]][["dt"]][idx])    
    
    lxy.df <- data.frame(lxy[["pts"]][idx, ,drop=FALSE])
    write.csv(lxy.df, file=file, quote=quote, row.names=FALSE)
    cat("LoCoH-xy", deparse(substitute(lxy)), "saved to:\n  ", normalizePath(path.expand(file)), "\n")
    return(invisible(lxy.df))

}
