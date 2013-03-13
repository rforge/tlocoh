#' findonpath
#'
#' Searches for a file on the system environment path
#'
#' @param fn The filename to search for (without a path)
#' @param status Show messages
#'
#' @note
#' This will return the first found occurence of file \code{fn}, searching 1) the current working directory, 2) the user's R 'home' directory, 
#' then 3) the directories on the operating system environment 'path' 
#' 
#' @return The full path and name of the found file, or NULL if not found
#'
#' @export

findonpath <-
function(fn, status=TRUE) {
    
    if (file.exists(fn)) return(fn)
    
    path <- Sys.getenv("path")
    if (path == "") {
        if (status) cat("Path environment variable not found \n")
        return(NULL)
    }
    
    path.dirs <- c(path.expand("~"), strsplit(path, ";", fixed = TRUE)[[1]])
    fn.with.path <- file.path(path.dirs, fn)
    fn.with.path.exists <- which(file.exists(fn.with.path))
    if (length(fn.with.path.exists)==0) {
        if (status) cat(fn, "not found anywhere on the path\n")
        return(NULL)
    } 
    if (length(fn.with.path.exists) > 1 && status) cat(length(fn.with.path.exists), "occurences of", fn, "found \n")
    return(normalizePath(fn.with.path[fn.with.path.exists[1]]))
}
