#' Change the id value(s) in a LoCoH-xy object
#'
#' Changes the id values in a LoCoH-xy object, with option to save the 'old' id values as an ancillary variable
#'
#' @param lxy A \link{LoCoH-xy} object
#' @param id.new New value(s) for the id (i.e., name of the individual(s)). Can be either a single character object 'broken_tooth', or 
#' factor or character vector of the same length as the number of points
#' @param id.old.save Whether to save the old ids as an ancillary variable. Either T/F, or the name of a column in the 
#' ancillary variables data frame, see details
#' @param dup.dt What to do if duplicate dt values are encountered in the merged object, see details
#' @param dup.dt.offset The maximum offset in seconds to offset duplicate dt values, used only if \code{dup.dt = "offset"} 
#' @param dt.int.round.to The proportion of the median sampling frequency that time intervals will be rounded to when computing the frequency 
#' table of sampling intervals (no change is made to the time stamps)
#' @param tau.diff.max The maximum deviation from tau (the median delta.t of the entire dataset), expressed as a proportion of tau, that time 
#' difference between two points must fall for the distance between those two points to be included in the calculation of the median step length
#'
#' @details
#' This function can be used to change the id value for each point. Note that the id value is the name of the individual animal or device, as opposed to ptid 
#' which is a unique numeric integer for each point.
#'
#' \code{tau.diff.max} exists to eliminate the inclusion of temporal outliers in the computation of the median step length. The time difference 
#' betwen points must be withint \code{tau.diff.max} of tau for that pair of points to be included in step length calculation.
#'
#' One can also use this function to change the id (name of the individual), rebuild the rw.params or table of sampling frequencies
#'
#' @return A \link{LoCoH-xy} object containing the original xys, dt, ptid and ancillary values for each point. Other items and attributes (including the comment) will be recreated or set to NULL
#'
#' @export

lxy.id.new <- function(lxy, id.new, save.old.id=NULL, dup.dt=c("check", "offset", "ignore")[1], dup.dt.offset=5, dt.int.round.to=0.1, tau.diff.max=0.02, status=TRUE) {

    if (!inherits(lxy, "locoh.lxy")) stop("lxy should be of class \"locoh.lxy\"")
    if (is.null(lxy[["pts"]])) stop("Old data structure detected, please update with lxy.repair()")
    if (!require(sp)) stop("package sp required")
    if (length(id.new) != 1 && length(id.new) != nrow(lxy[["pts"]])) stop("Invalid value for 'id.new'")

    ## Get the anv values
    if (is.null(lxy[["anv"]])) {
        anv.df <- NULL
        anv.desc <- NULL
    } else {
        anv.df <- lxy[["pts"]]@data[ , as.character(lxy[["anv"]][["anv"]])]
        anv.desc <- as.character(lxy[["anv"]][["desc"]])
    }
    
    #anv <- lxy[["pts"]]@data[ , which(!names(lxy[["pts"]]@data) %in%  c("ptid", "dt", "id"))]
    #if (ncol(anv)==0) anv <- NULL

    ## See if we need to save the existing id values
    if (!is.null(save.old.id)) {
        ## Get the name of the column in the anv where the current id values will be saved
        if (identical(save.old.id, TRUE)) {
            old.id.col <- "id.orig"
        } else if (is.character(save.old.id)) {
            old.id.col <- save.old.id
        } else {
            stop(cw("'save.old.id' should be T/F or the name of the variable (column) that the old ids will be saved as in the ancillary values table", final.cr=F, exdent=2))
        }
        
        old.id.col.desc <- "orig id"
        if (is.null(anv.df)) {
            anv.df <- data.frame(lxy[["pts"]][["id"]])
            names(anv.df) <- old.id.col
            anv.desc <- old.id.col.desc
        } else {
            ## Add the id values to anv.df
            anv.df[[old.id.col]] <- lxy[["pts"]][["id"]]
            
            ## See if the column is in anv.desc
            old.id.col.idx <- which(lxy[["anv"]][["anv"]]==old.id.col)
            #old.id.col.idx <- which(names(anv.df)==old.id.col)
            
            ## If it wasn't found, add it to the end
            if (length(old.id.col.idx)==0) {
                anv.desc <- c(anv.desc, old.id.col.desc)
            } else {
                anv.desc[old.id.col.idx] <- old.id.col.desc
            }
            
        }
    }
    
    if (dup.dt == "check") {
        dup.dt.check <- TRUE
    } else if (dup.dt == "ignore") {
        dup.dt.check <- FALSE
    } else if (dup.dt == "offset") {
        if (anyDuplicated(data.frame(dt=as.numeric(lxy[["pts"]][["dt"]], units="secs"), id=id.new)) != 0) {
            #print("Pause");browser()
            dups.idx <- which(duplicated(data.frame(dt=as.numeric(lxy[["pts"]][["dt"]], units="secs"), id=id.new)))
            if (status) cat(" - offsetting the time stamp of ", length(dups.idx), " duplicate time points \n"); flush.console()
            #dups.idx <- which(duplicated(dt.idnew))
            offset.amts <- sample(c(1:dup.dt.offset,-(1:dup.dt.offset)), length(dups.idx), replace=TRUE)
            lxy[["pts"]][["dt"]][dups.idx] <- lxy[["pts"]][["dt"]][dups.idx] + offset.amts
            if (anyDuplicated(data.frame(dt=as.numeric(lxy[["pts"]][["dt"]], units="secs"), id=id.new)) != 0) {
                dups.idx <- which(duplicated(data.frame(dt=as.numeric(lxy[["pts"]][["dt"]], units="secs"), id=id.new)))
                if (status) cat(" - there are still ", length(dups.idx), " duplicate time points \n"); flush.console()
                stop("Offsetting duplicate dt values didn't work. Try a larger value for dt.dup.offset or fix manually")
            }
        }
        dup.dt.check <- TRUE
    } else {
        stop("Unknown value for dup.dt")
    }
    
    
    
    return(xyt.lxy(xy=coordinates(lxy[["pts"]]), dt=lxy[["pts"]][["dt"]], proj4string=lxy[["pts"]]@proj4string, id=id.new, ptid=lxy[["pts"]][["ptid"]],
           anv=anv.df, anv.desc=anv.desc, dup.dt.check=dup.dt.check, dt.int.round.to=dt.int.round.to, tau.diff.max=tau.diff.max))

}
