#' Merges LoCoH-xy object
#'
#' Merges LoCoH-xy objects together
#'
#' @param ... \link{LoCoH-xy} objects
#' @param pts.flds A character vector of column names to include in the merged object, can also be \code{"all"} or \code{NULL}
#' @param save.ptid If duplicate ptid values are encounted in the merged object, whether to save the original ptid values as an ancillary variable, T/F
#' @param new.id A new id value for the merged object
#' @param save.old.id A boolean or the name of a ancillary variable where the old id values will be saved
#' @param dt.int.round.to The proportion of the median sampling frequency that time intervals will be rounded to when 
#' computing the frequency table of sampling intervals (no change is made to the time stamps)
#' @param tau.diff.max The maximum deviation from tau (the median delta.t of the entire dataset), expressed as a proportion of tau, that 
#' time difference between two points must fall for the distance between those two points to be included in the calculation of the median step length
#'
#' @details 
#'
#' The coordinate system of the LoCoH-xy objects being merged must be the same. Also each lxy object must have the same data columns 
#' present, e.g., dt, col, ancillary variables, etc. To merge a subset of the attribute fields, use the \code{pts.flds} parameter.
#'
#' Because LoCoH-xy object must have unique ptid values for each location, if the merged object contains duplicate ptid values 
#' new one will be created. If \code{save.ptid=TRUE}, the 'old' ptid values will be saved as \code{ptid.orig}.
#'
#' The merged object will no have nearest neighbors table and the random walk parameters will be recomputed.
#'
#' @seealso \code{\link{lxy.id.new}}
#' @export

lxy.merge <- function (..., pts.flds="all", save.ptid=FALSE, new.id=NULL, save.old.id=NULL, dt.int.round.to=0.1, tau.diff.max=0.02) {

    if (!require(sp)) stop("package sp required")
    if (!is.null(new.id) || !is.null(save.old.id)) stop(cw("These options are not yet working, but will allow you to assign a new id to the merged object and save the old ids as an ancillary variable", final.cr=F))
    
    lxy.lst <- list(...)
    if (length(lxy.lst)==1) stop("Can't merge one object")
    if (length(lxy.lst)==0) return(NULL)
    if (FALSE %in% sapply(lxy.lst, function(lxy) is(lxy, "locoh.lxy"))) stop("all merged objects should be class \"locoh.lxy\"")
    
    lxy1 <- lxy.lst[[1]]
    
    if (identical(pts.flds,"all")) {
        pts.flds <- names(lxy1[["pts"]])
    } else {
        pts.flds <- vectorize.parameter(pts.flds, type="character")
        ## Make sure ptid and id are the first two
        pts.flds <- unique(c("ptid","id", pts.flds))
        if (FALSE %in% (pts.flds %in% names(lxy1[["pts"]]))) stop(paste("Field(s) not found: ", paste(pts.flds[!pts.flds %in% names(lxy1[["pts"]])], collapse=", ", sep=""), sep=""))
        
        ## Delete any rows from lxy1[["anv"]] that aren't being copied to the new lxy
        if (!is.null(lxy1[["anv"]])) lxy1[["anv"]] <- lxy1[["anv"]][lxy1[["anv"]][["anv"]] %in% pts.flds, , drop=FALSE]
        if (nrow(lxy1[["anv"]])==0) lxy1[["anv"]] <- NULL
    }
    
    ## Merge all the spatial point data frames
    for (lxy2 in lxy.lst[-1]) {
        ## Make sure lxy2 has the necessary fields
        if (FALSE %in% (pts.flds %in% names(lxy2[["pts"]]))) stop(paste("Field(s) not found: ", paste(pts.flds[!pts.flds %in% names(lxy2[["pts"]])], collapse=", ", sep=""), sep=""))

        ## Merge the two spatial points data frames
        lxy1[["pts"]] <- rbind(lxy1[["pts"]][ , pts.flds], lxy2[["pts"]][ , pts.flds])
        
        ## The following command corrects a bug in package sp whereby rbind inserts a leading space the @proj4string@projargs 
        lxy1[["pts"]]@proj4string@projargs <- strTrim(lxy1[["pts"]]@proj4string@projargs)
    }
    
    ## Assign new ptid values if duplicates found
    if (anyDuplicated(lxy1[["pts"]][["ptid"]])) {
        cat("   Duplicate ptid values detected in the merged object \n")
        ## Save the old ptid values
        if (save.ptid) {
            ptid.orig.col <- "ptid.orig"
            cat("    - saving existing ptid values as ", ptid.orig.col, "\n")
            lxy1[["pts"]]@data[[ptid.orig.col]] <- lxy1[["pts"]][["ptid"]]
            
            ## If there isn't already a row in lxy[["anv"]] for ptid.orig, add one
            if (!ptid.orig.col %in% as.character(lxy1[["anv"]][["anv"]])) {
                lxy1[["anv"]] <- rbind(lxy1[["anv"]], data.frame(anv=ptid.orig.col, desc="orig ptid"))
            }
        }
        cat("    - creating new ptid values \n")
        lxy1[["pts"]][["ptid"]] <- 1:nrow(lxy1[["pts"]])
    }
                
    return(xyt.lxy(xy=coordinates(lxy1[["pts"]]), dt=lxy1[["pts"]][["dt"]], id=lxy1[["pts"]][["id"]], ptid=lxy1[["pts"]][["ptid"]], 
                   anv=if (is.null(lxy1[["anv"]])) NULL else lxy1[["pts"]]@data[ , as.character(lxy1[["anv"]][["anv"]]), drop=FALSE],
                   anv.desc=as.character(lxy1[["anv"]][["desc"]]), 
                   proj4string=lxy1[["pts"]]@proj4string, dt.int.round.to=dt.int.round.to, tau.diff.max=tau.diff.max))

}
