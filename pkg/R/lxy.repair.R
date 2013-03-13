#' Repair a LoCoH-xy object
#'
#' Recreates a LoCoH-xy object
#'
#' @param lxy A LoCoH-xy object
#' @param dt.int.round.to The proportion of the median sampling frequency that time intervals will be rounded to when 
#' computing the frequency table of sampling intervals (no change is made to the time stamps)
#' @param tau.diff.max The maximum deviation from tau (the median delta.t of the entire dataset), expressed as a proportion of tau, that 
#' time difference between two points must fall for the distance between those two points to be included in the calculation of the median step length
#'
#' @details
#' This will return a new lxy object containing the original xys, dt, and ptid from lxy. All other items and attributes (including the comment) 
#' will be recreated or set to NULL
#'
#' \code{tau.diff.max} exists to eliminate the inclusion of temporal outliers in the computation of the median step length. The time difference betwen points must be withint \code{tau.diff.max} of tau for that pair of points to be included in step length calculation.
#'
#' @return A LoCoH-xy object
#'
#' @seealso \code{\link{lxy.id.new}}
#' @export

lxy.repair <- function(lxy, fix.dup.ptid=FALSE, fix.anv=FALSE, dt.int.round.to=0.1, tau.diff.max=0.02) {
    
    if (class(lxy)=="LoCoH.lxy") class(lxy) <- "locoh.lxy"
    if (!inherits(lxy, "locoh.lxy")) stop("lxy should be of class \"locoh.lxy\"")
    if (!require(sp)) stop("package sp required")

    ## Convert the 'old' plural form of element names to singular forms
    if ("ids" %in% names(lxy)) names(lxy)[names(lxy)=="ids"] <- "id"
    if ("ptids" %in% names(lxy)) names(lxy)[names(lxy)=="ptids"] <- "ptid"
    if (!is.null(lxy[["nn"]])) {
        for (nn.idx in 1:length(lxy[["nn"]])) {
            if (names(lxy[["nn"]][[nn.idx]])[6] == "ptids") names(lxy[["nn"]][[nn.idx]])[6] <- "ptid"
        }
    }
    #if (fix.names.only) return(lxy)
    
    if (!is.null(lxy[["ptid"]])) {
        if (anyDuplicated(lxy[["ptid"]])) {
            if (fix.dup.ptid) {
                lxy[["ptid"]] <- 1:length(lxy[["ptid"]])
                cat(" - duplicate ptid values detected...new values generated\n")
            } else {
                stop(cw("Duplicate ptid values detected. To repair by generating new ptid values, set fix.dup.ptid=T", final.cr=FALSE, exdent=2))
            }
        }
    }

    if (!is.null(lxy[["anv"]])) {
        if (!identical(names(lxy[["anv"]]),  c("anv","desc"))) {
            if (nrow(lxy[["anv"]]) != nrow(lxy[["xys"]])) {
                if (fix.anv) {
                    lxy[["anv"]] <- NULL
                } else {
                    stop(cw("The number of rows in the ancillary variables data frame is different than the number of points. To correct this by deleting the ancillary variables table, set fix.anv=TRUE", final.cr=FALSE, exdent=2))
                }
            }
        }
    }
    
    if (is.null(lxy[["pts"]])) {
        xy.use <- lxy[["xys"]]
        ptid.use <- lxy[["ptid"]]
        dt.use <- lxy[["dt"]]        
        id.use <- lxy[["id"]]     
        anv.use <- lxy[["anv"]]
        anv.desc.use <- NULL
        proj4string.use <- CRS(as.character(NA))
    } else {
        xy.use <- coordinates(lxy[["pts"]])
        ptid.use <- lxy[["pts"]][["ptid"]]
        dt.use <- lxy[["pts"]][["dt"]]
        id.use <- lxy[["pts"]][["id"]]
        anv.use <- if (is.null(lxy[["anv"]])) NULL else lxy[["pts"]]@data[ , as.character(lxy[["anv"]][["anv"]]), drop=FALSE]
        anv.desc.use <- lxy[["anv"]][["desc"]]
        proj4string.use <- lxy[["pts"]]@proj4string
    }
        
    return(xyt.lxy(xy=xy.use, dt=dt.use, id=id.use, ptid=ptid.use, anv=anv.use, anv.desc=anv.desc.use, 
           proj4string=proj4string.use, dt.int.round.to=dt.int.round.to, tau.diff.max=tau.diff.max))

}
