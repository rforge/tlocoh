#' Converts a Move object to a LoCoH-xy object
#'
#' Converts a Move or MoveStack object from the move package to a LoCoH-xy object
#'
#' @param move.obj A object of class Move or MoveStack.
#' @param use.utm Import UTM coordinates if found \emph{and} all points fall within the same UTM zone (T/F).
#' @param xcoord The name of a field in the data table for the x-coordinates. If NULL, the default coordinates will be used. 
#' @param ycoord The name of a field in the data table for the y-coordinates. If NULL, the default coordinates will be used. 
#' @param proj Projection object of class \code{\link{CRS-class}}. Used *only* to specify the coordinate system when the coordinates being imported are coming from the data table
#' @param anv.flds The name of field(s) in the Move object data table that will be imported as ancillary variables. Can also be 'all'.
#' @param ptid The name of a numeric field in the Move object data table containing unique numeric values for each point. If NULL new point id values will be created.
#' @param del.dup.xyt Whether to delete duplicate rows with the same x, y, dt, and id value (T/F).
#' @param dup.dt.check Whether to check to make sure there are no duplicate date values for the same id.
#' @param dt.int.round.to The proportion of the median sampling frequency that time intervals will be rounded to when computing the frequency table of sampling intervals (no change is made to the time stamps).
#' @param tau.diff.max The maximum deviation from tau (expressed as a proportion of tau) that a point-to-point time difference must fall within for the point-to-point distance to be included in the calculation of the median step length.
#' @param req.id Require a value for id (T/F).
#' @param warn.latlong Show a warning message if it detects coordinates may be in latitude-longitude (T/F).
#' @param status Show status messages (T/F).
#'
#' @note Move and MoveStack are formal classes from the \emph{Move} package for storage of movement data for a single (Move) or multiple (MoveStack) indviduals. 
#' \code{move.lxy} will convert these objects to \link{LoCoH-xy} objects. To import the attribute data associated with 
#' each point (termed ancillary variables in T-LoCoH), set \code{anv.flds} to the names of the field(s) you wish to import (or \code{anv.flds="all"}). 
#' Note that the Move and MoveStack classes store a lot more metadata than
#' LoCoH-xy objects, including unused records, which are not supported by the \link{LoCoH-xy} data class and are not imported. 
#'
#' @return A \link{LoCoH-xy} object
#'
#' @examples
#' require(move)
#' leroy.move <- move(system.file("extdata","leroy.csv.gz",package="move"))
#' class(leroy.move)
#' leroy.lxy <- move.lxy(leroy.move, anv.flds=c("ground.speed", "heading"))
#' summary(leroy.lxy)
#'
#' @export
#' @seealso \code{\link{xyt.lxy}}

move.lxy <- function(move.obj, use.utm=FALSE, xcoord=NULL, ycoord=NULL, proj=NULL, anv.flds=NULL, ptid=NULL, 
                     del.dup.xyt=TRUE, dup.dt.check=TRUE, dt.int.round.to=0.1, tau.diff.max=0.02, req.id=TRUE, warn.latlong=TRUE, status=TRUE) {

    ## Still to come - convert burts to separate ids / sub-ids, or an ancillary variable field in the data frame

    if (!require(move)) stop("move package is required for this function")
    
    ## Check for correct class type
    if (!is(move.obj,"Move") && !is(move.obj,"MoveStack")) stop("move.obj must be of class Move")
    
    ## Check that both or neither xcoord and yccord have been passed
    err.msg <- "xcoord and ycoord should both be the names of columns in the data frame, or else omitted"
    if (is.null(xcoord) + is.null(ycoord) == 1) stop(err.msg)
    
    if (use.utm) {
        if (!is.null(xcoord) || !is.null(ycoord) || !is.null(proj)) {
            stop(cw("To automatically import utm coordinates, do not pass the name of the x and y coordinates or proj", final.cr=FALSE))
        }

        ## Check that UTM coordinates exist in the data table
        xcoord <- "utm.easting"; ycoord <- "utm.northing"
        if (FALSE %in% (c(xcoord,ycoord) %in% names(move.obj@data))) stop("Can't find columns in data table for UTM northing and UTM easting")

        ## Get the utm zone
        if ("utm.zone" %in% names(idData(move.obj))) {
            utm.zone <- unique(as.character(idData(move.obj)[["utm.zone"]]))
        } else {
            if ("utm.zone" %in% names(move.obj@data)) {
                utm.zone <- unique(as.character(move.obj@data[["utm.zone"]]))
            } else {
                stop("Can't find 'utm.zone' field in Move object")    
            }
        }
        if (length(utm.zone) != 1) stop(cw("Can only import utm coordinates if they all fall within the same UTM zone, which does not seem to be the case", final.cr=FALSE))
        
        ## Parse out the utm.zone into zone number and hemisphere
        north.south <- substr(utm.zone, nchar(utm.zone), nchar(utm.zone))
        if (!north.south %in% c("N","S")) stop("Can not determine the UTM zone hemisphere")
        if (north.south == "N") utm.ns <- "+north" else utm.ns <- "+south"
        
        utm.zone.num <- substr(utm.zone, 1, nchar(utm.zone)-1)
        if (length(grep("[^0-9\\.]", utm.zone.num, value = TRUE)) != 0) stop("Can not determine UTM zone number")
        utm.zone.num <- paste("+zone=", utm.zone.num, sep="")
        
        proj4string <- paste("+proj=utm", utm.ns, utm.zone.num, sep=" ")
        
        ## See if the proj4string is valid
        crs.good <- checkCRSArgs(proj4string)
        if (!crs.good[[1]]) stop(paste(proj4string, "\n", crs.good[[2]], sep=""))
        
        proj <- CRS(proj4string)
    }
    
    ## If the xcoord and ycoord are passed, we can't assume they have the same projection system
    ## as move.obj@coords. So set proj to NA
    if (!is.null(xcoord) && !is.null(ycoord) && is.null(proj)) {
        proj <- CRS(as.character(NA))
        warning("Coordinate system not known. Please add coordinate system info using lxy.proj.add")
    }
    
    ## Define proj if NULL or a character
    if (is.null(proj)) {
        proj <- move.obj@proj4string
    } else if (is.character(proj)) {
        proj <- CRS(proj)
        crs.good <- checkCRSArgs(proj@projargs)
        if (!crs.good[[1]]) stop(crs.good[[2]])
    } else if (!is(proj, "CRS")) {
        stop("proj must be class CRS or character")
    }

    ## Get the xy coordinates
    if (is.null(xcoord)) {
        xy <- coordinates(move.obj)
    } else {
        xs <- move.obj@data[[xcoord]]
        if (is.null(xs)) stop(paste("Can't find ", xcoord, " in ", deparse(substitute(move.obj)), sep=""))
        ys <- move.obj@data[[ycoord]]
        if (is.null(ys)) stop(paste("Can't find ", ycoord, " in ", deparse(substitute(move.obj)), sep=""))
        xy <- cbind(xs, ys)
    }

    ## Get the time stamps
    dt <- timestamps(move.obj)
    
    ## Get id value
    if (is(move.obj, "MoveStack")) {
        id <- move.obj@trackId
    } else {
        #id <- move.obj@idData[["individual.local.identifier"]] #or 'local_identifier'
        id <- rownames(idData(move.obj))
    }
    if (length(id)==0) {
        print("can't find id");browser()
        warning("Can not find the id(s) for this individual")    
        id <- "a"
    }    

    ## Get ancillary variables
    if (is.null(anv.flds)) {
        anv.df <- NULL
    } else {
        if (identical(anv.flds, "all")) {
            anv.flds <- names(move.obj@data)
            anv.flds <- anv.flds[anv.flds != "timestamp"]
            anv.df <- move.obj@data[, anv.flds, drop=FALSE]
        } else {
            if (FALSE %in% (anv.flds %in% names(move.obj@data))) {
                msg <- paste("Field(s) not found in data table for ", deparse(substitute(move.obj)), ": ", paste(anv.flds[!anv.flds %in% names(move.obj@data)], collapse=", ", sep=""), sep="")
                stop(msg)
            }
            anv.df <- move.obj@data[, anv.flds, drop=FALSE]
        }
    }
    
    ## Get ptid
    if (!is.null(ptid)) {
        if (!ptid %in% names(move.obj@data)) {
            msg <- paste(ptid, " not found in data table for ", deparse(substitute(move.obj)), sep="")
            stop(msg)
        }
        ptid <- move.obj@data[[ptid]]
    }

    lxy <- xyt.lxy(xy=xy, dt=dt, id=id, ptid=ptid, proj4string=proj, anv=anv.df, warn.latlong=warn.latlong)
    return(lxy)

}