#' Create a LoCoH-xy object
#'
#' Creates a \link{LoCoH-xy} object from a set of location data 
#'
#' @param xy A two-column matrix or data frame containing the xy coordinates of the points
#' @param dt Optional vector of date-time values (either POSIXct objects or objects that can be coerced to POSIXct)
#' @param id Optional character vector or factor containing the name(s) of the individual(s) of each location.
#' @param ptid Optional integer vector of point id values
#' @param proj4string Projection string object of class \code{\link{CRS-class}}
#' @param anv Optional ancillary variables for each point (data frame with same number of records as xy)
#' @param anv.desc Optional character vector with descriptions of the ancillary variables (in the same order as they appear in \code{anv})
#' @param tz The name of the time zone that will be assigned if not explicit in dt.
#' @param del.dup.xyt Whether to delete duplicate rows with the same x, y, dt, and id value. (T/F)
#' @param dup.dt.check Whether to check to make sure there are no duplicate date values for the same id
#' @param show.dup.dt Whether to show duplicate time stamps (if found) (T/F)
#' @param col Optional vector of color values (one for each point), or a single color value
#' @param dt.int.round.to The proportion of the median sampling frequency that time intervals will be rounded to when computing the frequency table of sampling intervals (no change is made to the time stamps)
#' @param tau.diff.max The maximum deviation from the median sampling interval (tau), expressed as a proportion of the median sampling interval, see details
#' @param req.id Require a value for id (T/F)
#' @param warn.latlong Show a warning message if coordinates appear to be in geographic coordinates (T/F)
#' @param show.bad.timestamps If \code{TRUE}, will print any timestamps that didn't successfully convert to a POSIXct object (T/F)
#' @param status Show status messages (T/F)
#'
#' @details
#' At a minimum, a \link{LoCoH-xy} object contains a set of points. It can also contain date-time values for each point, the name of 
#' the individual(s) associated with each point, and a nearest nieghbors lookup table.
#'
#' Importing date-time values into R is often a painful process. You may want to try exporting your
#' date-time values from your spreadsheet or database as a text field that is formatted in a style
#' that R will recognize as a valid time. To see a date-time format that R will be able to convert to a date-
#' time object (class \emph{POSIXct}), type \code{Sys.time()} at the console. Once you've noted the date-time format
#' recognized by R, try to format the date field in your spreadsheet or database in a similar manner,
#' then export the values in a text format (e.g., csv). For example in MS Access, you can use the
#' \code{format()} function as part of a query to format a date field, e.g. \code{format(PointDate,
#' "yyyy-mm-dd hh:nn:ss")}. In Excel, you can go to Format - Cells and give the date-time cells a
#' custom format \code{yyyy-mm-dd hh:mm:ss}. See the Appendices of the T-LoCoH tutorial for more examples.
#'
#' The time zone paramter \code{tz} is optional. If \code{tz} is omitted, R will try to get the timezone from
#' dt, and if there is no timezone specified set it to \code{UTC}. Valid timezone names
#' are to some extent OS specific. To see valid timezone names, you may run \code{OlsonNames()}.
#' If dt is of class POSIXct (which stores the time zone) and a different value for \code{tz} is passed,
#' a prompt will ask whether date values should be converted.
#'
#' By default, if the function finds two or more locations with the same time stamp for the same id (individual), an
#' error will be triggered. This usually results from a data processing error (e.g., duplication of a row), or a conversion problem or rounding
#' issue in the time stamp values. To see which records have duplicate time stamps, pass \code{show.dup.dt=TRUE}. 
#' To disable the checking for duplicate time stamps, pass \code{dup.dt.check=TRUE}. Duplicate locations are allowed, as are
#' duplicate time stamps for different individuals.
#'
#' \code{ptid} is an optional vector of numeric id values (i.e., primary key) for each location. When present, point 
#' id values of parent points will be saved in the data table of hulls. This enables linking the 
#' outputs of tlocoh (hulls and hull metrics) with other data and/or other software tools.
#' See also \code{\link{lhs.exp.csv}} and \code{\link{lhs.exp.shp}}. \code{ptid} should not be confused with 
#' \code{id}, which is a character vector of the name(s) of the individuals in the dataset.
#' 
#' \code{tau.diff.max} controls which consecutive pairs of points will be used to compute the median step
#' length (d-bar) and the maximum observed speed (v-max). Pairs of points whose sampling interval (i.e., time
#' difference) deviates from the median by more than \code{tau.diff.max} will be excluded from this
#' computation (presumably because there was a drop-out in the data, and the distance between those pairs of locations
#' does not represent the characteristic movement pattern). However for data that have a wide distribution
#' of sampling intervals (e.g., manually collected radio telemetry locations), this filtering may
#' result in an insufficient number of pairs. In this case, increase \code{tau.diff.max} or set it to zero to
#' disable filtering completely.
#'
#' Other variables, including environmental variables or biometric variables can be brought in using the \code{anv} parameter.
#' These variables can then be used in hull metrics.
#'
#' @return A object of class \link{locoh.lxy}
#'
#' @seealso \code{\link{lxy.nn.add}}, \code{\link{lxy.repair}}, \code{\link{lxy.subset}}
#'
#' @examples
#' \dontrun{
#' #Create an unbounded random walk as a sample dataset
#' n <- 500; stepsize <- 5
#' theta <- runif(n, min=0, max=2*pi)
#' start <- c(100,100)
#' xy <- data.frame(x=start[1]+cumsum(stepsize * cos(theta)), y=start[2]+cumsum(stepsize*sin(theta)))
#' plot(xy, pch=20, type="b", lty=1)
#' timestamps <- Sys.time() + 3600 * (1:n-1)
#'
#' #Combine the xy locations and timestamps into a LoCoH-xy object
#' lxy <- xyt.lxy(xy=xy, dt=timestamps, id="broken_tooth")
#' summary(lxy)
#' }
#'
#' @export
#' @import sp

xyt.lxy <- function (xy, dt=NULL, tz=NULL, id=NULL, ptid=NULL, proj4string=CRS(as.character(NA)), anv=NULL, anv.desc=NULL, col=NULL,
                     del.dup.xyt=TRUE, dup.dt.check=TRUE, show.dup.dt=FALSE, dt.int.round.to=0.1, tau.diff.max=0.02, req.id=TRUE, 
                     warn.latlong=TRUE, show.bad.timestamps=FALSE, status=TRUE) {
                     
    #cat("still want to try to error test whether tz is valid \n")
    
    err.msg <- "xy must be a two column data frame or matrix"
    if (is.matrix(xy)) {
         if (ncol(xy) != 2) stop(err.msg)
         xy <- as.data.frame(xy)
    }
    if (is.data.frame(xy) && length(xy) != 2) stop(err.msg)
    if (warn.latlong && min(xy[,1]) >= -180 && max(xy[,1]) <= 180 && min(xy[,2]) >= -90 && max(xy[,2]) <= 90) warning(cw("Your data appear to be in geographic coordinates (latitude-longitude). You can use T-LoCoH with geographic coordinates, but it isn't recommended because length and area in degrees are not meaningful. Consider projecting your data to a planar coordinate system with lxy.reproject().", final.cr=F, exdent=2))
    names(xy) <- c("x","y")
    
    if (is.null(id)) {
        if (req.id) stop("id (the indvidual name(s)) is a required parameter")    
    } else {
        if (length(id) != 1 && length(id) != nrow(xy)) stop("xy and id must be of the same length")
        if (TRUE %in% (sapply(id, is.na))) stop("NA value(s) detected in id")
    }
   
    if (!is.null(col)) {
        if (length(col) == 1) col <- as.factor(rep(col, times=nrow(xy)))
        if (length(col) != nrow(xy)) stop("xy and col must be of the same length")
        col <- as.factor(as.character(col))
    }
    
    if (!is.null(anv)) {
        if (!is.data.frame(anv) && !is.matrix(anv)) stop("anv must be a data frame or matrix")
        if (is.matrix(anv)) anv <- as.data.frame(anv)
        if (nrow(anv)==0 || ncol(anv)==0) anv <- NULL
    }
    
    ## Make sure that all vectors passed have the same length
    if (!is.null(dt) && (length(dt) != nrow(xy))) stop("xy and dt must be of the same length")
    if (!is.null(anv) && (nrow(anv) != nrow(xy))) stop("xy and anv must have the same number of rows")
    if (!is.null(id) && (length(id) != nrow(xy)) && (length(id) != 1)) stop("xy and id must be of the same length")
    if (!is.null(ptid) && (length(ptid) != nrow(xy))) stop("xy and ptid must be of the same length")
    
    ## Remove blank rows in xy
    orig.length <- nrow(xy)
    for (i in 1:2) {
        good.idx <- !is.na(xy[, i])
        xy <- xy[good.idx, ]
        if (!is.null(id) && length(id) != 1) id <- id[good.idx]
        if (!is.null(ptid)) ptid <- ptid[good.idx]
        if (!is.null(anv)) anv <- anv[good.idx, , drop=FALSE]
        if (!is.null(dt)) dt <- dt[good.idx]
        if (!is.null(col)) col <- col[good.idx]
    }
    if (status && (nrow(xy) != orig.length)) cat("  ", orig.length - nrow(xy), " blank rows removed \n", sep="")
        
    ## Remove duplicate xy-time-id rows 
    if (del.dup.xyt && !is.null(dt)) {
        orig.length <- nrow(xy)
        xyti.dups <- duplicated(data.frame(xy=xy, dt=dt, id=id))
        xy <- xy[!xyti.dups, ]
        if (!is.null(id) && length(id) != 1) id <- id[!xyti.dups]
        if (!is.null(ptid)) ptid <- ptid[!xyti.dups]
        if (!is.null(anv)) anv <- anv[!xyti.dups, , drop=FALSE]
        if (!is.null(dt)) dt <- dt[!xyti.dups]
        if (!is.null(col)) col <- col[!xyti.dups]
        if (status && (nrow(xy) != orig.length)) cat("  ", orig.length - nrow(xy), " duplicate xy-time-id rows removed \n", sep="")
    }

    ## Coerce columns in xy to class double (needed to prevent FNN from crashing R)
    if (!is.double(xy[, 1])) xy[,1] <- as.numeric(xy[,1])
    if (!is.double(xy[, 2])) xy[,2] <- as.numeric(xy[,2])
    
    if (is.null(id)) {
        id = as.factor(rep("1", nrow(xy)))
    } else {
        if (length(id)==1) {
            id = factor(rep(id, nrow(xy)))
        } else {
            ## Convert ID from a vector to a factor and drop unused levels
            id <- factor(id)
        }
    }
    
    ## Make sure ptid contains only unique integer values
    if (!is.null(ptid)) {
        if (TRUE %in% duplicated(ptid)) stop(cw("Duplicate values detected in ptid. The numeric values in ptid, if passed, must be unique.", final.cr=F))
        if (FALSE %in% is.numeric(ptid)) stop(cw("ptid (if given) is supposed to be a vector of unique ID numbers (integers).", final.cr=F))
        if (TRUE %in% is.na(ptid)) stop("There is at least one row in ptid without a valid value")
    }

    if (is.null(dt)) {
        rw.params <- NULL
        dt.int <- NULL
        if (is.null(ptid)) ptid <- 1:nrow(xy)
    } else {
        if (inherits(dt, "POSIXt")) {
            if (is.null(tz)) {
                tz <- attr(dt, "tzone")        
            } else {
                if (tz != attr(dt, "tzone")) {
                    cat(cw(paste("The saved timezone (", attr(dt, "tzone"), ") is different than the tz parameter (", tz, ").", sep=""), final.cr=T))
                    ans <- readline(prompt = paste("Convert the date values to ", tz, "? y/n ", sep=""))
                    if (ans != "y") {cat("Invalid value for tz \n"); return(invisible(NULL))}
                    dt <- as.POSIXct(format(dt, tz=tz), tz=tz)
                } 
            }
        } else {
            if (is.null(tz)) {
                ans <- readline(prompt = "tz (timezone) is a required parameter. Use 'UTC' (GMT)? y/n ")
                    if (ans != "y") {cat("Please provide a value for tz \n"); return(invisible(NULL))}
                tz <- "UTC"
            }
        } 
        
        dt.orig <- dt
        dt <- as.POSIXct(dt, tz=tz)  # for reasons which are not clear, omission of tz="UTC" results in
                                    # some values not being converted properly and becoming na
        if (TRUE %in% is.na(dt)) {
            if (show.bad.timestamps) {
                idx.bad.dates <- which(is.na(dt))
                cat(cw("The following timestamps did not convert to date-time objects in R. Aborting.", final.cr=TRUE))
                print(data.frame(row_num=idx.bad.dates, bad_date=dt.orig[idx.bad.dates]))
                return(NULL)
            } else {
                stop (cw("There is at least one value in dt which does not convert to a valid timestamp. Aborting. To see which date(s) are bad, set show.bad.timestamps=TRUE.", exdent=2, final.cr=F))
            }
        }
        attr(dt, "names") <- NULL
        rm(dt.orig)
        
        ## Check for duplicate time stamps for the same individual. 
        ## We have to convert dates to seconds otherwise daylight savings time could create spurious duplicates
        if (dup.dt.check) {
            if (anyDuplicated(data.frame(dt=as.numeric(dt, units="secs"), id=id)) != 0) {
                if (show.dup.dt) {
                    ## Create a data frame with id and dt (in secs since 1970) 
                    id_dtsecs_df <- data.frame(id=id, dtsecs=as.numeric(dt, units="secs"))
                    
                    ## For the purposes of identifying duplicates, construct a string
                    id_dtsecs_str <- apply(id_dtsecs_df, 1, function(x) paste(x, collapse="\r"))
                    
                    ## Identify all records whic are duplicates
                    duped_idx <- which(id_dtsecs_str %in% id_dtsecs_str[duplicated(id_dtsecs_str)])
                    
                    ## Define the order based on 1) id, 2) dtsets
                    duped_idx_ord <- order(id_dtsecs_df$id[duped_idx], id_dtsecs_df$dtsecs[duped_idx])
                    
                    ## Construct a data frame of the duplicate records in order
                    id_dt_dups_df <-data.frame(id=id, dt=dt)[duped_idx[duped_idx_ord], ]
                    
                    cat("Duplicate time stamps:\n")
                    print(id_dt_dups_df)

                    stop("Duplicate time stamps shown above. Aborting.")

                } else {
                    msg <- "Duplicate time stamps detected. Aborting. To view the records with duplicate time stamps, use show.dup.dt=TRUE. To disable the time stamp check, set dup.dt.check=FALSE."
                    stop(cw(msg, exdent=2, final.cr=FALSE))
                }
            
            
            }
        }
       
        ## Check if there are any ids with < 5 locations
        if (TRUE %in% (table(id) < 5)) {
            bad.ids <- table(id) < 5
            cat("Insufficient number of locations found:\n")
            print(table(id)[bad.ids])
            stop(cw(paste("At least five points are needed per id. Not enough points for the following id(s): ", paste(names(bad.ids)[bad.ids], collapse=", ", sep=""), ".", sep=""), exdent=2, final.cr=F))
        }

        ## Sort everything by ID then datetime
        dt.ord = order(id, dt)
        dt <- dt[dt.ord]
        xy <- xy[dt.ord,]

        id <- id[dt.ord]
        if (is.null(ptid)) {
            ptid <- 1:nrow(xy)    
        } else {
            ptid <- ptid[dt.ord]
        }
        
        if (!is.null(col)) col <- col[dt.ord]
        if (!is.null(anv)) anv <- anv[dt.ord, , drop=FALSE]

        ## Calculate frequency table and median value of time interval for each id
        rwp.dti.lst <- xyt.rw.params.dt.int(id=id, xy=xy, dt=dt, dt.int.round.to=dt.int.round.to, tau.diff.max=tau.diff.max) 
        dt.int <- rwp.dti.lst[["dt.int"]]
        rw.params <- rwp.dti.lst[["rw.params"]]
    }
    
    if (is.null(anv)) {
        anv.df <- NULL
    } else {
        if (is.null(anv.desc)) {
            anv.desc <- rep(NA, ncol(anv))
        } else {
            if (length(anv.desc) != ncol(anv)) stop("anv.desc must be the same length as anv")
        }
        anv.df <- data.frame(anv=names(anv), desc=anv.desc, stringsAsFactors=FALSE)
    }
    
    ## Construct comment
    comment <- list()
    id.tab <- table(id)
    for (id.name in names(id.tab)) {
        comment[[id.name]] <- paste(id.name, ".n", id.tab[[id.name]], 
              if (is.null(dt)) "" else paste(".", format(min(dt[id==id.name]), format = "%Y-%m-%d", tz = ""), ".", format(max(dt[id==id.name]), format = "%Y-%m-%d", tz = ""), sep=""), 
              sep="")
    }
    
    ## Create a data frame with the attribute values for each point
    pts.df <- data.frame(ptid, id)
    if (!is.null(dt)) pts.df <- data.frame(pts.df, dt)
    if (!is.null(col)) pts.df <- data.frame(pts.df, col)
    if (!is.null(anv)) pts.df <- data.frame(pts.df, anv)

    ## Create a spatial points data frame object    
    pts <- SpatialPointsDataFrame(coords=xy, data=pts.df, proj4string=proj4string, match.ID=FALSE)
                
    res <- list(pts=pts, dt.int=dt.int, rw.params=rw.params, anv=anv.df, comment=comment)
    class(res) <- "locoh.lxy"
    attr(res, "tlocoh_ver") <- packageVersion("tlocoh")
    return(res)
    
}
