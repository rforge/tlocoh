#' Export a LoCoH-hullset to csv
#'
#' Export hull metrics to a csv file / data frame
#'
#' @param lhs A LoCoH-hullset object
#' @param id The name(s) of individuals to export
#' @param k The k value of hullsets to export
#' @param r The r value of hullsets to export
#' @param a The a value of hullsets to export
#' @param s The s value of hullsets to export
#' @param hs.names The name(s) of saved hullsets to analyze
#' @param csv.save Whether to create csv file(s). If FALSE, a list of data frames will still be returned. T/F
#' @param dir The directory where the csv file(s) will be saved
#' @param csv.fn.pre A prefix for the csv file name(s)
#' @param csv.fn.mid Style of the filename mid-section: none, short, or long
#' @param hm The name of hull metric(s) that will be exported. Character vector or comma separated string.
#' @param anv The name(s) of ancillary variables(s) of the parent point of each hull to be exported. Default is NULL, can also be set to 'all'. Character vector or comma separated string.
#' @param hsp A list containing one hull scatterplot object with regions saved (i.e., object returned by \code{\link{lhs.plot.scatter}}), or the index of a hull scatter plot saved in the hullset (see \code{\link{lhs.hsp.add}}). If passed, a column for the region number will be included in the attribute table. Applicable only to hulls and hull parent points. 
#' @param metadata Export field descriptions in a meta data file (not yet supported)
#' @param status Show messages. T/F
#'
#' @return List (one list element for each hullset) of data frames of hull metrics and possibly ancillary variables
#'
#' @note 
#' This function exports hull metrics only. If you want to export the coordinates of the hulls themselves for further analysis in a GIS, see \code{/link{lhs.exp.shp}}.
#'
#' If \code{file} is left blank, no csv file will be written to disk but the function will still return a list of data frames (one for each hullset)
#' 
#' If you have created a scatterplot legend with manually defined regions (see tutorial for details), the region number of each hull parent 
#' point can be included in the csv file if you pass the hullscatter plot as parameter \code{hsp}.
#'
#' @seealso \code{\link{lhs.exp.shp}}
#' @export

lhs.exp.csv <- function(lhs, id=NULL, k=NULL, r=NULL, a=NULL, s=NULL, hs.names=NULL,
               csv.save=TRUE, dir=".", csv.fn.pre=NULL, csv.fn.mid=c("none","short","long")[2],
               hm="all", anv=NULL, hsp=NULL, status = TRUE) {

    if (!inherits(lhs, "locoh.lhs")) stop("lhs should be of class \"locoh.lhs\"")
    if (!require(sp)) stop("package sp required")
    if (csv.save && !file.exists(dir)) stop(paste("Output directory doesn't exist:", dir))

    hme <- hm.expr(names.only=FALSE, print=FALSE)
    hm <- vectorize.parameter(hm, type="character")
    anv <- vectorize.parameter(anv, type="character")

    if (!is.null(hm) && !identical(hm,"all")) if (FALSE %in% (hm %in% names(hme))) stop("Unknown hull metric name")

    if (is.null(id) && is.null(r) && is.null(k) && is.null(a) && is.null(s) && is.null(hs.names)) {
        hs <- lhs
    } else {    
        hs <- lhs.select(lhs, id=id, r=r, k=k, a=a, s=s, hs.names=hs.names)
    }
    if (length(hs)==0) stop("No hullsets found matching those criteria")
                                 
    #if (is.null(file.base)) stop("You must enter a base filename: file.base")

    ## Do some error checking on hsp if passed
    if (!is.null(hsp)) {
        err.msg <- cw("hsp must be either a valid index of a saved scatter plot(s), or a list of objects of class 'locoh.hsp'", final.cr=FALSE, exdent=2)
        if (is.numeric(hsp)) {
            if (min(hsp) < 1 || max(hsp) > sapply(hs, function(x) length(x$hsp))) stop(err.msg)
        } else if (is.list(hsp)) {
            if (FALSE %in% (sapply(hsp, function(x) class(x)=="locoh.hsp"))) stop(err.msg)
        } else {
             stop(err.msg)
        }
        if (length(hsp)>1) warning("Only the first scatterplot will be used")
    }
    
    ## Strip the 'csv' extension from csv.fn.pre if it exists
    if (csv.save && !is.null(csv.fn.pre)) {
        if(tolower(substr(csv.fn.pre, nchar(csv.fn.pre) - 3, nchar(csv.fn.pre))) == '.csv') {
            csv.fn.pre <- substr(csv.fn.pre, 0, nchar(csv.fn.pre) - 4)
        }
    }

    ## Create a collection of the name(s) of run(s) to include
    if (is.null(hs.names)) hs.names <- names(hs)

    ## Keep track of the csv file(s) saved
    files.saved <- character()
    res <- list()

    ## Loop through the names of runs
    for (hs.name in hs.names) {
        
        ## Create a vector of the indices of the hull parent points
        hulls.pp.idx <- hs[[hs.name]][["hulls"]][["pts.idx"]]

        ## Grab the coordinates of the hull parent points
        coords.mat <- coordinates(hs[[hs.name]][["pts"]][hulls.pp.idx, ])
        #colnames(coords.mat) <- c("x","y")

        ## Get the id, dropping unused levels
        id.col <- hs[[hs.name]][["pts"]][["id"]][hulls.pp.idx, drop=TRUE]

        ## Start a data frame with the id, parent point ptid, the parent point coordinates,
        csv.data <- data.frame(id=id.col, ptid=hs[[hs.name]][["pts"]][["ptid"]][hulls.pp.idx], coords.mat)

        ## Add dt next
        if (!is.null(hs[[hs.name]][["pts"]][["dt"]])) csv.data <- cbind(csv.data[,1:2], dt=hs[[hs.name]][["pts"]][["dt"]][hulls.pp.idx], csv.data[,3:4])

        ## Add hull metrics next
        # Get the hull metrics that will be exported
        hm.use <- NULL
        if (identical(hm, "all")) {
            hm.use <- names(hs[[hs.name]][["hm"]])
        } else if (!is.null(hm)) {
            hm.use <- names(hs[[hs.name]][["hm"]])[sapply(hs[[hs.name]][["hm"]], function(x) x[["type"]] %in% hm)]
            if (length(hm.use)==0) stop("Hull metric(s) not found")
        }
        if (!is.null(hm.use)) csv.data <- cbind(csv.data, hs[[hs.name]][["hulls"]]@data[,hm.use])
        
        ## Finally add anv
        ## Identify the anv fields present
        anv.use <- NULL
        if (identical(anv, "all")) {
            anv.use <- as.character(hs[[hs.name]][["anv"]][["anv"]])
        } else if (!is.null(anv)) {
            anv.use <- intersect(anv, as.character(hs[[hs.name]][["anv"]][["anv"]]))
            if (length(anv.use)==0) stop("Ancillary variable(s) not found")
        }
        if (!is.null(anv.use)) csv.data <- cbind(csv.data, hs[[hs.name]][["pts"]][hulls.pp.idx, anv.use])

        ## Create hmap (hull metric auxillary parameters for computing hsp values
        hmap <- as.data.frame(NA)
        if (is.null(hsp)) {
            hsp.use <- NULL
        } else {
            if (length(hsp)>1) warning("Multiple hull scatterplots found. Only the first one will be used")
            if (is.list(hsp)) {
                hsp.use <- hsp[[1]]
                hmap <- as.data.frame(hsp[[1]][["hmap"]])
            } else if (is.numeric(hsp)) {
                hsp.use <- hs[[hs.name]][["hsp"]][1][[1]]
                hmap <- as.data.frame(hsp.use[["hmap"]])
                if (is.null(hmap)) stop("Cant find stored hmap")
            } else {
                stop(cw("hsp must be a list of hsp objects, or the index of a saved hsp object in lhs", final.cr=FALSE, exdent=2))
            }
        } 
    

        for (hmap.idx in 1:nrow(hmap)) {

            ## Compute hsp values if needed
            hsp.reg <- NULL
            if (!is.null(hsp.use)) {
                hsp.xvals <- eval(hme[[hsp.use$x.axis]][["expr"]])
                hsp.yvals <- eval(hme[[hsp.use$y.axis]][["expr"]])
                if (length(hsp.xvals)==0 || length(hsp.yvals)==0) stop("Can't find values for saved hull scatter plot")

                ## Transform the x and y values if needed
                if (!is.null(hsp.use$trans.x)) hsp.xvals <- get(hsp.use$trans.x)(hsp.xvals)
                if (!is.null(hsp.use$trans.y)) hsp.yvals <- get(hsp.use$trans.y)(hsp.yvals)

                ## If there are regions, assign each point to a region number
                if (is.null(hsp.use$regions)) {
                    stop(cw("Can not export the hull scatterplot region for each point because regions are not defined in this hsp", final.cr=FALSE, exdent=2))
                } else {
                    hsp.reg <- hsp.col.reg(hsp.xvals, hsp.yvals, regions=hsp.use$regions, return.reg.num.only=TRUE)
                }
            }

            ## Add hsp regions
            if (!is.null(hsp.reg)) csv.data <- cbind(csv.data, hsp_reg=hsp.reg)

            ## Make sure all column names are unique, attempt to fix duplicates if found
            i <- 0
            while(anyDuplicated(names(csv.data))>0) {
                i <- i + 1; if (i > 99) stop("can't construct unique attribute field name")
                names(csv.data)[duplicated(names(csv.data))] <- paste(substr(names(csv.data)[duplicated(names(csv.data))], 1, 7), "_", sprintf("%02d", i), sep="")
            }

            ## Construct a base name for this hullset which will be used for both the csv file and list element name
            lhs.name.short <- with(hs[[hs.name]], paste(id, ".s", s, if (is.null(k)) NULL else paste(".k", k, sep=""), if (is.null(a)) NULL else paste(".a", a, sep=""), sep=""))
            if (csv.save) {
                csv.fn.pre.char <- if (!is.null(csv.fn.pre) && substr(csv.fn.pre, nchar(csv.fn.pre), nchar(csv.fn.pre)) != ".") "." else NULL


                if (csv.fn.mid=="none") {
                    csv.fn.mid.str <- NULL
                } else if (csv.fn.mid=="short") {
                    csv.fn.mid.str <- lhs.name.short
                } else if (csv.fn.mid=="long") {
                    csv.fn.mid.str <- hs.name
                } else {
                    stop("Unknown value for csv.fn.mid")
                }


                ## Construct a name for the csv file (minus extension)
                csv.fn <- paste(csv.fn.pre, csv.fn.pre.char, csv.fn.mid.str, sep = "")

                ## If shapefile already exists, add numeric suffix (still no ext)
                fn.csv.unique.str <- fn.unique(csv.fn, dir=dir, suf = "", ext = "csv", first.one.numbered=TRUE, exclude.ext=FALSE)

                write.csv(csv.data, file=fn.csv.unique.str, row.names = FALSE)
                if (status) cat("  ", fn.csv.unique.str, "\n", sep=""); flush.console()
                files.saved <- c(files.saved, fn.csv.unique.str)

            }

            ## Add data frame to result
            res[[lhs.name.short]] <- csv.data
        
        }
          
    } # for each hs.name

    if (csv.save && status) {
        if (length(files.saved) == 0) {
            cat("No csv files were saved \n")
        }
    }
    return(res)
     
}
