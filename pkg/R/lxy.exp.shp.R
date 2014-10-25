#' Export a LoCoH-xy to shapefile
#'
#' Export a LoCoH-xy object to point shapefile
#'
#' @param lxy A \link{LoCoH-xy} object
#' @param id The name(s) of individuals to export. If \code{NULL} all individuals will be exported
#' @param dir The directory where the shapefiles will be placed (use "." for the working directory, and "~" for the home directory)
#' @param file.base The base of the file name without any extension (the script will append to this)
#' @param anv The name of ancillary variables(s) to be exported in the attribute table. Character vector or comma separated string.
#' @param idsSeparate Whether to export the locations for each individual in separate shapefiles. T/F
#' @param autoName The rule for constructing a filename: \code{short} (consisting of the id(s) only), \code{long} (id + num points), or \code{none}
#' @param status Show messages. T/F
#'
#' @return A character vector of the shapefile(s) created
#'
#' @details
#' Filenames are automatically generated, but the user can specify the directory and a base (\code{file.base}). If a shapefile already exists,
#' a unique filename will be constructed using two-digit numeric extension. For shorter filenames, pass \code{autoName="short"}.
#'
#' Use the \code{anv} argument to specify which ancillary variables (i.e., attribute fields) should be exported. To not export
#' any ancillary variables, pass an empty string.
#'
#' Only the individual locations and their ancillary values are exported. The nearest neighbor table is not exported.
#'
#' @seealso \code{\link{lxy.exp.kml}}, \code{\link{lxy.exp.mov}}
#'
#' @export

lxy.exp.shp <- function(lxy, id=NULL, dir=".", file.base="", anv="all", idsSeparate=FALSE, autoName=c("long","short","none")[1], status=TRUE) {

    if (!inherits(lxy, "locoh.lxy")) stop("lxy should be of class \"locoh.lxy\"")
    if (!requireNamespace("rgdal")) stop("package rgdal required")
    if (!autoName %in% c("long","short","none")) stop("Unknown value for autoName")
    if (!file.exists(dir)) stop(paste("Output directory doesn't exist:", dir))
    if (autoName=="none" && file.base=="") stop("If you specify autoName=none, you must pass a value for file.base")

    anv <- vectorize.parameter(anv, type="character")
    id <- vectorize.parameter(id, type="character")

    if (is.null(id)) {
        id <- levels(lxy[["pts"]][["id"]])
        idx.all <- 1:nrow(lxy[["pts"]])
    } else {
        if (FALSE %in% (id %in% levels(lxy[["pts"]][["id"]]))) stop("id value(s) not found")
        idx.all <- which(lxy[["pts"]][["id"]] %in% id)
    }
    
    ## Strip the 'shp' extension from file.base if it exists
    if (file.base != "") {
        if(tolower(substr(file.base, nchar(file.base) - 3, nchar(file.base))) == '.shp') {
            file.base <- substr(file.base, 0, nchar(file.base) - 4)
        }
    }

    ## Keep track of the number of shapefile(s) saved
    files.saved <- NULL

    ## Identify the columns to export
    anv.all <- names(lxy$pts@data)
    if (is.null(anv) || identical(anv, "all")) {
        anv.use <- anv.all
    } else if (length(anv)> 0) {
        anv.use <- intersect(anv, anv.all)
        if (length(anv.use)==0) stop("Ancillary variable(s) not found")
    } else {
        ## writeOGR requires at least one column in the attached data frame
        anv.use <- "ptid"
    }
    
    ## Make sure all column names are < 10 chars with no periods for DBF 
    anvNamesCleaned <- substr(gsub(".", "_", anv.use, fixed=T), 1, 10)

    ## Make sure all column names are unique, attempt to fix duplicates if found
    i <- 0
    while(anyDuplicated(anvNamesCleaned) > 0) {
        i <- i + 1; if (i > 99) stop("can't construct unique attribute field name")
        anvNamesCleaned[duplicated(anvNamesCleaned)] <- paste(substr(anvNamesCleaned[duplicated(anvNamesCleaned)], 1, 7), "_", sprintf("%02d", i), sep="")
    }

    if (status) {
        cat("   Attribute fields: ", paste(anvNamesCleaned, collapse=", ", sep=""), "\n", sep="")
        flush.console()
    }

    ##  Make a list of the shapefiles to create and the indices that will go in each one
    ids.lst <- list()
    if (idsSeparate) {
        for (myid in id) {
            if (autoName=="none") {
                fn <- ""
            } else if (autoName=="short") {
                fn <- myid
            } else {
                fn <- lxy$comment[[myid]]
            }
            ids.lst[[myid]] <- list(file=fn, idx=which(lxy[["pts"]][["id"]] == myid))
        }    
    } else {
        if (autoName=="none") {
            fn <- ""
        } else {
            ## short or long
            fn <- paste(id, collapse=".", sep="")
            if (autoName=="long") {
                fn <- paste(fn, ".n", length(idx.all), sep="")
            }
        }

        ## Make sure we haven't constructed a super long file name
        if (nchar(fn) > 50) {
            fn <- substr(fn, 1, 50)
            warning("Many individuals make for a long filename. Try passing a value for file.base and setting autoName='none'.")
        }
        ids.lst[["all"]] <- list(file=fn, idx=idx.all)
    }
    
    for (myid in names(ids.lst)) {

        ## Construct a name for this shapefile (minus extension)
        shp.fn <- paste(file.base, ids.lst[[myid]][["file"]], sep = "")

        ## If shapefile already exists, add numeric suffix (still no ext)
        fn.shp.unique.str <- fn.unique(shp.fn, dir=dir, suf = "", ext = "shp", first.one.numbered=FALSE)

        if (status) {
            cat("   Saving ", fn.shp.unique.str, ".shp \n", sep="")
            flush.console()
        }

        pts.spdf <- lxy[["pts"]][ids.lst[[myid]][["idx"]], anv.use]
        names(pts.spdf@data) <- anvNamesCleaned

        rgdal::writeOGR(pts.spdf, dsn=dirname(fn.shp.unique.str), layer=basename(fn.shp.unique.str), driver="ESRI Shapefile")
        files.saved <- c(files.saved, fn.shp.unique.str)
    }

    if (length(files.saved) == 0) {
        if (status) cat("No shapefiles were saved \n")
    } else {
        if (status) {
            cat(length(files.saved), "shapefile(s) saved\n")
        }
    }
    
    return(invisible(files.saved))
     
}
