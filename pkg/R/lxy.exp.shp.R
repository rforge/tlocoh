#' Export a LoCoH-xy to shapefile
#'
#' Export hulls, hull parent points, and/or isopleths from a locoh-hullset object to ESRI's shapefile format
#'
#' @param lhs A LoCoH-hullset object
#' @param id The name(s) of individuals to export
#' @param k The k value of hullsets to export
#' @param r The r value of hullsets to export
#' @param a The a value of hullsets to export
#' @param s The s value of hullsets to export
#' @param hs.names The name(s) of saved hullsets to export
#' @param hulls Export hulls (as a polygon shapefile). T/F
#' @param hpp Export hull parent points (as a point shapefile). T/F
#' @param iso Export isopleths (as a polygon shapefile). T/F
#' @param iso.metric Hull sort metric(s) for the isopleths that will be exported (acts a filter). Character vector.
#' @param iso.idx Numeric vector of the indices of the isopleths that will be exported (acts a filter)
#' @param ellipses Export ellipses (as a polygon shapefile). T/F
#' @param allpts Export all points (as a point shapefile). T/F
#' @param dir The directory where the shapefiles will be placed (use "." for the working directory, and "~" for the user directory)
#' @param file.base The base of the file name without any extension (the script will append to this)
#' @param file.base.auto Whether to automatically generate the file name base from the id, s-value, and k-a-r value. Ignored if \code{file.base} is passed. T/F
#' @param projargs A character string that contains info about the coordinate system the data are projected in
#' @param prj.file The name of an existing ArcView *.prj file, which contains projection info
#' @param avl.file The name of an existing avl (ArcView legend) file
#' @param status Show messages. T/F
#' @param show.time Report time for script to complete. T/F
#' @param hm The name of hull metric(s) that will be included in the attribute table. Default is 'all'. Applicable only to hulls and hull parent points. Character vector or comma separated string.
#' @param anv The name of ancillary variables(s) of the parent point that will be included in the attribute table. Default is NULL. Applicable only to hulls and hull parent points. Character vector or comma separated string.
#' @param hsp A list containing one hull scatterplot object with regions saved (i.e., object returned by \code{\link{lhs.plot.scatter}}), or the index of a hull scatter plot saved in the hullset (see \code{\link{lhs.hsp.add}}). If passed, a column for the region number will be included in the attribute table. Applicable only to hulls and hull parent points. 
#' @param metadata Export field descriptions in a meta data file (not yet supported)
#'
#' @note 
#' Filenames are automatically generated, but the user can specify the directory. Constructed filenames will consist 
#' of the \code{file.base} (if provided) followed by the name of the hullset and a suffix indicating the type
#' of feature saved (e.g., '.hulls', '.pts', ".iso", etc. ). If a shapefile already exists with this, 
#' a unique filename will be constructed using two-digit numeric extension
#'
#' Most of the time, exporting hull parent points (\code{hpp=T}) is the same as exporting all 
#' points (\code{allpts=T}), however in some cases not all points have enough neighbors to make a hull 
#' so they wouldn't be included in exported of hull parent points. Only hull parent points will have the
#' corresponding hull metrics included in the attribute table.
#'
#' Passing a value for \code{projargs} will not re-project the data, only describe the existing projection. 
#' The projarg string must follow the format exactly as in the PROJ.4 documentation, in particular 
#' there cannot be any white space in the syntax '+<arg>=<value> strings', and successive such strings 
#' can only be separated by blanks. An example is \code{"+proj=utm +south +zone=33"}. 
#' See also \link{http://hosho.ees.hokudai.ac.jp/~kubo/Rdoc/library/sp/html/CRS-class.html}
#' and the main users manual for PROJ available at \link{http://trac.osgeo.org/proj}. 
#'
#' In lieu of projargs, you can supply an argument for \code{prj.file} (but not both). The script
#' will make a copy of prj.file with the same base name as the shapefile. Prj.file is presumed 
#' to be relative to the working directory.
#'
#' avl.file is an ArcView 3.x file that contains symbology info. If a value is passed, 
#' the script will make a copy of prj.file with the same base name as the shapefile, so that 
#' symbology will automatically created in ArcMap. avl.file is presumed to be relative
#' to the working folder
#'
#' @seealso \code{\link{lhs.exp.csv}}
#' @export

lxy.exp.shp <- function(lhs, id=NULL, k=NULL, r=NULL, a=NULL, s=NULL, hs.names=NULL,
               hpp=FALSE, hulls=FALSE, iso=FALSE, nn=FALSE, ellipses = FALSE, allpts = FALSE, iso.idx=NULL, iso.metric=NULL, 
               dir=".", file.base="", file.base.auto=TRUE, avl.file = NULL, status = TRUE,
               show.time=TRUE, hm="all", anv=NULL, hsp=NULL, metadata=TRUE) {

    stop("Not yet working")

# taken out: prj.file = NULL, proj4string

    if (!inherits(lhs, "locoh.lhs")) stop("lhs should be of class \"locoh.lhs\"")
    if (!require(sp)) stop("package sp required")
    if (!require(rgdal)) stop("package rgdal required")

    if (!hpp && !hulls && !iso && !nn && !ellipses && !allpts) stop(cw("Don't know what to export. Set at least one of the following to TRUE: hpp, hulls, iso, nn, ellipses, or allpts"))
    if (!file.exists(dir)) stop(paste("Output directory doesn't exist:", dir))

    hme <- hm.expr(names.only=FALSE, print=FALSE)
    start.time <- Sys.time()
    #proj4string = CRS(as.character(projargs))
    
    hm <- vectorize.parameter(hm, type="character")
    anv <- vectorize.parameter(anv, type="character")
    
    if (!is.null(iso.metric)) {if (!iso.metric %in% names(hme)) stop("Illegal value for iso.metric")}

    if (is.null(id) && is.null(r) && is.null(k) && is.null(a) && is.null(s) && is.null(hs.names)) {
        hs <- lhs
    } else {    
        hs <- lhs.select(lhs, id=id, r=r, k=k, a=a, s=s, hs.names=hs.names)
    }
    if (length(hs)==0) stop("No hullsets found matching those criteria")
                             
    if (is.null(file.base)) stop("You must enter a base filename: file.base")

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
    
    ## Make sure the avl file exist (if passed)
    if (!is.null(avl.file)) {if (!file.exists(avl.file)) stop(paste(avl.file, "not found"))}
    
    ## Strip the 'shp' extension from file.base if it exists
    if (file.base != "") {
        if(tolower(substr(file.base, nchar(file.base) - 3, nchar(file.base))) == '.shp') {
            file.base <- substr(file.base, 0, nchar(file.base) - 4)
        }
    }

    ## Create a collection of the name(s) of run(s) to include
    if (is.null(hs.names)) hs.names <- names(hs)

    ## Keep track of the shapefile(s) saved
    files.saved <- character()

    ## Loop through the names of hullsets
    for (hs.name in hs.names) {
        
        hmap <- as.data.frame(NA)
        
        # Get the hull metrics that will be exported with hulls and hpp
        if (hpp || hulls) {
        
            ## Create a vector of the indices of the hull parent points
            hulls.pp.idx <- hs[[hs.name]][["hulls"]][["pts.idx"]]
    
            ## Get the id, dropping unused levels
            id.col <- hs[[hs.name]][["pts"]][["id"]][hulls.pp.idx, drop=TRUE]
    
            ## Start a data frame with the id, parent point ptid, the parent point coordinates,
            csv.data <- data.frame(id=id.col, ptid=hs[[hs.name]][["pts"]][["ptid"]][hulls.pp.idx])
    
            ## Add dt next
            if (!is.null(hs[[hs.name]][["pts"]][["dt"]])) csv.data <- cbind(csv.data, dt=hs[[hs.name]][["pts"]][["dt"]][hulls.pp.idx])
    
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
    
            ## Make sure all column names are < 10 chars with no periods for DBF 
            names(csv.data) <- substr(gsub(".", "_", names(csv.data), fixed=T), 1, 10)

            ## Make sure all column names are unique, attempt to fix duplicates if found
            i <- 0
            while(anyDuplicated(names(csv.data))>0) {
                i <- i + 1; if (i > 99) stop("can't construct unique attribute field name")
                names(csv.data)[duplicated(names(csv.data))] <- paste(substr(names(csv.data)[duplicated(names(csv.data))], 1, 7), "_", sprintf("%02d", i), sep="")
            }

            ## Create hmap (hull metric auxillary parameters for computing hsp values
            ## We need to construct this data frame because if there is an HSP with regions, we need to 
            ## compute the hull metric values which means evaluating the expressions returned by hm.expr()
            ## which may in turn expects objects called 'hmap' and 'hmap.idx'
            
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
            
            if (!is.null(hsp.reg)) csv.data <- cbind(csv.data, hsp_reg=hsp.reg)
            
            
            ## Construct the file name base for all of these runs
            if (file.base == "") {
                if (file.base.auto) {
                    file.base.use <- with(hs[[hs.name]], paste(id, ".s", s, if (is.null(k)) NULL else paste(".k", k, sep=""), if (is.null(a)) NULL else paste(".a", a, sep=""), sep=""))
                } else {
                    file.base.use <- ""
                }
            } else {
                file.base.use <- file.base
            }
            
            if (hpp) {
                ## Construct a name for this shapefile (minus extension)
                shp.fn <- paste(file.base.use, if (file.base.auto) "" else hs.name, sep = "")
                    
                ## If shapefile already exists, add numeric suffix (still no ext)
                fn.shp.unique.str <- fn.unique(shp.fn, dir=dir, suf = "hpp", ext = "shp", first.one.numbered=TRUE)

                if (status) {
                    cat("   Saving ", fn.shp.unique.str, ".shp \n", sep="")
                    flush.console()
                }
                
                hpp.spdf <- hs[[hs.name]][["pts"]][hulls.pp.idx, ]
                hpp.spdf@data <- csv.data
                #print("just about to save hpp");browser()
                
                writeOGR(hpp.spdf, dsn=dirname(fn.shp.unique.str), layer=basename(fn.shp.unique.str), driver="ESRI Shapefile")
                files.saved <- c(files.saved, fn.shp.unique.str)
            }
            
            
            if (hulls) {
    
                ## Construct a name for this shapefile (minus extension)
                shp.fn <- paste(file.base.use, if (file.base.auto) "" else hs.name, sep = "")
                    
                ## If shapefile already exists, add numeric suffix (still no ext)
                fn.shp.unique.str <- fn.unique(shp.fn, dir=dir, suf = "hulls", ext = "shp", first.one.numbered=TRUE)
    
                if (status) {
                    cat("   Saving ", fn.shp.unique.str, ".shp \n", sep="")
                    flush.console()
                }

                hs[[hs.name]][["hulls"]]@data <- csv.data
                
                writeOGR(hs[[hs.name]][["hulls"]], dsn=dirname(fn.shp.unique.str), layer=basename(fn.shp.unique.str), driver="ESRI Shapefile")
                 
                files.saved <- c(files.saved, fn.shp.unique.str)
                
            }   # if hulls
            
            if (iso) {
                isos.idx.use <- 1:length(hs[[hs.name]][["isos"]])
                if (!is.null(iso.idx)) isos.idx.use <- intersect(isos.idx.use, iso.idx)
    
                if (!is.null(iso.metric)) {
                    isos.idx.use <- intersect(isos.idx.use, (1:length(hs[[hs.name]][["isos"]]))[as.vector(sapply(hs[[hs.name]][["isos"]], function(x) x[["sort.metric"]] %in% iso.metric))])
                }
                
                if (length(isos.idx.use) == 0) {
                    cat("  ", hs.name, ": no matching isopleths found \n", sep="")
                } else {
                    
                    isos.names <- names(hs[[hs.name]][["isos"]])
                    
                    ## Loop through the isopleths stored with this set of hulls
                    for (iidx in isos.idx.use) {
                        
                        ## Construct a name for this shapefile (minus extension)
                        shp.fn <- paste(file.base.use, ".", isos.names[iidx], sep = "")
                            
                        ## If shapefile already exists, add numeric suffix (still no ext)
                        fn.shp.unique.str <- fn.unique(shp.fn, dir=dir, suf = "iso", ext = "shp", first.one.numbered=TRUE)
    
                        if (status) {
                            cat("   Saving ", fn.shp.unique.str, ".shp \n", sep="")
                            flush.console()
                        }
                        
                        ## Make sure all column names are < 10 chars with no periods for DBF 
                        names(hs[[hs.name]][["isos"]][[iidx]][["polys"]]@data) <- substr(gsub(".", "_", names(hs[[hs.name]][["isos"]][[iidx]][["polys"]]@data), fixed=T), 1, 10)
                        
                        ## Create a SpatialPolygonDataFrame
                        writeOGR(hs[[hs.name]][["isos"]][[iidx]][["polys"]][hs[[hs.name]][["isos"]][[iidx]][["polys"]]@plotOrder, ], dsn=dirname(fn.shp.unique.str), layer=basename(fn.shp.unique.str), driver="ESRI Shapefile")
                        files.saved <- c(files.saved, fn.shp.unique.str)
                    
                    }
                                
                }
               
                ## figure out how to specify which hull metrics should go in the attribute table (including those based on time-use-space color values)
            
            }
            
            if (nn) stop("Exporting nn is not yet supported")
            if (ellipses) stop("Exporting ellipses is not yet supported")
            if (allpts) stop("Exporting allpts is not yet supported")
        
        }
          
    } # for each hs.name

    if (length(files.saved) == 0) {
        if (status) cat("No shapefiles were saved \n")
    } else {
        ## Copy avl files if needed
        if (!is.null(avl.file)) {
            for (thisfile in files.saved) {
                file.copy(avl.file, paste(thisfile, ".avl", sep=""))
            }
        }

        if (show.time) {
            time.taken = difftime(Sys.time(), start.time, units="auto")
            cat("Total time:", round(time.taken,1), units(time.taken), "\n", sep = " ")
        }
        
    }
     
}
