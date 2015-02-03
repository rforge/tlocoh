shp.layers <- function(layers=NULL, shp.csv=NULL, names.only=FALSE, delete_null_obj=FALSE) {
    ## Given the character vector containing the name(s) of layers (may be comma separated),
    ## will return a named list whose elements are list containing symbology options (pch, line width, etc.)
    ## and spatial data frame. 

    ## layers may be a character vector, in which case it should be a list of layer names (either a vector or comma-separated list)
    ## or
    ## it can be a list in which each element is a list with the following structure
    ##   $layer : layer name
    ##   $fn : file name of the shape file (req)
    ##   $type: 'polygon', 'line', or 'point' (req)
    ##   $lty: line type: 0=blank, 1=solid, 2=dashed, 3=dotted, 4=dotdash, 5=longdash, 6=twodash
    ##   $pch: plot character
    ##   $cex: expansion factor for plot characters
    ##   $lwd: line width
    ##   $border: border color (polygons only, to hide border set to NA)
    ##   $col: color of the fill / plot character / line

    ## shp.csv file contains the paths to shapefiles and display parameters
    ## If an item in layers is not found in the shp.csv, an error will occur

    if (is.null(layers)) {
        return(list())
    } else if (is.list(layers)) {
        shp.layers.lst <- layers
        err.msg <- "Incorrect list structure for parameter 'layers'"
        all.list.elements <- c("layer", "fn", "type", "lty", "pch", "cex", "lwd", "border", "col")
        
        if (!is.list(shp.layers.lst[[1]])) stop(err.msg)
        
        for (i in 1:length(shp.layers.lst)) {
            if (FALSE %in% (names(shp.layers.lst[[i]]) %in% all.list.elements)) stop(paste(err.msg, ". Unknown element name.", sep=""))
        
            ## Make sure all of the required elements are present
            if (FALSE %in% (c("fn", "type") %in% names(shp.layers.lst[[i]]))) stop(paste(err.msg, ". Required element missing.", sep=""))

            if (!shp.layers.lst[[i]]$type %in% c("point","line","polygon")) stop(paste(err.msg, ". Unknown feature type. ", sep=""))

            ## Set missing values to defaults
            if (is.null(shp.layers.lst[[i]]$border)) shp.layers.lst[[i]]$border <- "gray50"
            if (is.null(shp.layers.lst[[i]]$lwd)) shp.layers.lst[[i]]$lwd <- 1
            if (is.null(shp.layers.lst[[i]]$lty)) shp.layers.lst[[i]]$lty <- 1
            if (is.null(shp.layers.lst[[i]]$cex)) shp.layers.lst[[i]]$cex <- 1
            if (is.null(shp.layers.lst[[i]]$pch)) shp.layers.lst[[i]]$pch <- 16
            
            if (is.null(shp.layers.lst[[i]]$col)) {
                if (shp.layers.lst[[i]]$type=="polygon") {
                    shp.layers.lst[[i]]$col <- NA   ## transparent
                } else {
                    shp.layers.lst[[i]]$col <- "gray20"
                }
            }
            
            ## See default values for graphic arguments for polygons
            
            ## Set all other missing parameters to NA
            #shp.layers.lst[[i]][all.list.elements[!all.list.elements %in% names(shp.layers.lst[[i]])]] <- NA
        }
        
    } else {
        if (is.null(shp.csv)) {
            cat("Missing parameter: shp.csv\n", sep="")
            return(list())
        }
        
        if (!file.exists(shp.csv)) {                                         
            ## See if shp.csv is in the pacakge 'data' folder
            if (file.exists(file.path(path.package("tlocoh"), "shps", shp.csv))) {
                shp.csv <- file.path(path.package("tlocoh"), "shps", shp.csv)
            } else {
                cat(paste(shp.csv, " not found \n", sep=""))
                return(list())
            }
        }
        if (!names.only && (length(layers) == 0 || identical(layers, ""))) return(list())
        layers <- vectorize.parameter(layers, type="character")
        
        ## Read the csv file and convert it to a list
        g <- read.table(shp.csv, header=TRUE, sep=",", stringsAsFactors=FALSE, strip.white=TRUE)

        ## Check for the correct column names. This should also catch
        ## if the user edited the sample csv file in Excel
        colNames <- c("layer", "fn", "type", "lty", "pch", "cex", "lwd", "border", "col")
        if (FALSE %in% (names(g) %in% colNames)) stop(cw(paste("Incorrect column name(s) in ", shp.csv, ". Open the csv file in a text editor and compare it to the sample in ", system.file("shps", "kruger_gis.csv", package="tlocoh"), ".", sep=""), exdent=2, final.cr=F)) 
        
        ## Turn the dataframe into a list of individual layer parameters
        shp.layers.lst <- lapply(split(g, g$layer), function(x) as.list(x))
        if (length(shp.layers.lst)==0) stop(paste("No shapefile layers found listed in ", shp.csv, sep=""))
        
        if (names.only) {
            cat("Layers found in ", shp.csv, ":\n", sep="")
            cat(paste("   ", sort(names(shp.layers.lst)), "\n", sep=""), sep="")
            return(invisible(names(shp.layers.lst)))
        }
        
        ## Do a quick check of the layer type
        if (FALSE %in% (sapply(shp.layers.lst, function(x) x$type) %in% c("point","line","polygon"))) stop(paste("Error in ", shp.csv, ". Type must be 'point', 'line' or 'polygon'.", sep=""))
    
        ## Make sure all layer names passed are in the CSV
        if (FALSE %in% (layers %in% names(shp.layers.lst))) {
            err.msg <- cw(paste("Layer(s) not found in ", shp.csv, ": ", paste(layers[!layers %in% names(shp.layers.lst)], collapse=", ", sep=""), sep=""), exdent=2, final.cr=F)
            stop(err.msg)
        }
        
        ## Filter shp.layers.lst to only those layers that are needed
        shp.layers.lst <- shp.layers.lst[layers]

    }
                            
    ## Read each shp file and fill the in the $sdf element with a spatial data frame
    if (!requireNamespace("rgdal", quietly=TRUE)) stop("package rgdal required")
    for (i in 1:length(shp.layers.lst)) {
        fn.shp <- shp.layers.lst[[i]][["fn"]]
        if (!file.exists(fn.shp)) {
            ## See if its is in the same folder as the shp.csv file
            if (!is.list(layers)) fn.shp <- file.path(dirname(shp.csv), shp.layers.lst[[i]][["fn"]])
            
            ## If it still doesn't exist, give an error message
            if (!file.exists(fn.shp)) stop(paste("File not found: ", shp.layers.lst[[i]][["fn"]], sep=""))
        }
        shp.dir <- dirname(fn.shp)
        shp.base <- substr(basename(fn.shp), 0, nchar(basename(fn.shp))-4)
        shp.layers.lst[[i]][["sdf"]] <- rgdal::readOGR(dsn=shp.dir, layer=shp.base, verbose=FALSE)
    }
    return(shp.layers.lst)
}
