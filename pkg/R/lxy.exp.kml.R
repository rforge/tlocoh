#' Export a lxy object to kml (Google Earth)
#'
#' Export a lxy object to a format that can be animated in Google Earth
#'
#' @param lxy A LoCoH-xy object
#' @param file The name of the kml file to be created (relative to the working folder). The .kml extension will be added if needed.
#' @param id The id(s) of the individual(s) to include in the plot
#' @param skip The skip factor to use to reduce the number of points exported (i.e., every nth point exported where n=\code{skip})
#' @param overwrite Whether to overwrite an existing kml file (T/F)
#' @param compress Whether to copmress the kml file and create a kmz file (T/F)
#' @param pt.scale The relative size of the placemark symbols
#' @param col The point colors (one per id)
#' @param show.path Whether to include the point path in the kml file (T/F)
#' @param path.col The path colors (one per id), ignored if \code{show.path=FALSE}
#' @param path.opaque The opacity of the path (0=transparent, 255=fully opaque), ignored if \code{show.path=FALSE}
#' @param path.lwd The width of the path, ignored if \code{show.path=FALSE}
#'
#' @return The file name of the kml/kmz file 
#'
#' @note This function will export the locations in a \link{LoCoH-xy} object to a kml file that can be opened in Google Earth for animation.
#' The kml format is not optimized for large datasets, but this works reasonably well for moderate datasets (e.g., <1000).
#' To create animations of larger datasets, use a \code{skip} value > 1 or see \code{\link{lxy.exp.mov}}. 
#'
#' Adapated from plotKML package
#'
#' @seealso \code{\link{lxy.exp.mov}}
#' @export

lxy.exp.kml <- function(lxy, file, id=NULL, skip=1, overwrite=TRUE, compress=TRUE, pt.scale=0.5, col=NULL, show.path=TRUE, 
                        path.col=NULL, path.opaque=80, path.lwd=3) {

    ## Attributes / functionality still to add
    # skip
    # shapes (url for each id)
    # tails (either previous val or interpolate points
    # use.Google_gx (read kml_open)
    # create the time stamps offsets by reading the tz attribute of lxy$pts$dt
  
    if (!inherits(lxy, "locoh.lxy")) stop("lxy should be of class \"locoh.lxy\"")

    if (!requireNamespace("XML")) stop("package XML is required for this function")

    file.name <- file
    if (substr(file.name, nchar(file.name)-3, nchar(file.name)) != ".kml") file.name <- paste(file.name, ".kml", sep="") 
    
    if (file.exists(file.name) & overwrite == FALSE) {
      stop(paste("File", file.name, "already exists. Set the overwrite option to TRUE or choose a different name."))
    }

    ## Define some constants used in the meta data header
    #kml_xsd = get("kml_xsd", envir = plotKML.opts)
    #xmlns = get("kml_url", envir = plotKML.opts)
    #xmlns_gx = get("kml_gx", envir = plotKML.opts)

    lxy.inputobj <- deparse(substitute(lxy))
    kml_xsd <- "http://schemas.opengis.net/kml/2.2.0/ogckml22.xsd"
    xmlns <- "http://www.opengis.net/kml/2.2/"
    xmlns_gx <- "http://www.google.com/kml/ext/2.2"

    kml_visibility <- TRUE
    kml_open_main_folder <- TRUE
    kml_open_ind_folder <- TRUE
    
    icon.url <- "http://maps.google.com/mapfiles/kml/pal2/icon18.png"

    if (is.null(id)) {
        id <- rev(levels(lxy[["pts"]][["id"]]))
    } else {
        if (FALSE %in% (id %in% levels(lxy[["pts"]][["id"]]))) stop("id value(s) not found")
    }
    
    ## Get point colors
    if (is.null(col)) {
        col <- rainbow(length(id), end=5/6)
    } else {
        if (length(col) != length(id)) stop("The number of color values in 'col' must equal the number of ids")
    }
    
    ## Convert point colors to KML format
    pt.opaque <- 255       ## 100% opaque
    pt.colrgb.df <- data.frame(as.character((as.hexmode(t((col2rgb(col)))))))
    pt.col.kml <- paste(as.hexmode(pt.opaque), pt.colrgb.df[["blue"]], pt.colrgb.df[["green"]], pt.colrgb.df[["red"]], sep="")
     
    if (show.path) {
        if (is.null(path.col)) {
            path.col <- col
        } else {
            if (length(path.col) != length(id)) stop("The number of color values in 'path.col' must equal the number of ids")
        }

        ## Convert path colors to KML format
        #path.opaque <- 80       ## 50% opaque
        path.colrgb.df <- data.frame(as.character((as.hexmode(t((col2rgb(path.col)))))))
        path.col.kml <- paste(as.hexmode(path.opaque), path.colrgb.df[["blue"]], path.colrgb.df[["green"]], path.colrgb.df[["red"]], sep="")
    }
    
    ## Get the projection parameters of the lxy object    
    lxy.prj_params <- CRSargs(lxy$pts@proj4string)
    
    ## See if it needs to be reprojected to geographic - WGS84 
    kmlCRS <- "+proj=longlat +datum=WGS84"
    if (is.na(lxy.prj_params)) {
        stop(cw(paste("Can not export as KML because the coordinate system is unknown. Use lxy.proj.add() to specify which coordinate system the locations in ", deparse(substitute(lxy)), " are in, and try again.", sep=""), final.cr=FALSE))
    } else {
        if (grepl("+proj=longlat", lxy.prj_params, fixed=TRUE) && grepl("+datum=WGS84", lxy.prj_params, fixed=TRUE)) {
            blnNeedToReproject <- FALSE    
        } else {
            blnNeedToReproject <- TRUE
        }
        
        ## Need to do a more intelligent comparison between lxy projection and kmlCRS
        ## see check_projection() in plotKML package for sample
        ## Break apart lxy.projparams
        #lxy.prjparams_vals <- strsplit(lxy.prj_params, "\\+")[[1]]
        #lxy.prjparams_vals <- lxy.prjparams_vals[lxy.prjparams_vals != ""]

        #target_params <- stringr::str_trim(sapply(strsplit(lxy.prjparams_vals, "="), function(x) {x[2] }))
        #names(target_params) <- sapply(strsplit(value, "="), function(x) { x[1] })        
        # param_names <- sapply(strsplit(lxy.prj_params, "="), function(x) x[1])
        # params <- as.list(paste("\\+", sapply(strsplit(value, "="), function(x) {x[1]})))       
        # p4s <- getCRS(obj)
        # p4s <- CRSargs(prj)
        # params <- parse_proj4(p4s)
        # value <- strsplit(ref_CRS, "\\+")[[1]]
        # value <- value[value != ""]
        # target_params <- stringr::str_trim(sapply(strsplit(value, "="), function(x) {x[2] }))
        # names(target_params) <- sapply(strsplit(value, "="), function(x) { x[1] })
    }

    if (blnNeedToReproject) {
        cat("Reprojecting data to ", kmlCRS, "...", sep=""); flush.console()
        lxy$pts <- sp::spTransform(lxy$pts, CRSobj = CRS(kmlCRS))
        cat("Done.\n")
    }
    
    ## Format the timestamps according to the kml standard
    lxy.dts_str <- format(lxy[["pts"]][["dt"]], "%Y-%m-%dT%H:%M:%SZ")
    
    ## Get the coordinates and put them in a matrix for faster referencing later
    lxy.coords.df <- data.frame(coordinates(lxy[["pts"]]))
    
    kml.out <- newXMLNode("kml", attrs = c(version = "1.0"), namespaceDefinitions = c(xsd = kml_xsd, xmlns = xmlns))
    doc.parent <- newXMLNode("Document", parent=kml.out)
    if (!kml_open_ind_folder) {
        doc.style <- newXMLNode("Style", parent=doc.parent, attrs=list(id="check-hide-children"))
        doc.liststyle <- newXMLNode("ListStyle", parent=doc.style)
        doc.litype <- newXMLNode("listItemType", parent=doc.liststyle, text="checkHideChildren")
    }
        
    ## Create the styles for each id
    
    ## Create a variable for the parent not to make it easier if we need to attach this somewhere else
    ptstyle.parent <- doc.parent

    ## Loop through the ids
    for (i in 1:length(id)) {
        idVal <- id[i]
        ptstyle.node <- newXMLNode("Style", parent=ptstyle.parent, attrs=list(id=paste("pnt-", idVal, sep="")))

        # We're not using labels so leave these out
        # labelstyle <- newXMLNode("LabelStyle", parent=ptstyle.node)
        # labelscalestyle <- newXMLNode("scale", parent=labelstyle, text=as.character(pt.scale))

        iconparent <- newXMLNode("IconStyle", parent=ptstyle.node)
        iconcolor <- newXMLNode("color", parent=iconparent, text=pt.col.kml[i])
        iconscale <- newXMLNode("scale", parent=iconparent, text=as.character(pt.scale))
        
        iconstyle <- newXMLNode("Icon", parent=iconparent)
        hrefstyle <- newXMLNode("href", parent=iconstyle, text=icon.url)
        
        ## We're not using balloon text so leave these out
        ## balloonstyle <- newXMLNode("BalloonStyle", parent=ptstyle.node)
        ## balloonstyletext <- newXMLNode("text", parent=balloonstyle, text="Balloon text")
            
    }
    
    ## Add some more tags for the parent doc
    #lxyfolder.parent <- newXMLNode("Folder", parent=doc.parent)
    lxyfolder.parent <-  doc.parent
    lxyfolder.name <- newXMLNode("name", text=lxy.inputobj, parent=lxyfolder.parent)
    
    ## I think we always want the id folders to be visible
    if (!kml_visibility) lxyfolder.vis <- newXMLNode("visibility", text=as.numeric(kml_visibility), parent=lxyfolder.parent)
    
    lxyfolder.open <- newXMLNode("open", text=as.numeric(kml_open_main_folder), parent=lxyfolder.parent)
    
    ## Finally make the folders with the points
    
    ## Set up progress bar
    total_pts <- sum(as.numeric(sapply(id, function(x) length(seq(from=1, to=sum(lxy[["pts"]][["id"]] == x), by=skip)))))
    
    usePB <- total_pts > 100
    if (usePB) {
        cat("Exporting locations \n")
        pb <- txtProgressBar(min = 0, max=total_pts, style = 3)
        counter <- 0
    }
        
    for (i in 1:length(id)) {
        idVal <- id[i]
        idx <- which(lxy[["pts"]][["id"]] == idVal)
        if (skip > 1) idx <- idx[seq(from=1, to=length(idx), by=skip)]

        ## Create header tags for this folder
        idfolder.parent <- newXMLNode("Folder", parent=lxyfolder.parent)
        idfolder.name <- newXMLNode("name", text=idVal, parent=idfolder.parent)
        if (!kml_open_ind_folder) {
            idfolder.styleurl <- newXMLNode("styleUrl", text="#check-hide-children", parent=idfolder.parent)
        }
        
        ## Next add the placemarks
        for (ptidx in idx) {
            placemark.parent <- newXMLNode("Placemark", parent=idfolder.parent)
            #placemark.name <- newXMLNode("name", text=ptidx, parent=placemark.parent)
            placemark.styleurl <- newXMLNode("styleUrl", text=paste("#pnt-", idVal, sep=""), parent=placemark.parent)
            placemark.timestamp <- newXMLNode("TimeStamp", parent=placemark.parent)
            placemark.when <- newXMLNode("when", text=lxy.dts_str[ptidx], parent=placemark.timestamp)
            placemark.point <- newXMLNode("Point", parent=placemark.parent)
            # Extrude means place a line to the ground, we won't use this now
            #placemark.extrude <- newXMLNode("extrude", text="1", parent=placemark.point)
            placemark.alt <- newXMLNode("altitudeMode", text="clampToGround", parent=placemark.point)
            placemark.coords <- newXMLNode("coordinates", text=paste(lxy.coords.df[ptidx,1], lxy.coords.df[ptidx,2], "0", sep=","), parent=placemark.point)
            if (usePB) {
                counter <- counter + 1
                setTxtProgressBar(pb, counter)
            }
        }
        
        if (show.path) {
            path.parent <- newXMLNode("Placemark", parent=idfolder.parent)
            #path.name <- newXMLNode("name", text="Path", parent=path.parent)
            path.style <- newXMLNode("Style", parent=path.parent)
            path.linestyle <- newXMLNode("LineStyle", parent=path.style)
            path.color <- newXMLNode("color", text=path.col.kml[i], parent=path.linestyle)
            path.width <- newXMLNode("width", text=path.lwd, parent=path.linestyle)            
            path.linestring <- newXMLNode("LineString", parent=path.parent)
            path.tessellate <- newXMLNode("tessellate", text=1, parent=path.linestring)
            path.altitudeMode <- newXMLNode("altitudeMode", text="clampToGround", parent=path.linestring)
            path.coordinates <- newXMLNode("coordinates", text=paste(lxy.coords.df[idx,1], lxy.coords.df[idx,2], sep=",", collapse=" "), parent=path.linestring)
            
        }
        
    }
    if (usePB) close(pb)
  
    ## Close file and write to disk
    saveXML(kml.out, file.name)
    file.created <- file.name

    ## Compress file
    if (compress) {
        fn.kmz <- sub(".kml", ".kmz", file.name)
        try(x <- zip(zipfile = paste(getwd(), fn.kmz, sep = "/"), files = paste(getwd(), file.name, sep = .Platform$file.sep)))
        if (is(.Last.value, "try-error") | x == 127) {
            warning("Could not compress kml file.")
        }
        
        if (file.exists(fn.kmz)) {
            file.remove(file.name)
            file.created <- fn.kmz   
        }
    }
      
    cat(file.created, " created \n", sep="")
    return(invisible(file.created))
    
}