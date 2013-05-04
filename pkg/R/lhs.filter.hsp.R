#' Define subsets of hulls based on the location of the parent point in hull scatterplot space
#'
#' @param lhs A LoCoH-hullset object
#' @param id The name(s) of the individual(s) to include in the plot. Character vector or comma-delimited character.
#' @param k The k value of hullsets to create isopleths for
#' @param r The r value of hullsets to create isopleths for
#' @param a The a value of hullsets to create isopleths for
#' @param s The s value of hullsets to create isopleths for
#' @param hs.names The name(s) of saved hullsets to create isopleths for
#' @param hsp Either the index of a hull scatterplot(s) saved in lhs (use \code{\link{summary.locoh.lhs}} to which how many hsp 
#' objects have been saved), or a list of objects of class \code{locoh.hsp} (i.e., the return value of \code{\link{lhs.plot.scatter}}. 
#' @param reg.idx A numeric vector of the indices of the regions in \code{hsp} to include in the filter
#' @param label Character vector of the labels to use for each subset. If not passed, the saved label values in \code{hsp} will be used
#' @param col Vector of color values (one per region). If omitted the colors saved in \code{hsp} will be used
#'
#' @details 
#' This will return a list that defines subsets of hulls grouped according to which manually-digitized region in scatterplot 
#' space the hull parent point falls. This can be passed to several functions, including \code{\link{plot.locoh.lhs}} (future), 
#' and \code{\link{lhs.plot.scatter}}, to create plots of subsets of hulls.
#'
#' The scatterplot space, including the manually defined regions, are saved in a \code{locoh.hsp} object. Typically \code{hsp} will be created
#' using the \code{\link{lhs.plot.scatter}} function. \code{hsp} objects can also be saved in the \code{lhs} object (see \code{\link{lhs.hsp.add}}.
#'
#' Note that this function can only return subsets for *one* hullset and *one* hsp object. This means that either \code{lhs} must contain a single
#' hullset, or other parameters (e.g., \code{id}, \code{hs.names}, \code{k}, \code{r}, and/or \code{a}) are passed to select one and only one of the hullsets in \code{lhs}.
#'
#' @return A list that defines subsets of hulls, with one element for each region in \code{hsp}. Each element is a list with three elements:
#' \describe{
#'    \item{idx}{indices of the hull parent points}
#'    \item{label}{the label of the region of hsp}
#'    \item{col}{the color of the region in hsp}
#' }
#' 
#' @export

lhs.filter.hsp <- function(lhs, id=NULL, k=NULL, r=NULL, a=NULL, s=NULL, hs.names = NULL, 
                               hsp=NULL, reg.idx=NULL, label=NULL, col=NULL) {
  
    ## hsp can be either the index of a hsp saved in lhs, or A LIST OF objects of class locoh.hsp. 
    ## ONLY ONE HSP CAN BE PASSED
    ## A filter list element will be returned for each region in hsp. 
    ## 
    ## col is an optional vector of color values, if not passed the stored color values in the regions will be used

    if (!inherits(lhs, "locoh.lhs")) stop("lhs should be of class \"locoh.lhs\"")
    if (!require(sp)) stop("package sp required")
    err.msg.hsp <- "hsp should be the index of a hsp object saved in lhs, or a list with one object of class 'locoh.hsp'"
    if (length(hsp) != 1) stop(err.msg.hsp)

    if (is.null(id) && is.null(r) && is.null(k) && is.null(a) && is.null(s) && is.null(hs.names)) {
        hs <- lhs
    } else {    
        hs <- lhs.select(lhs, id=id, r=r, k=k, a=a, s=s, hs.names=hs.names)
    }
    if (length(hs)==0) stop("No hullsets found matching those criteria")
    if (length(hs)>1) stop("Multiple hull sets returned. Please use criteria that return only one set of hulls")
    
    if (is.numeric(hsp)) {
        hsp <- hs[[1]][["hsp"]][hsp]
    } else {
        if (!is.list(hsp)) stop(err.msg.hsp)
        if (class(hsp[[1]]) != "locoh.hsp") stop(err.msg.hsp)
    }
    

    ## Extract variables saved in hsp
    regions <- hsp[[1]][["regions"]]
    if (length(regions)==0) stop("no regions found in hsp")
    

    ## Create the standard hmap data frame from the saved values in hsp.
    ## Presumably this data frame this will just have one row, so we 
    ## don't have to loop through it, just set hmap.idx = 1
    if (length(hsp[[1]][["hmap"]]) > 0) {
        ## Convert the list of auxillary variables into a data frame containing all permutations of the variables
        hmap <- expand.grid(hsp[[1]][["hmap"]], stringsAsFactors=FALSE)
    } else {
        hmap <- as.data.frame(NA)
    }

    ## The saved hull metric expressions expect to have variables 'hs.name' and 'hmap.idx' in memory
    hs.name <- hsp[[1]][["hs.name"]]
    hmap.idx <- 1

    ## Set reg.idx
    if (is.null(reg.idx)) reg.idx <- 1:length(regions)
    
    ## Define colors
    if (is.null(col)) {
        reg.cols <- as.character(sapply(regions, function(x) x[["col"]]))
    } else {
        if (length(col) != length(reg.idx)) {
            stop("The number of colors must match the number of regions")
        } else {
            reg.cols <- rep(NA, length(regions))
            reg.cols[reg.idx] <- col
        }
    }

    ## Define labels
    if (is.null(label)) {
        reg.label <- as.character(sapply(regions, function(x) x[["label"]]))
    } else {
        if (length(label) != length(reg.idx)) {
            stop("The number of labels must match the number of regions")
        } else {
            reg.label <- rep(NA, length(regions))
            reg.label[reg.idx] <- label
        }
    }
    
    ## Get xvals
    hme <- hm.expr(names.only=FALSE)
    xvals <- eval(hme[[hsp[[1]][["x.axis"]]]][["expr"]])
    yvals <- eval(hme[[hsp[[1]][["y.axis"]]]][["expr"]])
    if (length(xvals)==0 || length(yvals)==0) stop("Can't find values for saved hull scatter plot")
    
    ## Transform the x and y values if needed
    if (!is.null(hsp[[1]][["trans.x"]])) xvals <- get(hsp[[1]][["trans.x"]])(xvals)
    if (!is.null(hsp[[1]][["trans.y"]])) yvals <- get(hsp[[1]][["trans.y"]])(yvals)
    
    res <- list()
    for (i in 1:length(reg.idx)) {
        poly.pts <- regions[[reg.idx[i]]][["poly.pts"]]
        
        ## short.list used to be an argument, but since it seems to be faster we have made it standard
        short.list <- TRUE
        if (short.list) {
            # My tests have shown that using the short list is a lot faster
            poly.pts.x.range <- range(poly.pts[,1])
            poly.pts.y.range <- range(poly.pts[,2])
            pts.idx.in.this.poly.short.list <- which(xvals >= poly.pts.x.range[1] & xvals <= poly.pts.x.range[2] & yvals >= poly.pts.y.range[1] & yvals <= poly.pts.y.range[2])
            pts.idx.in.this.poly <- pts.idx.in.this.poly.short.list[sapply(pts.idx.in.this.poly.short.list, function(m) point.in.polygon(point.x=xvals[m], point.y=yvals[m], pol.x=poly.pts[,1], pol.y=poly.pts[,2]))!=0]
        } else {
            pts.idx.in.this.poly <- which(sapply(1:length(xvals), function(i) point.in.polygon(point.x=xvals[i], point.y=yvals[i], pol.x=poly.pts[,1], pol.y=poly.pts[,2])!=0))
        }
        res[[i]] <- list(label=reg.label[reg.idx[i]], idx=pts.idx.in.this.poly, col=reg.cols[reg.idx[i]])
    }
    return(res)
}
