#' Define subsets of hulls based on the location of the parent point in hull scatterplot space
#'
#' @param lhs A LoCoH-hullset object
#' @param id The name(s) of the individual(s) to include in the plot. Character vector or comma-delimited character.
#' @param k The k value of hullsets to create isopleths for
#' @param r The r value of hullsets to create isopleths for
#' @param a The a value of hullsets to create isopleths for
#' @param s The s value of hullsets to create isopleths for
#' @param hs.names The name(s) of saved hullsets to create isopleths for
#' objects have been saved), or a list of objects of class \code{locoh.hsp} (i.e., the return value of \code{\link{lhs.plot.scatter}}. 
#'
#' @details 
#' This will return a list that defines subsets of hulls grouped according to which manually-digitized region in scatterplot 
#' space the hull parent point falls. This can be passed to several functions, including \code{\link{plot.locoh.lhs}} (future), 
#' and \code{\link{lhs.plot.scatter}}, to create plots of subsets of hulls.
#'
#' Note that this function can only return subsets for *one* hullset. This means that either \code{lhs} must contain a single
#' hullset, or other parameters (e.g., \code{id}, \code{hs.names}, \code{k}, \code{r}, and/or \code{a}) are passed to select 
#' one and only one of the hullsets in \code{lhs}.
#'
#' @return A list that defines subsets of hulls, with one element for each region in \code{hsp}. Each element is a list with three elements:
#' \describe{
#'    \item{idx}{indices of the hull parent points}
#'    \item{label}{the label for the subset}
#'    \item{col}{the color }
#' }
#'
#'
#' @export

lhs.filter.anv <- function(lhs, id=NULL, k=NULL, r=NULL, a=NULL, s=NULL, hs.names = NULL, 
                            label=NULL, col=NULL, anv.var=NULL, anv.val=NULL, status=TRUE) {
  
    ## A filter list element will be returned for each region in hsp. 
    ## label is an optional character vector for the labels for each subset. If not passed, the label values in regions will be used
    ## col is an optional vector of color values, if not passed the stored color values in the regions will be used

    if (!inherits(lhs, "locoh.lhs")) stop("lhs should be of class \"locoh.lhs\"")
    if (!require(sp)) stop("package sp required")

    if (is.null(id) && is.null(r) && is.null(k) && is.null(a) && is.null(s) && is.null(hs.names)) {
        hs <- lhs
    } else {    
        hs <- lhs.select(lhs, id=id, r=r, k=k, a=a, s=s, hs.names=hs.names)
    }
    if (length(hs)==0) stop("No hullsets found matching those criteria")
    if (length(hs)>1) stop("Multiple hull sets returned. Please use criteria that return only one set of hulls")
    
    if (is.null(anv.var)) stop("anv.var is a required parameter")
    if (length(anv.var) != 1) stop("anv.var must be of length 1")
    if (!anv.var %in% names(hs[[1]][["hulls"]]@data)) stop(paste(anv.var, " not found", sep=""))
    
    anv.val.all <- unique(hs[[1]][["hulls"]]@data[[anv.var]])
    if (is.null(anv.val)) {
        anv.val.use <- anv.val.all 
    } else {
        anv.val.use <- intersect(anv.val, anv.val.all)
    }
    if (length(anv.val.use)==0) stop("No matching values found")
    
    ## Define colors
    if (is.null(col)) {
        grp.cols <- rainbow(length(anv.val.use), end=5/6)
    } else {
        if (length(col) != length(anv.val.use)) {
            stop("The number of colors must match the number of unique values")
        } else {
            grp.cols <- col
        }
    }

    ## Define labels
    if (is.null(label)) {
        grp.label <- as.character(anv.val.use)
    } else {
        if (length(label) != length(anv.val.use)) {
            stop("The number of labels must match the number of unique values")
        } else {
            grp.label <- label
        }
    }
    
    res <- vector("list", length(anv.val.use))
    names(res) <- grp.label
    for (i in 1:length(anv.val.use)) {
        hulls.idx <- which(hs[[1]][["hulls"]]@data[[anv.var]] == anv.val.use[i])
        res[[i]] <- list(label=grp.label[i], idx=hulls.idx, col=grp.cols[i])
    }
    
    if (status) {
        out.df <- do.call(rbind, lapply(names(res), function(x) data.frame(val=x, count=length(res[[x]][["idx"]])))) 
        print(formatdf4print(out.df, indent=3), row.names=FALSE)
    }
    
    return(res)
}
