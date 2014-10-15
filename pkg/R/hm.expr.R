#' Hull metrics expressions
#'
#' Returns a list of expression objects that when evaluated in functions will return various hull metrics
#'
#' @param names.only Return only the names of hull metrics (not the expressions themselves). T/F.
#' @param desc Include descriptions of the hull metrics in the object returned. T/F.
#' @param print Display the list of hull metrics on the console. T/F.
#' 
#' @details Hull metrics are used by many functions. This function returns a list of the expressions to 'pull out' hull metric values from 
#' the data structure of a \code{\link{LoCoH-hullset}} object and other properties of hull metrics. These objects are used
#' by other T-LoCoH functions for plotting, exporting, creating isopleths, and other things you can do with hull metrics.
#'
#' Most users will not need to use this function other than to see a list of hull metric names (which you need to know to specify a 
#' hull metric in a plotting function for example) and the hull metric descriptions.
#' 
#' This function does not compute any hull metrics, only provide a comprehensive list of all possible metrics. 
#' Some hull metrics are computed by default when you create a hullset (see \code{\link{lxy.lhs}}).
#' Others metrics must be computed with a special function (e.g., \code{\link{lhs.ellipses.add}}. To see which hull metrics exist for a specific LoCoH-hullset, 
#' use \code{\link{summary.locoh.lhs}}.
#'
#' @return A named list with the following elements:
#' \enumerate{
#' \item ufat. User-friendly axis title
#' \item desc. Description (used to build plot captions)
#' \item def. Definition. If NULL, then the definition is the same as $desc
#' \item ufipt. User-friendly isopleth plot title. Used when plotting isopleths constructed by sorting hulls on the metric.
#' \item nhv. No hull value (the value to use for parent points that have no hulls, used to highlight the presence of points without hulls in histograms
#' \item iso.dec. Sort hulls in descending order when used to create isopleths). T/F
#' \item expr Expression object that when evaulated returns the hull metric values
#' \item zero2na Whether 0 values should be colored as NA (i.e., hidden) when classifying hull parent points, T/F
#' \item discrete Whether the metric assumes discrete (integer) values, used in classifying hull parent points T/F
#' \item auto.jiggle The maximum value of a uniform distribution used to apply a random offset to the metric when plotted on scatterplots to better see point density
#' \item spao.x include the metric as a default for the x-axis when the 'auto' option is used in \code{\link{lhs.plot.scatter.auto}}
#' \item spao.y include the metric as a default for the y-axis when the 'auto' option is used in \code{\link{lhs.plot.scatter.auto}}
#' \item req.metrics The underlying metric(s) that need to be saved in the hullset for successful computation. Character vector. This will usually simply be the name of the metric itself, but may be different in the case of derived metrics such as perimeter-area ratio.
#' \item req.ap The name(s) of required auxillary variables that are needed to extract the metric. Character vector.
#' \item req.ap.def A named list of default values of any required auxillary variables 
#' \item req.ap.subtitle A named list of expression objects (one for each hmap) that returns a string of hmap values that will be used in the subtitle of plots
#' \item req.ap.desc An named list of expression objects (one for each hmap) that returns a string of hmap values that will be used in the description of plots
#' \item fun The name of the function that creates the hull metric
#' \item iso.hm2 Second hull metric used to break ties when creating isopleths (used mostly when the metric returns discrete values)
#' \item iso.hm2.dec Sort second hull metric descending
#' }
#' 
#' @export
#' @seealso \code{\link{lhs.ellipses.add}}, \code{\link{lhs.visit.add}}

hm.expr <- function(names.only=TRUE, desc=names.only, print=names.only) {

    DI <- "Density Isopleths"
    VD <- "Visitation Distribution"
    RD <- "Revisitation Distribution"
    PEPD <- "Proportion of Enclosed Points Distribution"
    DU <- "Duration of Use Distribution"
    ED <- "Elongation Distribution"
    VEL_DIST <- "Velocity Distribution"
    
    ivg.desc <- list(ivg=expression(paste("Separate visits defined by an inter-visit gap period >= ", hmap[hmap.idx, "ivg"], "(", secs.fmt(hmap[hmap.idx, "ivg"]), "). ", sep="")))
    ivg.subtitle <- list(ivg=expression(paste("ivg=", hmap[hmap.idx, "ivg"], "(", secs.fmt(hmap[hmap.idx, "ivg"]), ")", sep="")))
    pep.subtitle <- list(pep.var=NULL, pep.val=expression(paste(hmap[hmap.idx, "pep.var"], "=", hmap[hmap.idx, "pep.val"], sep="")))
    pep.desc <- list(pep.var=NULL, pep.val=expression(paste(hmap[hmap.idx, "pep.var"], " = ", hmap[hmap.idx, "pep.val"], ". ", sep="")))
    #anv.subtitle <- list(anv=expression(paste("anv=", hmap[hmap.idx, "anv"], sep="")))
    anv.subtitle <- list(anv=expression("ancillary value of hull parent point"))
    anv.desc <- list(anv=expression(paste("anv=", hmap[hmap.idx, "anv"], ". ", sep="")))

    ta.desc <- list(ta.min=expression(paste("Revisits defined by a time-away interval of ", hmap[hmap.idx, "ta.min"], "s (", secs.fmt(hmap[hmap.idx, "ta.min"]), ")", if (hmap[hmap.idx, "ta.max"]>0) paste(" to ", hmap[hmap.idx, "ta.max"], "s (", secs.fmt(hmap[hmap.idx, "ta.max"]), ")", sep="") else "", ". ", sep="")), ta.max=expression(NULL))

    #ta.subtitle <- list(ta.min=expression(paste("ta.min=", hmap[hmap.idx, "ta.min"], "(", secs.fmt(hmap[hmap.idx, "ta.min"]), ")", sep="")), ta.max=expression(paste("ta.max=", hmap[hmap.idx, "ta.max"], "(", secs.fmt(hmap[hmap.idx, "ta.max"]), ")", sep="")))
                                               
    ta.subtitle <- list(ta.min=expression(paste(secs.fmt(hmap[hmap.idx, "ta.min"]), " - ", secs.fmt(hmap[hmap.idx, "ta.max"]), sep="")), ta.max=expression(NULL))
    so.subtitle <- list(hs2=expression(paste("Association with ", hmap[hmap.idx, "hs2"], sep="")))
                                                                                                                                 
    expr.lst <- list()

    ## Date and hour metrics
    expr.lst[["dt"]] <- list(ufat="date", desc="date of the parent point", def=NULL, ufipt="Distribution by Date", nhv=NA, iso.dec=FALSE, expr=expression(hs[[hs.name]][["pts"]][["dt"]][hs[[hs.name]][["hulls"]][["pts.idx"]]]), zero2na=FALSE, discrete=FALSE, auto.jiggle=0, spao.x=TRUE, spao.y=FALSE, req.metrics=NULL, req.ap=NULL, req.ap.def=NULL, req.ap.subtitle=NULL, req.ap.desc=NULL, fun=NULL, iso.hm2=NULL, iso.hm2.dec=FALSE)

    expr.lst[["dt.month"]] <- list(ufat="month", desc="month (1-12) of the parent point", def="month of the parent point", ufipt="Distribution by Month", nhv=NA, iso.dec=FALSE, expr=expression(as.POSIXlt(hs[[hs.name]][["pts"]][["dt"]][hs[[hs.name]][["hulls"]][["pts.idx"]]])$mon + 1), zero2na=FALSE, discrete=TRUE, auto.jiggle=0.1, spao.x=TRUE, spao.y=FALSE, req.metrics=NULL, req.ap=NULL, req.ap.def=NULL, req.ap.subtitle=NULL, req.ap.desc=NULL, fun=NULL, iso.hm2=NULL, iso.hm2.dec=FALSE)

    expr.lst[["dt.hour"]] <- list(ufat="hour of day", desc="hour of the day (0-24) of the parent point", def="hour of the day of the parent point", ufipt="Distribution by Hour of Day", nhv=NA, iso.dec=FALSE, expr=expression(as.POSIXlt(hs[[hs.name]][["pts"]][["dt"]][hs[[hs.name]][["hulls"]][["pts.idx"]]])$hour), zero2na=FALSE, discrete=TRUE, auto.jiggle=0.05, spao.x=TRUE, spao.y=FALSE, req.metrics=NULL, req.ap=NULL, req.ap.def=NULL, req.ap.subtitle=NULL, req.ap.desc=NULL, fun=NULL, iso.hm2=NULL, iso.hm2.dec=FALSE)

    expr.lst[["tspan"]] <- list(ufat="time span of nearest neighbors (multiples of tau)", desc="time span of nearest neighbors as a multiple of the median sampling interval", def="time span of nearest neighbors", ufipt="Time Span of Nearest Neighbors", nhv=NA, iso.dec=TRUE, expr=expression(hs[[hs.name]][["hulls"]][["tspan"]]), zero2na=FALSE, discrete=FALSE, auto.jiggle=0.05, spao.x=TRUE, spao.y=FALSE, req.metrics="tspan", req.ap=NULL, req.ap.def=NULL, req.ap.subtitle=NULL, req.ap.desc=NULL, fun=NULL, iso.hm2="area", iso.hm2.dec=FALSE)

    ## Core metrics saved in lhs object
    expr.lst[["area"]] <- list(ufat=expression("area"), desc="area", def=NULL, ufipt=DI, nhv=NA, iso.dec=FALSE, expr=expression(hs[[hs.name]][["hulls"]][["area"]]), zero2na=FALSE, discrete=FALSE, auto.jiggle=0, spao.x=FALSE, spao.y=TRUE, req.metrics="area", req.ap=NULL, req.ap.def=NULL, req.ap.subtitle=NULL, req.ap.desc=NULL, fun=NULL, iso.hm2=NULL, iso.hm2.dec=FALSE)

    expr.lst[["nnn"]] <- list(ufat="num nearest neighbors", desc="number of nearest neighbors", def=NULL, ufipt=DI, nhv=-1, iso.dec=TRUE, expr=expression(hs[[hs.name]][["hulls"]][["nnn"]]), zero2na=FALSE, discrete=TRUE, auto.jiggle=0.15, spao.x=FALSE, spao.y=FALSE, req.metrics="nnn", req.ap=NULL, req.ap.def=NULL, req.ap.subtitle=NULL, req.ap.desc=NULL, fun=NULL, iso.hm2="area", iso.hm2.dec=FALSE)

    expr.lst[["nep"]] <- list(ufat="num enclosed points", desc="number of enclosed points", def=NULL, ufipt=DI, nhv=-1, iso.dec=TRUE, expr=expression(hs[[hs.name]][["hulls"]][["nep"]]), zero2na=FALSE, discrete=TRUE, auto.jiggle=0.15, spao.x=TRUE, spao.y=TRUE, req.metrics="nep", req.ap=NULL, req.ap.def=NULL, req.ap.subtitle=NULL, req.ap.desc=NULL, fun=NULL, iso.hm2="area", iso.hm2.dec=FALSE)

    ## Elongation metrics
    expr.lst[["ecc"]] <- list(ufat="elongation (ecc)", desc="eccentricity of the bounding ellipse (higher ecc means more elongated)", def="eccentricity of the bounding ellipse", ufipt=ED, nhv=NA, iso.dec=TRUE, expr=expression(hs[[hs.name]][["hulls"]][["ecc"]]), zero2na=FALSE, discrete=FALSE, auto.jiggle=0, spao.x=TRUE, spao.y=TRUE, req.metrics="ecc", req.ap=NULL, req.ap.def=NULL, req.ap.subtitle=NULL, req.ap.desc=NULL, fun="lhs.ellipses.add", iso.hm2=NULL, iso.hm2.dec=FALSE)

    expr.lst[["par"]] <- list(ufat="elongation (par)", desc="elongation (perimeter:area ratio)", def="perimeter:area ratio", ufipt=ED, nhv=NA, iso.dec=TRUE, expr=expression(hs[[hs.name]][["hulls"]][["perim"]]/hs[[hs.name]][["hulls"]][["area"]]), zero2na=FALSE, discrete=FALSE, auto.jiggle=0, spao.x=TRUE, spao.y=TRUE, req.metrics=c("perim","area"), req.ap=NULL, req.ap.def=NULL, req.ap.subtitle=NULL, req.ap.desc=NULL, fun=NULL, iso.hm2=NULL, iso.hm2.dec=FALSE)

    expr.lst[["perim"]] <- list(ufat="perimeter", desc="perimeter", def=NULL, ufipt="Perimeter Isopleths", nhv=NA, iso.dec=TRUE, expr=expression(hs[[hs.name]][["hulls"]][["perim"]]), zero2na=FALSE, discrete=FALSE, auto.jiggle=0, spao.x=FALSE, spao.y=FALSE, req.metrics="perim", req.ap=NULL, req.ap.def=NULL, req.ap.subtitle=NULL, req.ap.desc=NULL, fun=NULL, iso.hm2=NULL, iso.hm2.dec=FALSE)

    ## Inter-visit gap based metrics (requires hull metric auxillary parameter ivg)
    expr.lst[["nsv"]] <- list(ufat="visitation rate (nsv)", desc="visitation rate (number of separate visits)", def="visitation rate", ufipt=VD, nhv=-1, iso.dec=TRUE, expr=expression(hs[[hs.name]][["hulls"]][[paste("nsv.", hmap[hmap.idx, "ivg"], sep="")]]), zero2na=FALSE, discrete=TRUE, auto.jiggle=0.1, spao.x=TRUE, spao.y=TRUE, req.metrics="nsv", req.ap="ivg", req.ap.def=list(ivg="all"), req.ap.subtitle=ivg.subtitle, req.ap.desc=ivg.desc, fun="lhs.visit.add", iso.hm2="area", iso.hm2.dec=FALSE)
    
    expr.lst[["nnsv"]] <- list(ufat="num visits normalized by area (nnsv)", desc="normalized visitation rate (number of separate visits normalized by area)", def="normalized visitation rate", ufipt=VD, nhv=-1, iso.dec=TRUE, expr=expression(hs[[hs.name]][["hulls"]][[paste("nsv.", hmap[hmap.idx, "ivg"], sep="")]] / hs[[hs.name]][["hulls"]][["area"]]), zero2na=FALSE, discrete=FALSE, auto.jiggle=0, spao.x=FALSE, spao.y=FALSE, req.metrics=c("nsv","area"), req.ap="ivg", req.ap.def=list(ivg="all"), req.ap.subtitle=ivg.subtitle, req.ap.desc=ivg.desc, fun="lhs.visit.add", iso.hm2=NULL, iso.hm2.dec=FALSE)
    
    expr.lst[["mnlv"]] <- list(ufat="duration of visit (mnlv)", desc="duration of visit (mean number of locations in the hull per visit)", def="duration of visit", ufipt=DU, nhv=-1, iso.dec=TRUE, expr=expression(hs[[hs.name]][["hulls"]][[paste("mnlv.", hmap[hmap.idx, "ivg"], sep="")]]), zero2na=FALSE, discrete=FALSE, auto.jiggle=0.05, spao.x=TRUE, spao.y=TRUE, req.metrics="mnlv", req.ap="ivg", req.ap.def=list(ivg="all"), req.ap.subtitle=ivg.subtitle, req.ap.desc=ivg.desc, fun="lhs.visit.add", iso.hm2=NULL,iso.hm2.dec=FALSE)

    expr.lst[["nmnlv"]] <- list(ufat="duration of visit normalized by area (nmnlv)", desc="normalized duration of visit (mean number of locations in the hull per visit normalized by area)", def="normalized duration of visit", ufipt=DU, nhv=-1, iso.dec=TRUE, expr=expression(hs[[hs.name]][["hulls"]][[paste("mnlv.", hmap[hmap.idx, "ivg"], sep="")]] / hs[[hs.name]][["hulls"]][["area"]]), zero2na=FALSE, discrete=FALSE, auto.jiggle=0, spao.x=FALSE, spao.y=FALSE, req.metrics=c("mnlv","area"), req.ap="ivg", req.ap.def=list(ivg="all"), req.ap.subtitle=ivg.subtitle, req.ap.desc=ivg.desc, fun="lhs.visit.add", iso.hm2=NULL, iso.hm2.dec=FALSE)

    expr.lst[["nsr"]] <- list(ufat="revisitation rate (nsr)", desc="revisitation rate (number of revisits)", def="revisitation rate", ufipt=RD, nhv=-1, iso.dec=TRUE, expr=expression(hs[[hs.name]][["hulls"]][[paste("nsr.", hmap[hmap.idx, "ta.min"], ".", hmap[hmap.idx, "ta.max"], sep="")]]), zero2na=TRUE, discrete=TRUE, auto.jiggle=0.1, spao.x=TRUE, spao.y=TRUE, req.metrics="nsr", req.ap=c("ta.min","ta.max"), req.ap.def=list(ta.min="all", ta.max="all"), req.ap.subtitle=ta.subtitle, req.ap.desc=ta.desc, fun="lhs.revisit.add", iso.hm2="area", iso.hm2.dec=FALSE)

    ## Proportion of enclosed points of an ancillary variable based metrics (requires hull metric auxillary parameter pep.var and pep.val)
    expr.lst[["pep"]] <- list(ufat="proportion of enclosed points (pep)", desc="proportion of enclosed points", def=NULL, ufipt=PEPD, nhv=NA, iso.dec=TRUE, expr=expression(hs[[hs.name]][["hulls"]][[paste("pep.", hmap[hmap.idx, "pep.var"], ".", hmap[hmap.idx, "pep.val"], sep="")]]), zero2na=TRUE, discrete=FALSE, auto.jiggle=0, spao.x=FALSE, spao.y=TRUE, req.metrics="pep", req.ap=c("pep.var", "pep.val"), req.ap.def=list(pep.var="all", pep.val="all"), req.ap.subtitle=pep.subtitle, req.ap.desc=pep.desc, fun="lhs.pep.add", iso.hm2=NULL,iso.hm2.dec=FALSE)
                                                             
    ## Proportion of enclosed points of an ancillary variable based metrics (requires hull metric auxillary parameter pep.var and pep.val)
    expr.lst[["pep.mcol"]] <- list(ufat="proportion of enclosed points (pep)", desc="proportion of enclosed points", def=NULL, ufipt=PEPD, nhv=NA, iso.dec=TRUE, expr=expression(hs[[hs.name]][["hulls"]]@data[names(hs[[hs.name]][["hm"]])[sapply(hs[[hs.name]][["hm"]], function(x) x$type=="pep")]]), zero2na=TRUE, discrete=FALSE, auto.jiggle=0, spao.x=FALSE, spao.y=TRUE, req.metrics="pep", req.ap=NULL, req.ap.def=NULL, req.ap.subtitle=pep.subtitle, req.ap.desc=pep.desc, fun="lhs.pep.add", iso.hm2=NULL,iso.hm2.dec=FALSE)
    
    #hs[[hs.name]][["hulls"]][[paste("pep.", hmap[hmap.idx, "pep.var"], ".", hmap[hmap.idx, "pep.val"], sep="")]]
    

    expr.lst[["npep"]] <- list(ufat="proportion of enclosed points normalized by area (npep)", desc="proportion of enclosed points normalized by area", def=NULL, ufipt=PEPD, nhv=NA, iso.dec=TRUE, expr=expression(hs[[hs.name]][["hulls"]][[paste("npep.", hmap[hmap.idx, "pep.var"], ".", hmap[hmap.idx, "pep.val"], sep="")]]), zero2na=TRUE, discrete=FALSE, auto.jiggle=0, spao.x=FALSE, spao.y=TRUE, req.metrics="pep", req.ap=c("pep.var", "pep.val"), req.ap.def=list(pep.var="all", pep.val="all"), req.ap.subtitle=pep.subtitle, req.ap.desc=pep.desc, fun="lhs.pep.add", iso.hm2=NULL,iso.hm2.dec=FALSE)

    ## Ancillary variables
    expr.lst[["anv"]] <- list(ufat=expression(hmap[hmap.idx, "anv"]), desc=expression(paste(hmap[hmap.idx, "anv"], " value of the parent point", sep="")), def="ancillary variable", ufipt=expression(paste("parent point value of ", hmap[hmap.idx, "anv"], sep="")), nhv=NA, iso.dec=FALSE, expr=expression(hs[[hs.name]][["pts"]][[as.character(hmap[hmap.idx, "anv"])]][hs[[hs.name]][["hulls"]][["pts.idx"]]]), zero2na=FALSE, discrete=FALSE, auto.jiggle=0, spao.x=FALSE, spao.y=FALSE, req.metrics=NULL, req.ap="anv", req.ap.def=list(anv="all"), req.ap.subtitle=anv.subtitle, req.ap.desc=anv.desc, fun=NULL, iso.hm2=NULL, iso.hm2.dec=FALSE)

    ## Velocity metrics
    expr.lst[["scg.enc.mean"]] <- list(ufat="enc pts avg speed (scg) ", desc="average 'coming and going' speed (t-1 to t+1) of each point enclosed by the hull (map units/sec)", def="avg speed of enclosed points", ufipt=VEL_DIST, nhv=-1, iso.dec=FALSE, expr=expression(hs[[hs.name]][["hulls"]][["scg.enc.mean"]]), zero2na=FALSE, discrete=FALSE, auto.jiggle=0, spao.x=FALSE, spao.y=TRUE, req.metrics="scg.enc.mean", req.ap=NULL, req.ap.def=NULL, req.ap.subtitle=NULL, req.ap.desc=NULL, fun=NULL, iso.hm2=NULL, iso.hm2.dec=FALSE)
    
    expr.lst[["scg.nn.mean"]] <- list(ufat="nn avg speed (scg)", desc="average 'coming and going' speed (t-1 to t+1) of the nearest neighbors used in hull construction (map units/sec)", def="avg speed of nearest neighbors", ufipt=VEL_DIST, nhv=-1, iso.dec=FALSE, expr=expression(hs[[hs.name]][["hulls"]][["scg.nn.mean"]]), zero2na=FALSE, discrete=FALSE, auto.jiggle=0, spao.x=FALSE, spao.y=FALSE, req.metrics="scg.nn.mean", req.ap=NULL, req.ap.def=NULL, req.ap.subtitle=NULL, req.ap.desc=NULL, fun=NULL, iso.hm2=NULL, iso.hm2.dec=FALSE)
    
    expr.lst[["scg.enc.sd"]] <- list(ufat="st.dev of enc pts speed", desc="standard deviation of the 'coming and going' speed (t-1 to t+1) of the points enclosed by the hull (map units/sec)", def="stdev of the speed of enclosed points", ufipt=VEL_DIST, nhv=-1, iso.dec=FALSE, expr=expression(hs[[hs.name]][["hulls"]][["scg.enc.sd"]]), zero2na=FALSE, discrete=FALSE, auto.jiggle=0, spao.x=FALSE, spao.y=FALSE, req.metrics="scg.enc.sd", req.ap=NULL, req.ap.def=NULL, req.ap.subtitle=NULL, req.ap.desc=NULL, fun=NULL, iso.hm2=NULL, iso.hm2.dec=FALSE)
    
    expr.lst[["scg.nn.sd"]] <- list(ufat="st.dev of nn pts speed", desc="standard deviation of the 'coming and going' speed (t-1 to t+1) of the nearest neighbors used in hull construction (map units/sec)", def="stdev of the speed of nearest neighbors", ufipt=VEL_DIST, nhv=-1, iso.dec=FALSE, expr=expression(hs[[hs.name]][["hulls"]][["scg.nn.sd"]]), zero2na=FALSE, discrete=FALSE, auto.jiggle=0, spao.x=FALSE, spao.y=FALSE, req.metrics="scg.nn.sd", req.ap=NULL, req.ap.def=NULL, req.ap.subtitle=NULL, req.ap.desc=NULL, fun=NULL, iso.hm2=NULL, iso.hm2.dec=FALSE)

    ## Spatial overlap metrics
    expr.lst[["so.count"]] <- list(ufat="num overlapping hulls", desc="number of overlapping hulls", def=NULL, ufipt="number overlapping hulls", nhv=NA, iso.dec=TRUE, expr=expression(hs[[hs.name]][["hulls"]][[paste("so.count.", hmap[hmap.idx, "hs2"], sep="")]]), zero2na=FALSE, discrete=TRUE, auto.jiggle=0.01, spao.x=TRUE, spao.y=FALSE, req.metrics="so.count", req.ap="hs2", req.ap.def=list(hs2="all"), req.ap.subtitle=so.subtitle, req.ap.desc="My Desc", fun="lhs.so.add", iso.hm2="area", iso.hm2.dec=FALSE)
    
    expr.lst[["so.dtmin"]] <- list(ufat="min time diff of overlapping hulls", desc="min time diff of overlapping hulls", def=NULL, ufipt="min time diff of overlapping hulls", nhv=NA, iso.dec=TRUE, expr=expression(hs[[hs.name]][["hulls"]][[paste("so.dtmin.", hmap[hmap.idx, "hs2"], sep="")]]), zero2na=FALSE, discrete=FALSE, auto.jiggle=0.01, spao.x=TRUE, spao.y=FALSE, req.metrics="so.dtmin", req.ap="hs2", req.ap.def=list(hs2="all"), req.ap.subtitle="My Subtitle", req.ap.desc="My Desc", fun="lhs.so.add", iso.hm2="area", iso.hm2.dec=FALSE)

    ## Temporal overlap metrics
    expr.lst[["to.mcd"]] <- list(ufat="Mean Centroid Distance", desc="mean centroid distance of temporally overlapping hulls", def=NULL, ufipt="Mean Centroid Distance", nhv=NA, iso.dec=FALSE, expr=expression(hs[[hs.name]][["hulls"]][[paste("to.mcd.", hmap[hmap.idx, "hs2"], sep="")]]), zero2na=FALSE, discrete=TRUE, auto.jiggle=0.01, spao.x=TRUE, spao.y=FALSE, req.metrics="to.mcd", req.ap="hs2", req.ap.def=list(hs2="all"), req.ap.subtitle=so.subtitle, req.ap.desc="My Desc", fun="lhs.to.add", iso.hm2=NULL, iso.hm2.dec=FALSE)

    if (names.only) {
        if (desc) {
            res_desc <- sapply(expr.lst, function(x) if (is.null(x$def)) x$desc else x$def)
            res_ap <- sapply(expr.lst, function(x) if (is.null(x$req.ap)) "" else paste(" (requires '", paste(x$req.ap, collapse="', '", sep=""), "')", sep=""))
            res_fun <- sapply(expr.lst, function(x) if (is.null(x$fun)) "" else paste(", see ", x$fun, sep=""))
            res <- data.frame(metric=names(expr.lst), desc=paste(res_desc, res_ap, res_fun, sep=""))
            rownames(res) <- 1:length(expr.lst)
            if (print) {
                cat("  Hull Metrics \n")
                formatdf4cat(res, indent=2, wrap.last.col=TRUE, print=TRUE, just.left=c(TRUE, TRUE), print.col.titles=FALSE)
            }
            return(invisible(res))
        } else {
            if (print) print(names(expr.lst))
            return(names(expr.lst))
        }
    } else {
        return(expr.lst)
    }
    
}
