#' Plot the isopleth edge:area ratio as a function of k/a/r
#'
#' Creates a plot of the ratio of edge to area for isopleths
#'
#' @param lhs A LoCoH-hullset object
#' @param id The id(s) of the individual(s) to include in the plot
#' @param k A k-value for the number of nearest neighbors around each point to include in the plot
#' @param r A r-value for the number of nearest neighbors around each point to include in the plot
#' @param a A a-value for the number of nearest neighbors around each point to include in the plot
#' @param s The s value(s) of nearest neighbor sets to include in the plot. If NULL, all values will be used
#' @param hs.names The name(s) of saved hullsets to analyze
#' @param sort.metric The name(s) of isopleth sort metrics to include in the plot
#' @param iso.idx The indices of isopleths to include in the plot
#' @param figs.per.page Number of plots per page
#' @param legend Whether to include a legend. T/F.
#' @param title The title to be displayed. Character. If NULL a title will be constructed.
#' @param title.show Whether to show the title. T/F.
#' @param subtitle Whether to add a subtitle to the automatically constructed title (when \code{title=NULL}, otherwise ignored)
#' @param mar The plot margins. A four item numeric vector
#' @param mgp The distance away from the edge of the plot for the 1) label, 2) tick marks, and 3) axis line. A three-item numeric vector
#' @param png.fn A filename for a PNG file
#' @param png.dir The directory for a PNG file (filename will be constructed automatically). Ignored if png.fn is passed
#' @param png.dir.make Whether to create png.dir if it doesn't exist. T/F
#' @param png.width The width of the PNG image
#' @param png.height The height of the PNG image
#' @param png.pointsize The pointsize (in pixels) for the PNG image, equivalent to the height or width of a character in pixels (increase to make labels appear larger)
#' @param png.fn.pre A prefix that will be used in the construction of the PNG filename. Ignored if png.fn is passed.
#' @param png.fn.suf A suffix that will be used in the construction of the PNG filename. Ignored if png.fn is passed.
#' @param png.overwrite Whether to overwrite an existing PNG file if it exists. T/F.
#' @param panel.num A number or letter to display in the upper left hand corner of the plot when the plot will be used as part of a multi-frame graphic (as in publications). Character
#' @param panel.num.inside.plot Whether to display panel.num inside the plot area itself, as opposed to the title area. Ignored if panel.num is NULL. T/F
#' @param legend.space The amount of additional space on the lower end of the x-axis to make room for the legend. Expressed as a proportion of the range of the x-axis values
#' @param ... Additional parameters that will be passed to the \code{\link{plot}} function
#'
#' @export

lhs.plot.isoear <- function(lhs, id=NULL, k=NULL, r=NULL, a=NULL, s=NULL, hs.names = NULL, sort.metric=NULL,
                             iso.idx=NULL, figs.per.page=1, legend=TRUE,
                             title=NULL, title.show=TRUE, subtitle=TRUE, 
                             mar=c(3.3, 3.2, if (title.show) (if (subtitle) 3.2 else 2.3) else 0.5, 0.5), mgp=c(2, 0.7, 0), 
                             png.fn=NULL, png.dir=NULL, png.dir.make=TRUE, png.width=800, png.height=png.width, png.pointsize=12+(png.width-480)/80,
                             png.fn.pre=NULL, png.fn.suf=NULL, png.overwrite=TRUE,
                             panel.num=NULL, panel.num.inside.plot=!title.show, bg="white", legend.space=if (legend) 0.05 else 0, ...) {

  ## If overlay=T, all series are combined on one axis
  ## Taken out: series=c("hullsets","iso.levels")[ifelse(length(lhs)==1,1,2)], 

    if (!inherits(lhs, "locoh.lhs")) stop("lhs should be of class \"locoh.lhs\"")
    if (!require(sp)) stop("package sp required")

    if (is.null(id) && is.null(r) && is.null(k) && is.null(a) && is.null(s) && is.null(hs.names)) {
        hs <- lhs
    } else {    
        for (str.param in c("k","a","r","s")) assign(str.param, vectorize.parameter(get(str.param)))
        hs <- lhs.select(lhs, id=id, r=r, k=k, a=a, s=s, hs.names=hs.names)
    }
    if (length(hs)==0) stop("No hullsets found matching those criteria")

    ## See if the output directory exists
    if (is.null(png.fn) && !is.null(png.dir) && !file.exists(png.dir)) {
        if (png.dir.make) {
            dir.made <- dir.create(png.dir)
            if (!dir.made) stop(paste("Couldn't make output folder ", png.dir, sep=""))            
        } else {
            stop(paste(png.dir, " doesn't exist and png.dir.make is False, so can not continue."))
        }
    }

    ## Error check sort.metric.all and filter isos.lst.df if needed
    sort.metric.all <- hm.expr(names.only=T, desc=F, print=F)
    if (is.null(sort.metric)) {
        sort.metric <- sort.metric.all
    } else {
        if (FALSE %in% (sort.metric %in% sort.metric.all)) stop("Invalid value in sort.metric")
    }
    
    res <- NULL
    opar <- NULL

    ## So basically we have a lot of hullsets with different methods and k/a/r values, and presumably isopleths
    ## We need all unique combinations of id, method, s, and sort.metric. For each unique combo, we need a matrix
    ## in the form 
    ## iso.level, param.val, area
    ## We start by building a data frame of id, method, sort.metric, param.val, iso.level, area
    
    iso.info.all <- do.call(rbind, lapply(hs, function(myhs) do.call(rbind, lapply(myhs[["isos"]], function(myiso) data.frame(id=myhs[["id"]], mode=myhs[["mode"]], s=myhs[["s"]], param.val=myhs[[myhs[["mode"]]]], sort.metric=myiso[["sort.metric"]], myiso[["polys"]]@data[ c("iso.level", "area", "edge.len")])))))
    row.names(iso.info.all) <- NULL
    
    id.mode.metric <- with(iso.info.all, paste(id, "|", mode, "|", sort.metric, "|", s, sep=""))
    
    for (immVal in unique(id.mode.metric)) {
         iso.info.immVal <- iso.info.all[id.mode.metric==immVal, ]
         
         param.vals.immVal <-sort(unique(iso.info.immVal[["param.val"]]))
         iso.levels.immVal <-sort(unique(iso.info.immVal[["iso.level"]]))
         col.overlay <- rainbow(length(iso.levels.immVal), end=5/6)
         
         x.mat <- matrix(param.vals.immVal, ncol=length(iso.levels.immVal), nrow=length(param.vals.immVal))
         y.mat <- matrix(NA, ncol=length(iso.levels.immVal), nrow=length(param.vals.immVal))
         
         iso.info.immVal <- transform(iso.info.immVal, param.val=as.factor(param.val), iso.level=as.factor(iso.level))
         iso2colidx <- match(levels(iso.info.immVal[["iso.level"]]), as.character(iso.levels.immVal))
         paramval2rowidx <- match(levels(iso.info.immVal[["param.val"]]), as.character(param.vals.immVal))

         id.str <- as.character(iso.info.immVal[1, "id"])
         param.str <- levels(iso.info.immVal[["mode"]])
         s.str <- as.character(iso.info.immVal[1, "s"])
         sort.metric.str <- iso.info.immVal[1, "sort.metric"]
         fn.series <- paste(".", param.str, ".vs.isoear", sep="")
                   
         ## Open a PNG device if needed
          if (!is.null(png.dir) || !is.null(png.fn)) {
              png.fn.use <- NULL
              if (is.null(png.fn)) {
                  png.fn.use <- file.path(png.dir, paste(png.fn.pre, id.str, ".s", s.str, ".srt-", sort.metric.str, fn.series, png.fn.suf, ".png", sep=""))
              } else {
                  png.fn.use <- png.fn
              }
              if (file.exists(png.fn.use) && !png.overwrite) stop(paste(png.fn.use, "exists"))
              png(filename=png.fn.use, height=png.height, width=png.width, pointsize=png.pointsize, bg=bg)
              res <- c(res, png.fn.use)
          }
          if (is.null(opar)) opar <- par(mfrow = n2mfrow(figs.per.page), mar=mar, mgp=mgp)

          ## Construct the plot title
          if (title.show) {
              if (is.null(title)) {
                  hs.sub <- if (subtitle) paste("\n", paste(id.str, collapse=".", sep=""), ", s=", paste(s.str, collapse=".", sep=""), sep="") else ""
                  title.use <- paste(param.str, " vs. isopleth edge-area ratio", hs.sub, sep="")
              } else {
                  title.use <- title
              }
          } else {
              title.use <- NULL
          }


         for (i in 1:nrow(iso.info.immVal)) {
             y.mat[ paramval2rowidx[as.numeric(iso.info.immVal[i,"param.val"])], iso2colidx[as.numeric(iso.info.immVal[i, "iso.level"])]] <- iso.info.immVal[i, "edge.len"] / iso.info.immVal[i, "area"]
         }
         
         rx <- range(x.mat[,1])
         xlim <- rx - c(legend.space * (rx[2]-rx[1]), 0)
         
         matplot(x.mat, y.mat, xlim=xlim, type="b", xlab=param.str, ylab="edge / area", col=col.overlay,
                 main=title.use, pch=20, lty=3, ...)
         if (legend) legend("topright", as.character(iso.levels.immVal), col=col.overlay, lty=1, title="iso level", cex=0.8)

        ## Add panel.num
        if (!is.null(panel.num)) {
            if (panel.num.inside.plot) {
                text(x=par("usr")[1], y=par("usr")[4], labels=panel.num, cex=2, adj=c(-0.3,1.2), font=2)
            } else {
                mar.old <- par("mar")
                par(mar=c(0, 0.3, 0.2, 0))
                title(main=panel.num, adj=0, cex.main=2, line=-1.5)
                par(mar=mar.old)
            }
        }

        ## Close PNG device
        if (!is.null(png.dir) || !is.null(png.fn)) invisible(dev.off())

    }


    if (!is.null(png.dir) || !is.null(png.fn)) {
        cat(paste(" - ", res, collapse="\n", sep=""), "\n", sep="")
    } 
}
