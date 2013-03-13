lxy.lhs.wiz <- function (lxy, tau.thin=0.5, xy.mov=FALSE, s="auto", sa.signif=1, sa.tau.range=c(10, 500), sa.quant=0.5, plot.all=TRUE,
                         method=c("a","k")[1], k=3*(1:9), iso.levels=0.15+0.10*(0:8), status=TRUE, img.display.width=400,
                         a="auto", a.num=9, a.digits="auto", kmin=2, fn.html=NULL,
                         out.dir="lhswiz", img.subdir="images", shp.csv=NULL, layers=NULL) {


    if (!inherits(lxy, "locoh.lxy")) stop("lxy should be of class \"locoh.lxy\"")
    if (is.null(lxy[["pts"]])) stop("Old data structure detected, please update with lxy.repair()")

    



    stop("Done")

    html.header <- "<html><head><title>T-LoCoH Hullset Wizard</title></head><body>\n"
    html.footer <- "<p><em>T-LoCoH for R. (c) Lyons and Getz, UC Berkeley, 2012</em></p><br></body></html>"
    html.lst <- list()

    if (!file.exists(out.dir)) {
        dir.made <- dir.create(out.dir)
        if (!dir.made) stop(paste("Couldn't make output folder ", out.dir, sep=""))
    }

    if (length(img.subdir) > 0) {
        img.dir <- file.path(out.dir, img.subdir)
        if (!file.exists(img.dir)) {
            dir.made <- dir.create(img.dir)
            if (!dir.made) stop(paste("Couldn't make images folder ", img.dir, sep=""))
        }
    } else {
        img.dir <- out.dir
    
    }

    for (idVal in levels(lxy$id)) {
        html.lst[[idVal]] <- paste("<h2 align=\"center\">Hullsets for ", idVal, "</h2>", sep="")
    }

    ## Create fn.html if needed
    if (is.null(fn.html)) {
        fn.html.use <- file.path(out.dir, paste(paste(levels(lxy$id), collapse=".", sep=""), "_index.html", sep=""))
    } else {
        fn.html.use <- file.path(out.dir, fn.html)
    }
    

    ## Cleaning
    if (tau.thin > 0) {
        if (status) cat("Temporal thinning..."); flush.console()
        pl.freq <- lxy.plot.freq(lxy, cp=T, png.dir=img.dir, desc=0, status=FALSE)
        for (idVal in levels(lxy$id)) {
            html.lst[[idVal]] <- c(html.lst[[idVal]], "<h3>Temporal Cleaning</h3> \n")
            plf.idval <- pl.freq[sapply(pl.freq, function(x) x$id==idVal)][[1]]
            fn.png <- plf.idval$fn
            #print("let remove the dirs from images");browser()
            ## remove dir.out and img.subdir
            if (substr(fn.png, 1, nchar(out.dir))==out.dir) {
                fn.png <- substr(fn.png, nchar(out.dir) + 2, nchar(fn.png))
            #    print(fn.png)
            }
            #if (substr(fn.png, 1, nchar(img.subdir))==img.subdir) {
            #    fn.png <- substr(fn.png, nchar(img.subdir) + 1, nchar(fn.png))
            #    print(fn.png)
            #}
            #html.lst[[idVal]] <- c(html.lst[[idVal]],  paste("<p><a href=\"test.png\"><img src=\"", fn.png, "\" width=", img.display.width, " border=1></a><br>", plf.idval$desc, "</p> \n", sep=""))
            html.lst[[idVal]] <- c(html.lst[[idVal]],  paste("<table width=", img.display.width + 50, "><tr><td><a href=\"", fn.png, "\"><img src=\"", fn.png, "\" width=", img.display.width, " border=2></a><br><font size=-1>", plf.idval$desc, "</font></td</tr></table> \n", sep=""))
            cat("Done.\n")
        }
    }
    
    
    
    ## Prepare GIS features
    gis.features <- if (is.null(layers)) list() else shp.layers(layers, shp.csv=shp.csv)

    ## Plot of the locations

    ## Animation
    
    ## Histogram
    
    ## sampling interval by date
    
    ## S finder
    
    ## a or k finder
    
    
    ## Isopleths
    
    


    ## Save HTML file to disk
    if (TRUE) {
    
        #print("lets see");browser()
    
        html.out.con <- file(fn.html.use, open = "w")
        writeLines(html.header, con=html.out.con)
        for (idVal in levels(lxy$id)) {
            writeLines(html.lst[[idVal]], con=html.out.con)
        }
        writeLines(html.footer, con=html.out.con)
        close(html.out.con)

        if (status) cat("   Saved ", fn.html.use, "\n", sep="")
    }



}