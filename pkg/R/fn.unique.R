
fn.unique <- function (fn.base=NULL, dir=".", suf=NULL, ext="shp", exclude.ext=TRUE,
                       auto.inc.width=2, auto.inc.max=(10^auto.inc.width)-1, first.one.numbered=TRUE) {

    ## returns a unique file name for a new shapefile in the form
    ##    fn.base.xx.suf.ext
    ##
    ## where xx is a auto incremented numeric token to produce a unique filename
    ## exclude.ext - option to exclude the extension in the returned object (which is not 
    ##               needed when creating a shapefile by the rgdal or shapefiles package
    ## auto.inc.width is the number of digits in the auto-number token
    ## auto.inc.max is the maximum value of the auto.increment (maximum is determined by auto.inc.width)
    ##  
    ## fn.base is the 'base' of the filename (no extension),
    ## dir is the destination directory

    if (is.null(fn.base)) {
        return(NULL)
    } else {
        
        if (auto.inc.max > (10^auto.inc.width)-1) stop(cw("The maximum value for auto.inc.max is (10^auto.inc.width)-1. Increase the value of auto.inc.width.", final.cr=FALSE))

        ## First, strip any extension from fn.base
        if(tolower(substr(fn.base, nchar(fn.base) - 3, nchar(fn.base))) == paste(".", ext, sep="")) {
            fn.base.noext <- substr(fn.base, 0, nchar(fn.base) - 4)
        } else {
            fn.base.noext <- fn.base
        }                        

        if (first.one.numbered) {
            first.num <- paste(".", paste(rep("0", times=auto.inc.width), collapse="", sep=""), sep="")
        } else {
            first.num <- NULL
        }

        if (is.null(suf) || suf=="") {
            suf.sep <- NULL
        } else {
            suf.sep <- "."
        }
        
        fn.fullname = file.path(dir, paste(fn.base.noext, first.num, suf.sep, suf, ".", ext, sep=""))
    
        ## See if the file already exists
        if (file.exists(fn.fullname)) {
           
           # file exists, construct a new name
           i <- 1
	         fn.fullname <- file.path(dir, paste(fn.base.noext, ".", formatC(i, flag=0, width=auto.inc.width), suf.sep, suf, ".", ext, sep=""))
           while (file.exists(fn.fullname)) {
              i <- i + 1
              if (i > auto.inc.max) stop(paste("Can't find a unique filename for maximum auto increment token=", auto.inc.max, sep=""))
              fn.fullname <- file.path(dir, paste(fn.base.noext, ".", formatC(i, flag=0, width=auto.inc.width), suf.sep, suf, ".", ext, sep=""))
           }       
        }
        
    }
     
    ## Return filename	    
    if (exclude.ext) {
        ## Minus the extension
	return(substr(fn.fullname, 0, nchar(fn.fullname) - 4))
    } else {
        return(fn.fullname)
    }

}
