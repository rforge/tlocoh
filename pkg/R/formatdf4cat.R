formatdf4cat <- function(df, indent=2, wrap.last.col=TRUE, just.left=NULL, print=TRUE, print.col.titles=TRUE) {
    ## This will take a data frame and convert it to a character vector that will look aligned when printed with the cat command.
    ## The table will be indented by indent
    ## wrap.last.col determines whether the last column will wrap to the next line (as opposed to bleeding off the window). T/F
    ## If print=T, will print the results on screen
    ## If print.col.titles=T, the column titles will print as well
    ## just.left is a boolean vector that sets whether the column is left justified. If passed, just.left should be 
    ## equal in length to the number of columns. Note that the last column will always be left justified
    
    if (is.null(just.left)) {
        just.left <- rep(FALSE, ncol(df))
    } else {
        if (length(just.left) != ncol(df)) stop("just.left should be the same length as the number of columns in df")
    }
    
    ## Coerce all columns to character
    if (!is.matrix(df)) df <- do.call(cbind, lapply(df, as.character))
    
    cnames <- colnames(df)


    col.width <- rep(0, ncol(df)-1)

    ## Get the maximum width of each column and pad entries with leading or trailing blanks as needed
    
    for (i in 1:(ncol(df)-1)) {
        col.width[i] <- max(nchar(as.character(df[,i])))
        
        ## Create string of blank spaces
        padding.str <- sapply(col.width[i] - sapply(df[,i], nchar), function(x) paste(rep(" ", x), collapse="", sep=""))
        
        df[,i] <- paste(if (just.left[i]) "" else padding.str, df[,i], if (just.left[i]) padding.str else "", " ", sep="")
        cnames[i] <- paste(paste(rep(" ", times=col.width[i] - nchar(cnames[i])), collapse="", sep=""), cnames[i], " ", sep="")
        
        ## If this is the first column, apply the indent
        if (i==1 && indent > 0) {
            df[,i] <- paste(paste(rep(" ", times=indent-1), collapse="", sep=""), df[,i])
            cnames[i] <- paste(paste(rep(" ", times=indent-1), collapse="", sep=""), cnames[i])
        }
        
    }
    
    ## Insert blanks and carriage returns in the second column to achieve an indented look
    if (wrap.last.col) {
    
        # Pad last column in preparation for the cw function
        num.leading.spaces <- indent - 1 + sum(col.width) + ncol(df)
        initial.buf <- paste(rep("-", times=num.leading.spaces), collapse="", sep="")
        
        df[,ncol(df)] <- paste(initial.buf, df[,ncol(df)], sep="")
        
        # Apply the cw function to insert \n characters
        df[,ncol(df)] <- sapply(df[,ncol(df)], function(x) cw(x, indent=0, exdent=num.leading.spaces, final.cr=FALSE))

        # Get rid of the initial padding
        df[,ncol(df)] <- substr(df[,ncol(df)], num.leading.spaces + 1, stop=sapply(df[,ncol(df)], nchar))
    }
    
    if (print) {
        if (print.col.titles) cat(paste(cnames, collapse="", sep=""), "\n", sep="")
        do.call(cat, list(x=unlist(lapply(1:nrow(df), function(i) paste(paste(df[i,], collapse="", sep=""), "\n", sep=""))), sep=""))
    }
    return(invisible(df))
}
