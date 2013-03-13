formatdf4print <- function(df, indent=3) {
    ## This will take a data frame and pad the colname of the first column with blank spaces
    ## such that when printed with print(df, row.names=F) the table will 
    ## be indented 
    
    ## Reduce indent by 1, because the print() command automatically adds one space
    indent <- indent - 1
    names(df)[1] <- paste(paste(rep(" ", max(indent, max(nchar(as.character(df[,1]))) + indent - nchar(names(df)[1])    )), collapse="", sep=""), names(df)[1], sep="")
    return(df)

}
