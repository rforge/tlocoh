#' Divides a long string into separate lines for adding to a plot
#'
#' Breaks a long character object into separate lines at specific characters 
#'
#' @param str The string to format
#' @param width The maximum width of any single line in inches
#' @param char.break The character where the string should be broken
#' @param char.break.keep Whether to keep the char.break character when the string gets broken. T/F
#' @param cex The character expansion factor that will be used to display the text 
#'
#' @note
#' This function is designed to break up a potentially long character object that is going to be 
#' added to a plot either as a title or text. Line breaks will be inserted so that the string will
#' wrap appropriately at \code{width} inches when plotted with \code{cex} character expansion factor. 
#'
#' The character object will be broken at the \code{char.break} characters
#' such that each line will be <= \code{width} when plotted. Breaks are replaced with carriage returns. If 
#' \code{char.break.keep} is \code{TRUE}, the break character will be kept, otherwise it will be removed. If no occurrence
#' of the /code{char.break} is available, the line will be chopped at the last character that fits.
#' 
#' @return A two-item character vector containing [1] the formatted character string with \\n inserted and [2] number of lines
#'
#' @export

chop2plot <- function(str, width, char.break=" ", char.break.keep=FALSE, cex=1) {
    
    pieces <- NULL
    cur.width <- 0
    last.char.break.pos <- 0
    this.piece <- ""
    trunc.num <- if (char.break.keep) 0 else 1
    
    for (i in 1:nchar(str)) {
        this.piece <- paste(this.piece, substr(str, i, i), sep="")
        cur.width <- strwidth(this.piece, units="inches", cex=cex)
        
        ## Update last.char.break.pos if we're less than widht or it'd be ok to chop off the char.break
        if (cur.width <= width || !char.break.keep) {
            if (substr(str, i, i) == char.break) last.char.break.pos <- nchar(this.piece)
        }
        if (cur.width > width) {
            ## Need to chop
            if (last.char.break.pos == 0) {
                ## There hasn't been any char.break characters found, just chop off the entire this.piece
                pieces <- c(pieces, this.piece)
                this.piece <- ""
            } else {
                ## Chop off this.piece at the last encounter of char.break
                pieces <- c(pieces, substr(this.piece, 1, last.char.break.pos - trunc.num))
                if (last.char.break.pos == nchar(this.piece) && !char.break.keep) {
                    this.piece <- ""
                } else {
                    this.piece <- substr(this.piece, last.char.break.pos + 1, nchar(this.piece)) 
                }
            }
            last.char.break.pos == 0
        }
    }
    if (nchar(this.piece) > 0) pieces <- c(pieces, this.piece)
    return(c(paste(pieces, collapse="\n", sep=""), as.character(length(pieces))))

}
