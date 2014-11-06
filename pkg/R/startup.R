.onAttach <- function(lib, pkg) {
    ver <- read.dcf(file.path(lib,pkg,"DESCRIPTION"), "Version")
    msg <- sprintf("T-LoCoH for R (version %s)\nURL: http://tlocoh.r-forge.r-project.org/\nBug reports: tlocoh@gmail.com", as.character(ver))
    packageStartupMessage(msg)
}
