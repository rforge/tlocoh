lxy.grab <- function(lxy, nn.idx=1, pp.idx="auto", nn.rank="auto") {
                               
    if (!require(sp)) stop("package sp required")
    if (is.null(lxy[["nn"]])) stop("nn set not found")
    
    nn.df <- lxy[["nn"]][[nn.idx]][["nn.df"]]
    
    
    
    if (identical(pp.idx, "auto") && identical(nn.rank, "auto")) {
        nn.df.idx.use <- sample(1:nrow(nn.df), 1)
    } else {

        if (identical(pp.idx, "auto")) {
            pp.idx.use <- sample(unique(nn.df[["pp.idx"]]), 1)
        } else {
            pp.idx.use <- pp.idx
        }
        pp.rows <- which(nn.df[["pp.idx"]] == pp.idx.use)
        
        if (nn.rank == "auto") {
            nn.df.idx.use <- sample(pp.rows, 1) 
        } else {
            nn.df.idx.use <- pp.rows[which(nn.df[pp.rows,"nn.rank"]==nn.rank)]
        }
        
    }
    
    pp.xys <- coordinates(lxy[["pts"]])[nn.df[nn.df.idx.use,"pp.idx"],  ]
    nn.xys <- coordinates(lxy[["pts"]])[nn.df[nn.df.idx.use,"nn.idx"],  ]
    tdif <- list(tdif=abs(as.numeric(lxy[["pts"]][["dt"]][nn.df[nn.df.idx.use,"nn.idx"]]) - as.numeric(lxy[["pts"]][["dt"]][nn.df[nn.df.idx.use,"pp.idx"]])))
    tsd <- list(tsd=nn.df[nn.df.idx.use,"tsd"])
    sVal <- list(s=lxy[["nn"]][[nn.idx]][["s"]])
    rw.params <- lxy[["rw.params"]] [lxy[["rw.params"]][["id"]] == lxy[["nn"]][[nn.idx]][["id"]],  -1]

    toclip <- c(pp.xys, nn.xys, tdif, sVal, tsd, rw.params)
    
    write.table(toclip, "clipboard", sep="\t", row.names=FALSE)
    
    print(unlist(toclip))
    
    
}
