
## ------------------------------------------------------------------------
require(tlocoh)


## ----, cache=TRUE--------------------------------------------------------
mycon <- url("http://tlocoh.r-forge.r-project.org/toni.n5775.s0.003.a6000.lhs.01.RData")
load(mycon); close(mycon) 
summary(toni.lhs)


## ----tonilhs_allpts, cache=TRUE------------------------------------------
plot(toni.lhs, allpts=TRUE)


## ----tonilhs_iso, cache=TRUE---------------------------------------------
toni.lhs <- lhs.iso.add(toni.lhs, status=FALSE)
plot(toni.lhs, iso=TRUE)


## ----tonilhs_visitadd----------------------------------------------------
toni.lhs <- lhs.visit.add(toni.lhs, ivg=3600*20)


## ----tonilhs_hist2-------------------------------------------------------
par(mfrow=c(1,2))
hist(toni.lhs, lo.margins.set=FALSE, metric="nsv")
hist(toni.lhs, lo.margins.set=FALSE, metric="mnlv")


## ----tonilhs_hppnsv------------------------------------------------------
plot(toni.lhs, hpp=T, hpp.classify="nsv", hpp.classify.chop=0, col.ramp="rainbow")


## ----tonilhs_hppmnlv-----------------------------------------------------
plot(toni.lhs, hpp=T, hpp.classify="mnlv", hpp.classify.chop=0, col.ramp="rainbow")


## ----tonilhs_data--------------------------------------------------------
class(toni.lhs[[1]]$hulls)
names(toni.lhs[[1]]$hulls@data)


## ----tonilhs_hulls-------------------------------------------------------
head(toni.lhs[[1]]$hulls@data)


## ------------------------------------------------------------------------
hm <- toni.lhs[[1]]$hulls@data
head(hm)


## ------------------------------------------------------------------------
head(hm[order(hm$mnlv.72000, decreasing=T),])


## ------------------------------------------------------------------------
ptidLongDur <- 1537


## ----tonilhs_hull1537----------------------------------------------------
plot(toni.lhs, hulls=T, ptid=1537, allpts=T)


## ----tonilhs_ptsdata-----------------------------------------------------
pts <- toni.lhs[[1]]$pts@data
head(pts)


## ----tonilhs_ptidlongdur-------------------------------------------------
pts[pts$ptid==ptidLongDur, ]


## ------------------------------------------------------------------------
idxLongDur <- which(toni.lhs[[1]]$hulls@data$ptid == ptidLongDur)
idxLongDur


## ------------------------------------------------------------------------
names(toni.lhs[[1]]$enc.pts)
length(toni.lhs[[1]]$enc.pts$idx)
head(toni.lhs[[1]]$enc.pts$idx)


## ------------------------------------------------------------------------
idxEncPts <- toni.lhs[[1]]$enc.pts$idx[[idxLongDur]]
idxEncPts


## ----showdatepp, cache=TRUE----------------------------------------------
pts[idxEncPts, "dt"]


## ----setdeltat, cache=TRUE-----------------------------------------------
encPtsDtOrd <- sort(pts[idxEncPts, "dt"])
encPtsDtOrd
deltat <- c(NA, difftime(encPtsDtOrd[2:length(encPtsDtOrd)], encPtsDtOrd[1:length(encPtsDtOrd)-1]))
data.frame(dt=encPtsDtOrd, deltat=deltat)


## ----tonilhs_scatter-tus-------------------------------------------------
lhs.plot.scatter(toni.lhs, x="nsv", y="mnlv")


## ----load_toni-k15, cache=TRUE-------------------------------------------
mycon <- url("http://tlocoh.r-forge.r-project.org/toni.n5775.s0.003.k15.lhs.01.RData")
load(mycon); close(mycon) 
summary(toni.k15.lhs)


## ----tonik_addiso, cache=TRUE--------------------------------------------
toni.k15.lhs <- lhs.iso.add(toni.k15.lhs, status=FALSE)
plot(toni.k15.lhs, iso=T, allpts=T, cex.allpts=0.25)


## ------------------------------------------------------------------------
toni.k15.lhs <- lhs.visit.add(toni.k15.lhs, ivg=3600*20)


## ----histpairs1, fig.show='hold', fig.width=8, cache=TRUE----------------
op <- par(mfrow=c(1,2))
hist(toni.lhs, lo.margins.set=FALSE, metric="nsv")
hist(toni.k15.lhs, lo.margins.set=FALSE, metric="nsv")
par(op)


## ----histpairs2, fig.show='hold', fig.width=8, cache=TRUE----------------
op <- par(mfrow=c(1,2))
hist(toni.lhs, lo.margins.set=FALSE, metric="mnlv")
hist(toni.k15.lhs, lo.margins.set=FALSE, metric="mnlv")
par(op)


