
## ------------------------------------------------------------------------
require(tlocoh)


## ------------------------------------------------------------------------
mycon <- url("http://tlocoh.r-forge.r-project.org/pepper.n4571.s0.003.k15.iso.lhs.01.RData")
load(mycon); close(mycon) 


## ------------------------------------------------------------------------
summary(pepper.lhs.k15)
plot(pepper.lhs.k15, iso=T)


## ------------------------------------------------------------------------
pepper.isos <- isopleths(pepper.lhs.k15)


## ------------------------------------------------------------------------
class(pepper.isos)
names(pepper.isos)
class(pepper.isos[[1]])


## ------------------------------------------------------------------------
pepper.isos[[1]]@data


## ------------------------------------------------------------------------
pepper.core <- pepper.isos[[1]][ pepper.isos[[1]][["iso.level"]]==0.5, ]
class(pepper.core)
pepper.hr <- pepper.isos[[1]][ pepper.isos[[1]][["iso.level"]]==0.95, ]
class(pepper.hr)


## ------------------------------------------------------------------------
plot(pepper.hr, border="blue")
plot(pepper.core, border="red", add=T)


## ------------------------------------------------------------------------
mycon <- url("http://tlocoh.r-forge.r-project.org/toni.n5775.s0.003.k15.iso.lhs.01.RData")
load(mycon); close(mycon) 
plot(toni.lhs.k15, iso=T)
toni.isos <- isopleths(toni.lhs.k15)
toni.core <- toni.isos[[1]][ toni.isos[[1]][["iso.level"]]==0.5, ]
toni.hr <- toni.isos[[1]][ toni.isos[[1]][["iso.level"]]==0.95, ]
plot(toni.hr, border="green", add=T)
plot(toni.core, border="purple", add=T)


## ------------------------------------------------------------------------
plot(pepper.core, border="red")
plot(toni.core, border="purple", add=T)


## ------------------------------------------------------------------------
require(rgeos)
tp.core.common <- gIntersection(pepper.core, toni.core)
class(tp.core.common)
plot(pepper.core, border="red")
plot(toni.core, border="purple", add=T)
plot(tp.core.common, col="brown", add=T)


## ------------------------------------------------------------------------
tp.core.common@polygons[[1]]@area


## ------------------------------------------------------------------------
plot(pepper.hr, border="blue")
plot(toni.hr, border="green", add=T)
tp.hr.common <- gIntersection(pepper.hr, toni.hr)
plot(tp.hr.common, col="brown", add=T)
tp.hr.common@polygons[[1]]@area


