## FINDING THE AREA OF OVERLAP BETWEEN TWO HOME RANGES
##
## A number of indices of association have been developed by ecologists for location data*. The most
## basic one (but also intuitive) is simply the area of overlap in the home range or core area (often
## approximated by the 95% and 50% isopleth respectively). In this exercise, we'll find the area of
## intersection of the home ranges of two individuals, as computed by T-LoCoH. This same technique
## could be used to find the area of HR overlap for the same individual during two different time
## periods, such as wet season and dry season. 
## 
## Hull-based home range estimators like T-LoCoH compute utilization distributions as a series of
## nested polygons. So our workflow is essentially to get the correct polygon for each individual, and
## then find the area of intersection. We'll be working with home ranges data for two buffalo, pepper
## and toni, from Kruger National Park in South Africa whose location data are available on
## MoveBank.org. These datasets have already been analyzed by T-LoCoH and home ranges constructed (see
## T-LoCoH tutorial for details). Note these datasets are illustrative only, the parameters used have
## not been cross-checked. 
##
## * for a review of association indices, see Miller, J. 2012. Using Spatially Explicit Simulated Data
## to Analyze Animal Interactions: A Case Study with Brown Hyenas in Northern Botswana. Transactions in
## GIS v16, n3, pp. 271-291, June 2012. 
## http://onlinelibrary.wiley.com/doi/10.1111/j.1467-9671.2012.01323.x/full

## Load T-LoCoH. If you don't have the T-LoCoH package for R installed, you can get it from 
## http://tlocoh.r-forge.r-project.org/

require(tlocoh)

## Download the hullset for 'pepper' 

mycon <- url("http://tlocoh.r-forge.r-project.org/pepper.n4571.s0.003.k15.iso.lhs.01.RData")
load(mycon); close(mycon) 

## See what it looks like

summary(pepper.lhs.k15)
plot(pepper.lhs.k15, iso=T)

## A locoh-hullset object contains a bunch of stuff. We only need the 
## isopleth polygons, which we can extract with the isopleths() function 

pepper.isos <- isopleths(pepper.lhs.k15)

## The isopleths() function returns a list of SpatialPolygonDataFrame
## objects, which is how isopleths are saved. In our case, there will 
## only be one list element because pepper only has one set of hulls 
## with one set of isopleths.

class(pepper.isos)
names(pepper.isos)
class(pepper.isos[[1]])

## Let's look at the attached dataframe

pepper.isos[[1]]@data

## Next, we get just the 50th and 95% isopleths for Pepper.
## We are pulling these out using standard indexing syntax
## for SpatialPolygonDataFrames (which is very similar to the
## indexing notation for regular data frames)

pepper.core <- pepper.isos[[1]][ pepper.isos[[1]][["iso.level"]]==0.5, ]
class(pepper.core)
pepper.hr <- pepper.isos[[1]][ pepper.isos[[1]][["iso.level"]]==0.95, ]
class(pepper.hr)

## Plot the core and homerange

plot(pepper.hr, border="blue")
plot(pepper.core, border="red", add=T)

## Next, do the same for toni

mycon <- url("http://tlocoh.r-forge.r-project.org/toni.n5775.s0.003.k15.iso.lhs.01.RData")
load(mycon); close(mycon) 
plot(toni.lhs.k15, iso=T)
toni.isos <- isopleths(toni.lhs.k15)
toni.core <- toni.isos[[1]][ toni.isos[[1]][["iso.level"]]==0.5, ]
toni.hr <- toni.isos[[1]][ toni.isos[[1]][["iso.level"]]==0.95, ]
plot(toni.hr, border="green", add=T)
plot(toni.core, border="purple", add=T)

## Next, let's find the intersection between pepper's core area 
## and toni's core area. First we'll overlay them on a fresh plot

plot(pepper.core, border="red")
plot(toni.core, border="purple", add=T)

## Next, we'll find the area of intersection of the core areas using 
## a function from the rgeos package

require(rgeos)
tp.core.common <- gIntersection(pepper.core, toni.core)
class(tp.core.common)
plot(tp.core.common, col="brown", add=T)

## Find the area of intersection of the core areas.
## There isn't a simple built-in function that returns polygon area, but 
## you can easily get it if you know a little bit about the structure
## of a SpatialPolygons object

tp.core.common@polygons[[1]]@area

## Next, let's do the same for the intersection of their home ranges

plot(pepper.hr, border="blue")
plot(toni.hr, border="green", add=T)
tp.hr.common <- gIntersection(pepper.hr, toni.hr)
plot(tp.hr.common, col="brown", add=T)
tp.hr.common@polygons[[1]]@area

## We see that the area of overlap in the home ranges of toni and pepper is quite a bit larger than the
## area of overlap of the core areas (which is expected because home ranges are by definition larger
## than core areas).

## This concludes our exercise. A more advanced technique would be to convert the isopleths into a
## raster probability surface and use the volume of intersection measure of overlap, which we'll do
## in another exercise.



