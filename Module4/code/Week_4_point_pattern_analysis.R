# Week 4: Point Pattern Analysis Using R

library(GISTools)
library(raster)
library(spatstat)

data(newhaven)
ls()

plot(blocks)
plot(breach,add=T, col="red")

is.projected(breach)
proj4string(breach)

# "events"
xy<-coordinates(breach)

dim(xy)

head(xy)

#mean center
mc <- apply(xy,2,mean)
mc

#standard distance
sd <- sqrt(sum((xy[,1] - mc[1])^2 + (xy[,2] - mc[2])^2) / nrow(xy))
sd


#plot the mean center
raster::plot(blocks, col='light blue')
points(breach, cex=.5)
points(cbind(mc[1], mc[2]), pch='*', col='red', cex=5)


####FIRST ORDER EFFECTS

## Method 1: Quadrant count approach

# create number of grids for raster map
Area <- raster::area(blocks)
Density <- nrow(xy)/Area

r <- raster(blocks, ncol=50,nrow=50)

r <- rasterize(blocks,r)
plot(r)

#place boundaries around each grid
quads <- as(r, 'SpatialPolygons')
plot(quads, add=TRUE)
#plot points
points(breach, col='red', cex=.5)


# count events
events <- rasterize(coordinates(breach), r, fun='count', background=0)
plot(events)
plot(blocks, add=TRUE)

#find frequency

f <- raster::freq(events, useNA='no')
head(f,20)


# number of quadrats
quadrats <- sum(f[,2])
# number of cases
cases <- sum(f[,1] * f[,2])
mu <- cases / quadrats
mu


#Calculating the variance-mean ratio (VMR)
ff <- data.frame(f)
colnames(ff) <- c('K', 'X')
ff$Kmu <- ff$K - mu
ff$Kmu2 <- ff$Kmu^2
ff$XKmu2 <- ff$Kmu2 * ff$X
head(ff)

s2 <- sum(ff$XKmu2) / (sum(ff$X)-1)
s2

VMR <- s2 / mu
VMR

#Redo the above with a different number of quadrants. What happens when you increase the number of quadrants?



#another approach

library(spatstat)

breach.ppp<-as.ppp(breach)

q <- quadratcount(breach.ppp,50,50)
plot(q)
plot(breach.ppp, add=T)
quadrat.test(breach.ppp,50,50)
#look at the p-value.


## Method 2: Kernel Density Estimate

#calculate density
#library(GISTools)
breach.density <- kde.points(breach, lims=blocks)

#create level plot
level.plot(breach.density)


#create a map of the boundary of studied area
masker <- poly.outer(breach.density, tracts, extend=100)
#plot(masker)

add.masking(masker) #read the help file for add.masking

#now put the blocks on top
plot(blocks,add=TRUE)


#An example of creating two KDE maps side-by-side
par(mfrow=c(1,2), mar=c(0,0,2,0))

brf.density <- kde.points(burgres.f, lims=blocks)
level.plot(brf.density)
masker<-poly.outer(brf.density, blocks, extend=100)
add.masking(masker)
plot(blocks, add=TRUE)
title("Forced Burglaries")

brf.density <- kde.points(burgres.n, lims=blocks)
level.plot(brf.density)
masker<-poly.outer(brf.density, blocks, extend=100)
add.masking(masker)
plot(blocks, add=TRUE)
title("Non-Forced Burglaries")

par(mfrow=c(1,1)) #reset plot area


####SECOND ORDER EFFECTS

#Method 1: Nearest Neighbors

#convert to ppp object
#library(spatstat)
breach.ppp<-as.ppp(breach)
nnd <- nndist.ppp(breach.ppp)
summary(nnd)

#calculate the mean nearest-neighbor distance
mnnd <- mean(nnd)
mnnd
#quite large!

#compare against expected pattern for IRP/CSR
W <- as.owin(blocks) #must convert area of study to object in spatstat
exp_nnd <- 0.5 / sqrt(breach.ppp$n / area.owin(W)) #calculate the expected IRP/CSR mean nearest-neighbor distance
mnnd / exp_nnd #ratio of observed/expected #compare to Clark & Evan's R statistic


#Method #2: G-function or the fraction of nearest neighbors < d

Gf <- envelope(breach.ppp, Gest)
plot(Gf)

#correction for edge effects 
Gf <- envelope(breach.ppp, Gest, correction="border")
#read the documentation for Gest function
plot(Gf)

mad.test(breach.ppp,Gest)
dclf.test(breach.ppp,Gest)

#Method #3: F-function or fraction of nearest neighbors to random point locations (hazard rate)

Ff <- envelope(breach.ppp, Fest, correction="border")
plot(Ff)

mad.test(breach.ppp,Fest)
dclf.test(breach.ppp,Fest)

#Method #4: K-function or fraction of neighbors within a certain radius

Kf <- envelope(breach.ppp, Kest, correction="border")
plot(Kf)

mad.test(breach.ppp,Kest)
dclf.test(breach.ppp,Kest)

#Method #5: L-function or the linear transformation of the K-function
#Preferred over K-function for ease of use

Lf <- envelope(breach.ppp, Lest, correction="border")
plot(Lf)

mad.test(breach.ppp,Lest)
dclf.test(breach.ppp,Lest)


