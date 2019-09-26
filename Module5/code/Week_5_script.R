#Week 5: Point Pattern Analysis with Fields

library(gstat)
library(maptools)
data(fulmar)

head(fulmar)
?fulmar

fulmar.spdf<-SpatialPointsDataFrame(cbind(fulmar$x, fulmar$y), fulmar)

fulmar.spdf <-fulmar.spdf[fulmar.spdf$year==1999,]

head(fulmar.spdf)

#Method 1: Proximity Polygons

library(dismo)
fulmar.voro <- voronoi(fulmar.spdf)
plot(fulmar.voro)

library(GISTools)
sh<-shading(breaks=c(5,15,25,35), cols=brewer.pal(5, "Purples"))
par(mar=c(0.1,0.1,0.1,0.1))
choropleth(fulmar.voro, fulmar.voro$fulmar, shading=sh, border=NA)
plot(fulmar.voro, border="lightgrey", add=TRUE, lwd=0.5)
choro.legend(px="topright", sh=sh)


#Method 3: Inverse Distance Weighting

#Define a sample grid to use as a set of points to estimate fulmar density
s.grid <- spsample(fulmar.voro, type="regular", n=6000)
idw.est <- gstat::idw(fulmar~1, fulmar.spdf, newdata=s.grid, idp=1.0)

#Extract the distinct x and y coordinates of the grid
#Extract the predicted values and form into a matrix of gridded values
ux<-unique(coordinates(idw.est)[,1])
uy<-unique(coordinates(idw.est)[,2])
predmat <- matrix(idw.est$var1.pred, length(ux), length(uy))

#What happens if we set k (alpha in our textbook) = 2?
idw.est2 <- gstat::idw(fulmar~1, fulmar.spdf, newdata=s.grid, idp=2.0)
predmat2<- matrix(idw.est2$var1.pred, length(ux), length(uy))

par(mar=c(0.1,0.1,0.1,0.1),mfrow=c(1,2))
plot(fulmar.voro, border=NA, col=NA)
.filled.contour(ux,uy,predmat, col=brewer.pal(5,'Purples'), levels=c(0,2,4,6,8,30))

sh<- shading(breaks=c(2,4,6,8), cols=brewer.pal(5,'Purples'))
choro.legend(px="topright", sh=sh, bg="white")

plot(fulmar.voro, border=NA, col=NA)
.filled.contour(ux,uy,predmat2, col=brewer.pal(5,'Purples'), levels=c(0,2,4,6,8,30))
choro.legend(px="topright", sh=sh, bg="white")

#spikiness in interpolation due to honoring all data points
#Three dimensional plots of IDW

par(mfrow=c(1,2), mar=c(0,0,2,0))
persp(predmat,box=FALSE, main="k=1")
persp(predmat2, box=FALSE, main="k=2")

#Method 5: Ordinary Kriging

#default setting is 15 lags
evgm<-variogram(fulmar~1, fulmar.spdf)
plot(evgm)

#change the number of lags
evgm <- variogram(fulmar~1, fulmar.spdf,cutoff=90000,width=90000/30)
plot(evgm)

#fit a "default" Spherical Model.
fvgm<-fit.variogram(evgm,vgm(model="Sph"))
plot(evgm, model=fvgm)

#fit a specific model using customized partial sill, range, and nugget values.
#psill = partial sill value, or where we should cut off the variogram in terms of the semivariance value (i.e. y axis)
#range = range of distance to cut off.
#nugget = semivariance value (i.e. y-intercept) where distance = 0

fvgm<-fit.variogram(evgm, fit.sills = FALSE,
              vgm(psill=13,model="Sph",range=90000,nugget=0))

plot(evgm,model=fvgm)

#try out other models

#Exponential Model

fvgm<-fit.variogram(evgm, fit.sills = FALSE,
                    vgm(psill=13,model="Exp",range=90000,nugget=0))
plot(evgm,model=fvgm)

#Gaussian Model

fvgm<-fit.variogram(evgm, fit.sills = FALSE,
                    vgm(psill=13,model="Gau",range=90000,nugget=0))

plot(evgm,model=fvgm)

#Marten Model
fvgm<-fit.variogram(evgm, fit.sills = FALSE,
                    vgm(psill=13,model="Mat",range=90000,nugget=0))

plot(evgm,model=fvgm)


#ordinary kriging
krig.est <- krige(fulmar~1, fulmar.spdf, newdata=s.grid, model=fvgm)

#interpolated estimates
predmat3<-matrix(krig.est$var1.pred,length(ux), length(uy))
fivenum(predmat3)
par(mar=c(0.1,0.1,0.1,0.1),mfrow=c(1,2))
plot(fulmar.voro, border=NA, col=NA)
filled.contour(ux,uy, pmax(predmat3,0), col=brewer.pal(6,"Purples"), levels=c(0,5,10,15,20,25,30))
sh<-shading(breaks=c(5,10,15,20,25), cols=brewer.pal(6, "Purples"))
choro.legend(px="topright", sh=sh, bg="white")


#variances of interpolated estimates
errmat3<-matrix(krig.est$var1.var,length(ux), length(uy))
fivenum(errmat3)
par(mar=c(0.1,0.1,0.1,0.1),mfrow=c(1,2))
plot(fulmar.voro, border=NA, col=NA)
filled.contour(ux,uy, pmax(errmat3,0), col=rev(brewer.pal(5,"Purples")), levels=c(0,3,6,9,12,15))
sh<-shading(breaks=c(3,6,9,12), cols=rev(brewer.pal(5, "Purples")))
choro.legend(px="topright", sh=sh, bg="white")

#smoother interpolated surface

persp(predmat3, box=FALSE)


