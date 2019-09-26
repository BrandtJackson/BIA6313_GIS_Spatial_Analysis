#Week 6: Spatial Areal Analysis

library(SpatialEpi)
library(GISTools)
library(rgdal)

data(pennLC)
?pennLC #look up documentation

# Extract the SpatialPolygon info
penn.state.latlong <- pennLC$spatial.polygon
proj4string(penn.state.latlong)
is.projected(penn.state.latlong)

# Convert to UTM zone 17N
# http://spatialreference.org/ref/epsg/3724/
penn.state.utm <- spTransform(penn.state.latlong,
                              CRS("+init=epsg:3724 +units=km"))

# Obtain the smoking rates
smk <- pennLC$smoking$smoking * 100

# Set up a shading object, draw choropleth and legend
par(mar=c(0,2,0,2))
shades <- auto.shading(smk,n=6,cols=brewer.pal(5,'Blues'))
choropleth(penn.state.utm,smk,shades)
choro.legend(px="topright",sh=shades,title='Smoking Uptake (% of popn.)', cex=0.6)


## ----seeding,echo=FALSE,results='hide'-----------------------------------
set.seed(1234)


## ----ransmk,fig.cap='Randomisation of smoking uptake rates',dependson='seeding'----
# Set up the parameters - six plots in 3 rows by 2 cols
# set margins as smaller than usual to allow bigger maps
par(mfrow=c(3,2),mar=c(1,1,1,1)/2)

# Which one will be the real data
real.data.i <- sample(1:6,1) 

# Draw six plots.  Five will be random one will be the real data
for (i in 1:6) {
  if (i == real.data.i) {
    choropleth(penn.state.utm,smk,shades)}
  else {
    choropleth(penn.state.utm,sample(smk),shades)}
}


## ----reveal,dependson='ransmk'-------------------------------------------
real.data.i
par(mfrow=c(1,1))

## ----get neighbor list---------------------------------------------------------------
library(spdep)
penn.state.nb <- poly2nb(penn.state.utm) #default is Queen's case
penn.state.nb


## ----plotnetwork2,fig.cap='Comparison of Neighbouring Counties of Penn State (Rook's vs. Queen's case).'----
# Calculate the Rook's case neighbours
penn.state.nb2 <- poly2nb(penn.state.utm,queen=FALSE)
# Plot the counties in background
plot(penn.state.utm,border='lightgrey')
# Plot the Queen's case neighbourhoods info as a network
plot(penn.state.nb,coordinates(penn.state.utm),add=TRUE,col='blue',lwd=2)
# Now overlay the Rook's case neighbours
plot(penn.state.nb2,coordinates(penn.state.utm),add=TRUE,col='yellow')


## ----nb2lw---------------------------------------------------------------
# Convert the neighbour list to a listw object - use Rook's case...
penn.state.lw <- nb2listw(penn.state.nb2)
penn.state.lw

#view W matrix
penn.state.lw[3]


## ----lagmean,fig.cap='Lagged means of smoking uptake rates'--------------
smk.lagged.means <- lag.listw(penn.state.lw,smk)
choropleth(penn.state.utm,smk.lagged.means,shades)


## ----nbplot,fig.cap='Lagged Mean plot for smoking uptake'----------------
plot(smk,smk.lagged.means,asp=1,xlim=range(smk),ylim=range(smk))
abline(a=0,b=1)
abline(v=mean(smk),lty=2)
abline(h=mean(smk.lagged.means),lty=2)


## ----msp,fig.cap='Lagged Mean plot for smoking uptake - alternative method.'----
par(mar=c(5,5,5,5))
moran.plot(smk,penn.state.lw)


## ----morancomp-----------------------------------------------------------
moran.test(smk,penn.state.lw)

## ----RandomMoran---------------------------------------------------------
moran.mc(smk,penn.state.lw,10000)


## ----Basic Simultaneous Autoregressive (SAR) Model
sar.res <- spautolm(smk~1,listw=penn.state.lw) #constant term for the mean of the predicted value
sar.res

 xxc
## ----getLambdaSESAR------------------------------------------------------
sar.res$lambda.se


## ----getLambdaCISAR------------------------------------------------------
sar.res$lambda + c(-2,2)*sar.res$lambda.se


## ----seedata-------------------------------------------------------------
head(pennLC$data)


## ----makedata1-----------------------------------------------------------
library(plyr)
totcases <- ddply(pennLC$data,c("county"),numcolwise(sum))
head(totcases)
totcases <- transform(totcases,rate=10000*cases/population)
head(totcases)

# Check the distribution of rates
boxplot(totcases$rate,horizontal=TRUE,
        xlab='Cancer Rate (Cases per 10,000 Popn.)')


## ----makemodels----------------------------------------------------------
sar.mod <- spautolm(rate~smk,listw=penn.state.lw,
                    weight=population,data=totcases)
summary(sar.mod)

sar.mod2 <- spautolm(rate~sqrt(smk),listw=penn.state.lw,
                    weight=population,data=totcases)
summary(sar.mod2)


