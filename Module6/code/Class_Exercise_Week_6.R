#WEEK 5 CLASS EXERCISE: SPATIAL INTERPOLATION OF HOUSING PRICES IN ZIP CODE 64110

#Packages you will need:
#install.packages("ggmap")
#install.packages("tigris")
#install.packages("sp")
#install.packages("mapview")
#install.packages("gstat")
#install.packages("maptools")
#install.packages("dismo")


#Step 1: Bring in sample control points 
houses <- read.csv("https://www.dropbox.com/s/y0sbrcyd7egm6ha/houses.csv?dl=1")

View(houses)

library(ggmap)
houses$address<-as.character(houses$address)
output <- geocode(houses$address, output="latlona", source="dsk")
names(output)
output.2<-output[,c(1:2)]
houses.2 <-cbind(houses,output.2)

#price in thousand of dollars
houses.2$price <- houses.2$price/1000

#Step 2: Get census tracts for zip code 64110

library(tigris)

tracts <- tracts("29", county="095") #Jackson County, MO

index <- c(tracts@data$GEOID=="29095006700"|
             tracts@data$GEOID=="29095006500"|
             tracts@data$GEOID=="29095016900"|
             tracts@data$GEOID=="29095006600"|
             tracts@data$GEOID=="29095006300"|
             tracts@data$GEOID=="29095007400"|
             tracts@data$GEOID=="29095007500"|
             tracts@data$GEOID=="29095008200"|
             tracts@data$GEOID=="29095008100"|
             tracts@data$GEOID=="29095008000"|
             tracts@data$GEOID=="29095008600")


zip <- tracts[index,]

par(mar=c(0,0,0,0))
plot(zip)

#Step 3: Creating SpatialPointsDataFrame

library(sp)
coordinates(houses.2)<- ~lon+lat #transform to spatialpointsdataframe
proj4string(houses.2) <- CRS(proj4string(zip))

plot(zip)
plot(houses.2,add=TRUE,col="green")


# Step 4: Set a projection

zip <-spTransform(zip, CRS("+init=EPSG:2817"))
houses.2<-spTransform(houses.2,CRS("+init=EPSG:2817"))

write.csv(houses.2,"C:/Users/PhamX/Dropbox/Spring_2018/Spatial_Analysis/Week_5/houses2.csv",row.names=FALSE)

#Codes below are taken from "Intro to GIS and Spatial Analysis" by Manual Gimond
#https://mgimond.github.io/Spatial/interpolation-in-r.html


#Set the bbox of houses.2 to the same as the census tracts

houses.2@bbox<-zip@bbox

#Map out the control points

library(tmap)
tm_shape(zip) + tm_polygons() +
  tm_shape(houses.2) +
  tm_dots(col="price", palette = "RdBu", auto.palette.mapping = FALSE,
          title="Sampled House Price \n(in 1000's)", size=0.7) +
  tm_text("price", just="left", xmod=.5, size = 0.7) +
  tm_legend(legend.outside=TRUE)



#Proximity Polygons

library(spatstat)  # Used for the dirichlet tessellation function
library(maptools)  # Used for conversion from SPDF to ppp
library(raster)    # Used to clip out thiessen polygons

#Rename the SpatialPointsDataFrame object
P<-houses.2

# Create a tessellated surface
th  <-  as(dirichlet(as.ppp(P)), "SpatialPolygons")

# The dirichlet function does not carry over projection information
# requiring that this information be added manually
proj4string(th) <- proj4string(P)

# The tessellated surface does not store attribute information
# from the point data layer. We'll use the over() function (from the sp
# package) to join the point attributes to the tesselated surface via
# a spatial join. The over() function creates a dataframe that will need to
# be added to the `th` object thus creating a SpatialPolygonsDataFrame object
th.z     <- over(th, P, fn=mean)
th.spdf  <-  SpatialPolygonsDataFrame(th, th.z)

# Finally, we'll clip the tessellated  surface to the boundaries
th.clp   <- raster::intersect(zip,th.spdf)

# Map the data
tm_shape(th.clp) + 
  tm_polygons(col="price", palette="RdBu", auto.palette.mapping=FALSE,
              title="Predicted House Price \n(in 1000's)") +
  tm_legend(legend.outside=TRUE)



#Inverse Distance Weights

library(gstat) # Use gstat's idw routine
library(sp)    # Used for the spsample function

# Create an empty grid where n is the total number of cells
grd              <- as.data.frame(spsample(P, "regular", n=50000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

# Add P's projection information to the empty grid
proj4string(grd) <- proj4string(P)

# Interpolate the grid cells using k's value
P.idw <- gstat::idw(price ~ 1, P, newdata=grd, idp=1)

# Convert to raster object then clip to shapefile
r       <- raster(P.idw)
r.m     <- mask(r, zip)

# Plot
tm_shape(r.m) + 
  tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
            title="Predicted House Price \n(in 1000's)") + 
  tm_shape(P) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)



###Kriging------------------------------------------

#Enter codes for chosen experimental variogram and the fitted model
###########################
evgm <- variogram()
plot(evgm)
fvgm<-fit.variogram()
plot(evgm, fvgm)
############################

# Perform the krige interpolation 
dat.krg <- krige( price~1, P, grd, fvgm)

# Convert kriged surface to a raster object for clipping
r <- raster(dat.krg)
r.m <- mask(r, zip)

# Plot the map
tm_shape(r.m) + 
  tm_raster(n=10, palette="RdBu", auto.palette.mapping=FALSE, 
            title="Predicted House Price \n(in 1000's)") +
  tm_shape(P) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)



