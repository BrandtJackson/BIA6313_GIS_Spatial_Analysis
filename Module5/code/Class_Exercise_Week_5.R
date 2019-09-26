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

#Digression 

library(mapview)
m1<-mapView(zip) #overlay census tracts shapefile on an interactive map
h1<-mapView(houses.2) #overlay houses on an interactive map

m1+h1 #show both layers

# Step 4: Set a projection

zip <-spTransform(zip, CRS("+init=EPSG:3857"))
houses.2<-spTransform(houses.2,CRS("+init=EPSG:3857"))



