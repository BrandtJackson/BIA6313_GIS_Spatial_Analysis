setwd("")

crimes<-read.csv("KCPD_Crime_Data_2017.csv")

library(tidyverse)
homicides<-filter(crimes,Description=="HOMICIDE/Non Neglige") 

library(stringr)
homicides$Address <- str_replace(homicides$Address, "TE", "TER") #weird quirk in data

homicides$Complete.Address <- paste(homicides$Address,",",homicides$City,",",homicides$Zip.Code)


library(ggmap)
output <- geocode(homicides$Complete.Address, output="latlona")
homicides <-cbind(homicides,output)
homicides<-homicides[complete.cases(homicides$lat),]

coordinates(homicides)<- ~lon+lat #transform to spatialpointsdataframe
zero<-zerodist(homicides)
length(unique(zero[,1]))
#homicides<-as(remove.duplicates(homicides),'SpatialPoints')
homicides<-remove.duplicates(homicides)


library(rgdal)
kcmo <- readOGR(".","geo_export_7b4dbae7-9db3-4958-9842-f1c7b5c01696")
proj4string(kcmo)
proj4string(homicides) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") #assigns CRS
kcmo <- spTransform(kcmo, CRS("+init=EPSG:2817"))
homicides <- spTransform(homicides, CRS("+init=EPSG:2817"))

plot(kcmo, main="Homicides in Kansas City, MO")
plot(homicides,add=TRUE, col="red")

#writeOGR(kcmo, dsn=".", layer='kcmo', driver="ESRI Shapefile")
#writeOGR(homicides, dsn=".", layer='homicides', driver="ESRI Shapefile")
