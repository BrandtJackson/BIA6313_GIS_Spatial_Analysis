library(rgdal)
library(GISTools)

county <- readOGR(".","County_2010Census_DP1")

plot(county, main="US Map")

head(county@data)

# See link for complete list of FIPS State Code: https://en.wikipedia.org/wiki/Federal_Information_Processing_Standard_state_code

county@data$STATE <- substr(county@data$GEOID10,1,2)

head(county@data)

mo<-county[county@data$STATE==29,]

head(mo@data)

ks <-county[county@data$STATE==20,]

ia <- county[county@data$STATE==19,]

class(mo)
class(ks)
class(ia)

plot(mo)
plot(ks)
plot(ia)

tri_state<-rbind(ia,ks,mo)
plot(tri_state)

# use locator() to find pixel positions on plot
# use par(mar()) to resize plot margin area. For more information, see link: 
#https://www.r-bloggers.com/setting-graph-margins-in-r-using-the-par-function-and-lots-of-cow-milk/

# run this next line to remove all census data attached to the tri-state shapefile. 
tri_state_shapefile <- tri_state[,1:2]
head(tri_state_shapefile@data)