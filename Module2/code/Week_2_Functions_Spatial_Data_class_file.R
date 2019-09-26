library(GISTools)
data(georgia)
ls()
georgia.polys[[1]]
plot(georgia.polys[[1]], asp=1, type='l') #Appling County


plot(c(939200,1419420),c(905510,1405900),asp=1,type='n') #polygon function draws polygons next to each other
lapply(georgia.polys,polygon)

#use invisible function to keep the polygon function from returning NULL values.
#invisible(lapply(georgia.polys,polygon)) 

# Can we get at the same answer without being given the bounding box?

poly<-georgia.polys[[1]]
min(poly[,1]) #most eastern point of Appling County
#See page 120 in text for the remaining most western, southern, and northern points

# Can we put what we just did above in a function?
most.eastern <- function(x){
  return(min(x[,1]))
}

#Use lapply so it does it for every county

most.eastern.list<-lapply(georgia.polys, function(x)
{return(min(x[,1]))
})
#notice the output is a list

#use unlist() to make it a vector
unlist(most.eastern.list)

#take the min() of that to get the minimum eastern point
min(unlist(most.eastern.list))


#how do we put all this together in our existing function?

most.eastern.point <- function(x){
  most.eastern.list<-lapply(x, function(x)
    return(min(x[,1])))
  return(min(unlist(most.eastern.list)))
}

#Repeat it for all other points
#Use the table on page 120 to find the other extreme points.


#Now that we have all the min points for the bounding box, we can use it to draw a map--county by county.

draw.polys <- function(x){
  ew<-c(most.eastern.point(x), most.western.point(x))
  ns<-c(most.southern.point(x), most.northern.point(x))
  plot(ew,ns,asp=1,type='n',xlab='',ylab='',xaxt='n',yaxt='n',bty='n') #this switches off the axes, labels, and box around the graph
  invisible(lapply(x,polygon))
}

draw.polys(georgia.polys)


#Now add in a shaded map

head(georgia2$PctRural)

classifier <- factor(ifelse(georgia$PctRural > 50, "rural","urban"))

draw.polys.2 <- function(x){
  
  fill.cols <- vector(mode="character", length=length(classifier)) 
  #this creates a character vector with 
  #the same length as the number of polygons, containing only empty strings.
  
  fill.cols[classifier=="urban"] <- "yellow"
  fill.cols[classifier=="rural"] <- "dark green"
  
  ew<-c(most.eastern.point(x), most.western.point(x))
  ns<-c(most.southern.point(x), most.northern.point(x))
  plot(ew,ns,asp=1,type='n',xlab='',ylab='',xaxt='n',yaxt='n',bty='n')
  invisible(mapply(polygon,x, col=fill.cols))
}

draw.polys.2(georgia.polys)


#Let's apply it to a non-textbook data set.

#Bring in the county shapefile from the Census

library(rgdal)
library(GISTools)

county <- readOGR("C:/Users/PhamX/Courses/Spring_2018/Spatial_Analysis/Week_1","County_2010Census_DP1")

geo <- county@polygons[[1]]@Polygons[[1]]@coords #x&y coordinates for first county

#getting the coordinates for all counties
List <- list()
for (i in 1:3221){
  geo <- county@polygons[[i]]@Polygons[[1]]@coords
  List[[length(List)+1]] = geo #write out the coordinates
}

draw.polys(List) #find the ew/ns coordinates for each county & plot it

county@data$single_female_household<-round(((county@data$DP0130008/county@data$DP0130001)*100),digits=2)
classifier <- factor(ifelse(county$single_female_household > 11.58, "above national average","less than or equal to national average"))

draw.polys.3 <- function(x){
  
  fill.cols <- vector(mode="character", length=length(classifier)) 
  #this creates a character vector with 
  #the same length as the number of polygons, containing only empty strings.
  
  fill.cols[classifier=="above national average"] <- "red"
  fill.cols[classifier=="less than or equal to national average"] <- "blue"
  
  ew<-c(most.eastern.point(x), most.western.point(x))
  ns<-c(most.southern.point(x), most.northern.point(x))
  plot(ew,ns,asp=1,type='n',xlab='',ylab='',xaxt='n',yaxt='n',bty='n')
  invisible(mapply(polygon,x, col=fill.cols))
}

draw.polys.3(List)


# Can we approach this from a state-to-state basis?

county@data$STATE <- substr(county@data$GEOID10,1,2)

for (i in 1:56)
{
  name <- paste("state",i, sep=".")
  assign(name, county[county@data$STATE==i,])
}


List <- list()
for (i in 1:115){
  geo <- state.29@polygons[[i]]@Polygons[[1]]@coords
  List[[length(List)+1]] = geo
}


classifier <- factor(ifelse(state.29$single_female_household > 11.58, "above national average","less than or equal to national average"))

draw.polys.3(List)
