#Week 3: Using R as a GIS


library(GISTools)
data(tornados)
ls()
#to clear all objects
#rm(list=ls()) 

#SPATIAL INTERSECTION using gIntersection

par(mar=c(0,0,0,0))
plot(us_states)

summary(torn) #see projection & attributes of torn
#torn and torn2 describe locations of tornados between 1950 and 2004
#us_states2 is projected in meters

plot(torn, add=T,pch=1, col="red", cex=0.4)
plot(us_states,add=T) #this overlays the state boundary map on top of the tornados 

index <- us_states@data$STATE_NAME=="Texas"|
  us_states@data$STATE_NAME=="New Mexico"|
  us_states@data$STATE_NAME=="Oklahoma"|
  us_states@data$STATE_NAME=="Arkansas"

AoI <- us_states[index, ]

plot(AoI)
plot(torn,add=T,pch=1, col="red", cex=0.4) #notice R is plotting all tornado records

AoI.torn <- gIntersection(AoI, torn, byid=TRUE) #byid preserves all attributes in data frame
par(mar=c(0,0,4,0))
plot(AoI,add=T)
plot(AoI.torn,add=T,pch=1,col="red")
title(main="Tornados in TX, NM, OK, AR", font.main=2)

head(data.frame(AoI.torn)) #first number is state record; second number is tornado record

#BUFFERS USING gBuffer

proj4string(us_states2)
AoI<-us_states2[us_states2@data$STATE_NAME=="Texas",]
AoI.buf <-gBuffer(AoI, width=25000)

par(mar=c(0,0,0,0))
plot(AoI)
#plot(torn2, add=T, col="red")
plot(AoI.buf, add=T, border="blue")

#use the buffer map to find tornados within 25 squared km of TX border
AoI.torn2 <- gIntersection(AoI.buf, torn2, byid=TRUE)#byid preserves all attributes in data frame
plot(AoI.torn2,add=T,col="red")


#MERGING SPATIAL FEATURES USING gUnaryUnion

AoI.merge <- gUnaryUnion(us_states) #merge all polygons together
par(mar=c(0,0,0,0))
plot(us_states, border="darkgreen", lty=3) #make the state map a bit different than usual
plot(AoI.merge,add=T) #overlay outline map
#plot(torn, add=T, pch=1, col="red", cex=0.4)

#POINT AND AREA CALCULATIONS

#point in polygon

torn.count<-poly.counts(torn, us_states)
torn.count

#function reminder

#WRITE YOUR OWN FUNCTION 

points.count<-function(x,y){
  sum(poly.counts(x,y))
}

points.count(torn,us_states)



#remember to write this function out to a new R script so we can reuse it.




#Area Calculations

proj4string(us_states2)

poly.areas(us_states2)/(1000^2) #square kilometers



#EXERCISE 1: POINT & AREA ANALYSIS USING POISSON REGRESSION MODEL

rm(list=ls()) #clear objects
dev.off() #clear plot

data(newhaven)
ls()

plot(blocks)
plot(breach,add=T,col="red")

proj4string(blocks) #check the unit of measurement. Should say ft.

ft2miles(ft2miles(poly.areas(blocks))) #convert blocks to squared miles

#calculate breach of peace density for each block
blocks@data$densities <- poly.counts(breach,blocks)/ft2miles(ft2miles(poly.areas(blocks)))

#choropleth map
par(mar=c(2,0,2,0))
density.shades <- auto.shading(blocks@data$densities, col=brewer.pal(5, "Oranges"), cutter=rangeCuts)
choropleth(blocks, blocks@data$densities, shading=density.shades)
choro.legend(524733,167261, density.shades)
title("Incidents per Squared Mile in New Haven")

#Is there a relationship between rate of owner occupied homes and breach density?
#Is there a relationship between rate of renter occupied homes and breach density?

cor(blocks@data$P_OWNEROCC, blocks@data$densities) #find correlation coefficient
cor(blocks@data$P_RENTROCC, blocks@data$densities) 

dev.off() #clear current plot

plot(blocks@data$P_OWNEROCC, blocks@data$densities)



#SOURCE IN YOUR FUNCTION

source("C:/Users/PhamX/Dropbox/Spring_2018/Spatial_Analysis/Week_3/function.R")


points.count(breach,blocks)
#Can we make the function even fancier??

points.count <- function(x,y){
  pts<-poly.counts(x,y)
  pts.df<-as.data.frame(pts)
  point.count<-(sum(pts.df))
  area.count<-nrow(pts.df)
  mean <- point.count/area.count
  print(cat("Total points= ",point.count,"\n"))
  print(cat("Total areas= ",area.count,"\n"))
  print(cat("Mean= ",mean,"\n"))
}

points.count(breach, blocks)



#Don't forget to save the revised function for later use.

#What is the probability that a block has 0 breach? 1? 2? 3? And so on...

#Poisson distribution:http://homepage.divms.uiowa.edu/~mbognar/applets/pois.html

#Poisson regression: http://data.princeton.edu/wws509/notes/c4.pdf
#Poisson regression: https://onlinecourses.science.psu.edu/stat504/node/168

blocks@data$total.breaches <- poly.counts(breach,blocks)
blocks@data$area <- ft2miles(ft2miles(poly.areas(blocks))) #squared kilometers

model.1 <- glm(total.breaches ~ P_OWNEROCC, data=blocks@data, offset=log(area), family=poisson)

summary(model.1)

#model.1$coefficients
exp(model.1$coefficients[[2]])

#write it as a percentage
100*(1-exp(model.1$coefficients[[2]]))
#1% increase in owner occupied houses reduces breach incidents/block by 0.9695
#or (1-0.97)*100% = 3% less

#standardized residuals
s.resids <- rstandard(model.1)
hist(s.resids)
#empirical rule: http://www.statisticshowto.com/empirical-rule-2/

resid.shades <- shading(c(-2,2),c("lightblue","grey","darkblue"))
par(mar=c(0,0,2,0))
choropleth(blocks, s.resids, resid.shades)
title("Standardized Residuals of Poisson Model")


#CALCULATING DISTANCES USING gDistance()

data(newhaven)

places
proj4string(places)
proj4string(places) <- CRS(proj4string(blocks)) #assign a projection
proj4string(places)

centroids <- gCentroid(blocks, byid=T, id=rownames(blocks))
distances <- ft2miles(gDistance(places, centroids, byid=T))

distances #see the matrix

min.dist<-as.vector(apply(distances,1,min))

access<-min.dist<1a


#EXERCISE 2: DISTANCE ANALYSIS 

ethnicity <- as.matrix(data.frame(blocks[,14:18])/100) #convert to ratio instead of percent
ethnicity

#multiply to total pop to get actual population by ethnicity
ethnicity <- apply(ethnicity,2,function(x) #apply to margin=2 means to do it to each column
{
  x*blocks@data$POP1990
  }) 

ethnicity <- matrix(as.integer(ethnicity), ncol=5)

colnames(ethnicity) <-c("White", "Black", "Native American", "Asian", "Other")

#crosstabulates access to green space by population/ethnicity 

mat.access.tab <- xtabs(ethnicity~access) 

greenspace.df <- as.data.frame(mat.access.tab)

colnames(greenspace.df) <- c("Access","Ethnicity","Freq")

model.2 <- glm(Freq~Access*Ethnicity, data=greenspace.df, family=poisson)

summary(model.2)

model.coef<-summary(model.2)$coef

options(scipen = 999)
tab<-100*(exp(model.coef[,1])-1)
tab

#Narrow to Interaction Terms
tab2<-tab[7:10]

##refer to pp.87-88
mosaicplot(t(mat.access.tab),xlab='',ylab='Access to Green Space',
           main='Mosaic Plot of Access', shade=TRUE, las=3, cex=0.8)
