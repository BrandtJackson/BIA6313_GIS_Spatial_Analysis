library(GWmodel)
data(EWHP)
head(ewhp)

#convert purchased prices from pounds to thousand of pounds for easier reading & mapping

ewhp$PurPrice <- ewhp$PurPrice/1000 

#creating a spatialpointsdataframe

houses.spdf <- SpatialPointsDataFrame(ewhp[,1:2],ewhp) #these are the eastings & northings coordinates

#get the UK boundary shapefile & plot out the sampled houses
par(mfrow=c(1,1))
par(mar=c(1,1,1,1))
data(EWOutline) 
plot(ewoutline)



#ENTER YOUR CODE TO CALCULATE THE FOLLOWING SUMMARY STATISTICS FOR ewhp$PurPrice

#Measures of central tendency: mean, median





#Measures of dispersion: standard deviation, range, and interquartile range (robust)
#Note: R does not have a range function. Use max() - min()
#Note: R does not have an interquartile range function. Use the quantile() function. 
#Look up the help page for quantile. 






#Shape: skewness & histogram
#Note: use skewness() in package e1071







#Scatter plot between ewhp$FlrArea and ewhp$PurPrice
#Pearson's correlation coefficient between ewhp$FlrArea and ewhp$PurPrice
