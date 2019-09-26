#Calculate geographically weighted summary statistics 
#for PurPrice & FlrArea using moving window of 50 km

library(GWmodel)
data(EWHP)
head(ewhp)
data(EWOutline) 

#Convert purchased prices from pounds to thousand of pounds for easier reading & mapping
ewhp$PurPrice <- ewhp$PurPrice/1000 

#creating a spatialpointsdataframe
houses.spdf <- SpatialPointsDataFrame(ewhp[,1:2],ewhp) #these are the eastings & northings coordinates


#Calculate geographically weighted summary statistics 
#using a moving window of 50 km
localstats1 <- gwss(houses.spdf,vars=c("PurPrice","FlrArea"),bw=50000)
localstats.df <- as.data.frame(localstats1$SDF)


## Function to create geographically weighted summary statistics
require(RColorBrewer)
quick.map <- function(spdf,var,legend.title,main.title) {
  x <- spdf@data[,var]
  cut.vals <- pretty(x)
  x.cut <- cut(x,cut.vals)
  cut.levels <- levels(x.cut)
  cut.band <- match(x.cut,cut.levels)
  colors <- brewer.pal(length(cut.levels),'Reds')
  par(mar=c(1,1,1,1))
  plot(ewoutline,col='grey85')
  title(main.title)
  plot(spdf,add=TRUE,col=colors[cut.band],pch=16)
  legend('topleft',cut.levels,col=colors,pch=16,bty='n',title=legend.title)
}


## quick.map(gwss.object,variable.name,legend.title,main.map.title)

# EXAMINING CENTRAL TENDENCY

## ----gwmeanmap,fig.cap="Geographically Weighted Mean"--------------------
quick.map(localstats1$SDF,"PurPrice_LM",
          "1000's Uk Pounds","Geographically Weighted Mean")

## ----gwmedian,fig.cap="Geographically Weighted Medians"------------------
localstats2 <- gwss(houses.spdf,vars=c("PurPrice","FlrArea"),
                    bw=50000,quantile=TRUE)
quick.map(localstats2$SDF,"PurPrice_Median","1000\'s UK Pounds",
          "Geographically Weighted Median House Price")


# EXAMINING DISPERSION

## ----sdsk,fig.cap="Geographically Weighted Standard Deviation and skewness"----
par(mfrow=c(1,2))
quick.map(localstats1$SDF,"PurPrice_LSD",
          "1000's Pounds","Local Standard Deviation")
quick.map(localstats2$SDF,"PurPrice_IQR","1000\'s UK Pounds",
          "Geographically Weighted Interquartile Range")


# EXAMINING PEARSON'S CORRELATION

## ----qmrho,fig.cap="Geographically Weighted Pearson Correlation"---------
quick.map(localstats1$SDF,"Corr_PurPrice.FlrArea",
          expression(rho),"Geographically Weighted Pearson Correlation")





