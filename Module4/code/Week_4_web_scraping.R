library(rvest)
library(readr)

#You will also need to add SelectorGadget to your bookmarks: 
#http://selectorgadget.com/


#Find the site

url <- read_html("https://www.beeradvocate.com/place/list/?start=0&&c_id=US&s_id=KS&brewery=Y&sort=name")

results_name <- url %>% html_nodes(xpath='//a//b') %>% html_text()

results_rating <- url %>% html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "hr_bottom_light", " " )) and (((count(preceding-sibling::*) + 1) = 2) and parent::*)]//b')%>%html_text()


results_address <- url %>% 
  html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "hr_bottom_dark", " " )) and (((count(preceding-sibling::*) + 1) = 1) and parent::*)]')%>%
  html_text()






url <- read_html("https://www.beeradvocate.com/place/list/?start=20&&c_id=US&s_id=KS&brewery=Y&sort=name")

results_name_2 <- url %>% html_nodes(xpath='//a//b') %>% html_text()

results_rating_2 <- url %>% html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "hr_bottom_light", " " )) and (((count(preceding-sibling::*) + 1) = 2) and parent::*)]//b')%>%html_text()

results_address_2 <- url %>% 
  html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "hr_bottom_dark", " " )) and (((count(preceding-sibling::*) + 1) = 1) and parent::*)]')%>%
  html_text()



url <- read_html("https://www.beeradvocate.com/place/list/?start=40&&c_id=US&s_id=KS&brewery=Y&sort=name")

results_name_3 <- url %>% html_nodes(xpath='//a//b') %>% html_text()

results_rating_3 <- url %>% html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "hr_bottom_light", " " )) and (((count(preceding-sibling::*) + 1) = 2) and parent::*)]//b')%>%html_text()

results_address_3 <- url %>% 
  html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "hr_bottom_dark", " " )) and (((count(preceding-sibling::*) + 1) = 1) and parent::*)]')%>%
  html_text()


#Clean the brewery names
results_name <- results_name[-1]#Remove "Extreme Beer Fest"
results_name_2 <- results_name_2[-1]#Remove "Extreme Beer Fest"
results_name_3 <- results_name_3[-1]#Remove "Extreme Beer Fest"

name <- c(results_name, results_name_2, results_name_3)
name <- as.data.frame(name)
name.final <- as.data.frame(name[name$name!="Become a BeerAdvocate Supporter",])
colnames(name.final) <- "brewery_name"


#Clean address

address <- c(results_address, results_address_2, results_address_3)

address <- as.data.frame(address)
address$address <- as.character(address$address)

library(tidyr)
address.final <- address %>% separate(address, c("address", "phone_number"), sep = "United States")


#Clean rating 

rating <- c(results_rating, results_rating_2, results_rating_3)
rating.final <- as.data.frame(rating)

rating.final$review_score <-as.numeric(as.character(rating.final$rating))
rating.final$rating <- NULL

#Combine
BeerAdvocate_KS <- cbind(name.final, address.final, rating.final)
BeerAdvocate_KS <- BeerAdvocate_KS[-16,] #remove duplicate record


#Geocode
library(ggmap)

out <- geocode(location = BeerAdvocate_KS$address, output="latlon", source="dsk")
out[25,1]<--97.34159
out[25,2]<- 37.69576

BeerAdvocate_KS <- cbind(BeerAdvocate_KS,out)

write.csv(BeerAdvocate_KS,"C:/Users/PhamX/Dropbox/Spring_2018/Spatial_Analysis/Week_8/beeradvocate_ks.csv")

#Mapping with Leaflet
#Code below adapted from Jasmine Dumas
#<https://trendct.org/2016/03/18/tutorial-web-scraping-and-mapping-breweries-with-import-io-and-r/>

library(leaflet)


# create the pop-up info
BeerAdvocate_KS$pop_up_content <- paste(sep="</br>",BeerAdvocate_KS$brewery_name,"</br>",
                                        BeerAdvocate_KS$address, "</br>",
                                        BeerAdvocate_KS$phone_number, "</br>",
                                        paste("Avg. Review Score: ", BeerAdvocate_KS$review_score,"/5.0"))


# add colors corresponding to the average beer ratings  
pal <- colorNumeric("YlOrRd", BeerAdvocate_KS$review_score, n = 6) 

map <- leaflet(data = BeerAdvocate_KS) %>% 
  addTiles() %>%
  setView(lng =-94.57157, lat=39.81735, zoom = 5) %>% 
  addCircleMarkers(~lon, ~lat, popup = ~as.character(pop_up_content), color = ~pal(review_score)) %>%
  addLegend("topright", pal = pal, values = ~review_score,
            title = "Avg. Review Score",
            opacity = 1
  )
map







