### Final project - 545 Data Visualization
### SF Restaurants - Health Inspections

## Load libraries

library(ggplot2)
library(lubridate)
library(maps)
library(ggmap)
library(dplyr)


data2 = read.csv("SFrestaurants.csv")

## Data cleaning

data2$address2 = paste(data2$business_address, 
                       data2$business_city, 
                       data2$business_state, 
                       sep = ", ")


# fill in all missing coordinates

for(i in 1:nrow(data2))
{
  if(is.na(data2$business_latitude[i]))
  {
    result <- geocode(data2$address2[i], 
                      output = "latlon", 
                      source = "google")
    data2$business_longitude[i] <- as.numeric(result[1])
    data2$business_latitude[i] <- as.numeric(result[2])
  }
}


# save as csv to be read

write.csv(data2, 
          file = "SFrestaurantsData.csv",
          row.names = F)





## now that data is clean...


data3 = read.csv("SFrestaurantsData.csv")


# filter out observations that have "Off the Grid" for address; these are generally food trucks or otherwise mobile entities
# filter out obviously wrong lat/lon (some of geocode's results were in cities like New York, Denver, Chicago); these limits on lat/lon make sure the results are in the bay area


data4 = data3 %>%
  filter(business_address != "Off The Grid",
         between(business_latitude, 37, 38),
         between(business_longitude, -123, -122))

View(data4)



## 4/25/18 (CJ) next steps: decide how to handle multiple observations
## for the same restaurant (diff days, diff vioations on same day)...
## ... do we pick the worst offense?  or the most recent? or some average?





## Plot map

SF = qmap("San Francisco",
                  zoom = 12,
                  maptype = "roadmap",
                  color = "bw")

SF

View(head(data))

SF + geom_point(data = data4, 
                aes(x= business_longitude, 
                    y = business_latitude),
                alpha = 0.05,
                color = "darkred",
                size = 2)


#1 set colors to risk_category (dark = bad, green = good) 
#2 set size or color or something to inspection_score; or make levels (e.g. 0-10, 10-20, ... 90-100)



