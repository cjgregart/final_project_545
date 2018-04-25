## Final project


library(ggplot2)
library(lubridate)
library(maps)
library(ggmap)
library(dplyr)

data = read.csv("SFrestaurants.csv")

data2 = data %>%
  filter(business_address != "Off The Grid")

# filter out observations that have "Off the Grid" for address; these are generally food trucks or otherwise mobile entities

data2$address2 = paste(data2$business_address, 
                        data2$business_city, 
                        data2$business_state, 
                        sep = ", ")


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


write.csv(data2, 
          file = "SFrestaurants2.csv",
          row.names = F)


View(data2)


## 4/25/18 (CJ) next steps: decide how to handle multiple observations
## for the same restaurant (diff days, diff vioations on same day)...
## ... do we pick the worst offense?  or the most recent? or some average?





# Plot map

SF = qmap("San Francisco",
                  zoom = 12,
                  maptype = "roadmap",
                  color = "bw")

SF

View(head(data))

SF + geom_point(data = data, 
                aes(x= business_longitude, 
                    y = business_latitude),
                alpha = 0.05,
                color = "darkred",
                size = 2)


#1 set colors to risk_category (dark = bad, green = good) 
#2 set size or color or something to inspection_score; or make levels (e.g. 0-10, 10-20, ... 90-100)
#3 look up lat/long of blank rows by address to fill in missing 20,000 values
    # a. skip rows with "off the grid" for address, or look up by business name
    # b. skip rows that already have lat/lon; roughly 25k remaining
    # c. at 2,500 per person per day, should only take 2 days worth of running queries by each of us

    # this accomplishes some "data cleaning"


###### testing 

dataB = data[1:20,]

dataC = dataB %>%
  filter(business_address != "Off The Grid")

dataC$address2 <- paste(dataC$business_address, 
                        dataC$business_city, 
                        dataC$business_state, 
                        sep = ", ")

for(i in 1:nrow(data3))
{
  if(is.na(dataC$business_latitude[i]))
  {
    result <- geocode(dataC$address2[i], 
                      output = "latlon", 
                      source = "google")
    dataC$business_longitude[i] <- as.numeric(result[1])
    dataC$business_latitude[i] <- as.numeric(result[2])
  }
}


write.csv(dataC, 
          file = "dataC.csv",
          row.names = F)
