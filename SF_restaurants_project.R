## Final project

data = read.csv("SFrestaurants.csv")


library(ggplot2)
library(lubridate)
library(maps)
library(ggmap)

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