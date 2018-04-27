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

## convert dates from excel format

data3$inspection_date = as.Date(data3$inspection_date, origin = "1899-12-30")


# filter out observations that have "Off the Grid" for address; these are generally food trucks or otherwise mobile entities
# filter out obviously wrong lat/lon (some of geocode's results were in cities like New York, Denver, Chicago); these limits on lat/lon make sure the results are in the bay area


data4 = data3 %>%
  filter(business_address != "Off The Grid",
         between(business_latitude, 37, 38),
         between(business_longitude, -123, -122))



## 4/25/18 (CJ) next steps: decide how to handle multiple observations
## for the same restaurant (diff days, diff vioations on same day)...
## ... do we pick the worst offense?  or the most recent? or some average?





## Plot map

SF = qmap("San Francisco",
                  zoom = 12,
                  maptype = "roadmap",
                  color = "bw")

SF


SF + geom_point(data = data4, 
                aes(x= business_longitude, 
                    y = business_latitude),
                alpha = 0.05,
                color = "darkred",
                size = 2)


#1 set colors to risk_category (dark = bad, green = good) 
#2 set size or color or something to inspection_score; or make levels (e.g. 0-10, 10-20, ... 90-100)



data5 = data4 %>%
  filter(data4$risk_category != "")


risk_hist = ggplot(data5, aes(x = data5$risk_category)) +
  geom_histogram(stat = "count") +
  scale_x_discrete(limits = c("Low Risk", "Moderate Risk", "High Risk")) +
  xlab("") +
  ylab("")+
  ggtitle("Distribution of violation risk levels")

risk_hist  


# Distribution of scores

data_scores = data4 %>%
  filter(!is.na(data4$inspection_score))

data_scores2 = data_scores %>%
  group_by(gr = cut(data_scores$inspection_score, 
                       breaks = c(39,49,59,69,79,89,99,100)))


data_scores3 = data_scores2 %>%
  group_by(gr) %>%
  summarise(n = n())

score_dist = ggplot(data_scores3, 
                    aes(x = data_scores3$gr,
                        y = data_scores3$n
                      
                        
                        )) +
  geom_col() +
  geom_text(aes(label = n),vjust = -0.25)+
  scale_y_continuous(limits = c(0,15000),
                     breaks = seq(0,15000,5000))+
  xlab("Score (0-100)") +
  ylab("Count")+
  ggtitle("Distribution of inspection scores") 


score_dist
