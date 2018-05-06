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


data6 = data4


data6 = data6 %>%
  filter(data6$inspection_score != "NA")

data6$gr = cut(data6$inspection_score, 
               breaks = c(39,49,59,69,79,89,99,100),
               labels = c(40,50,60,70,80,90,100))

View(data6)

SF + geom_point(data = data6, 
                aes(x= business_longitude, 
                    y = business_latitude,
                fill = data6$gr),
                alpha = 1,
                
                shape = 21,
                size = 2
                ) +
  scale_fill_manual(values = c("darkred",
                               "red",
                               "darkorange",
                               "orange",
                               "yellow",
                               "green",
                               "darkgreen"))

# map of low scores


data_lowIS = data6 %>%
  filter(data6$inspection_score < 60)

  # selected threshold = 60

SF + geom_point(data = data_lowIS, 
                aes(x= business_longitude, 
                    y = business_latitude,
                    fill = data_lowIS$gr),
                alpha = 0.1,
                shape = 21,
                size = 2
) +
  scale_fill_manual(values = c("darkred",
                               "red",
                               "darkorange",
                               "orange",
                               "yellow",
                               "green",
                               "darkgreen"))

# vermin map

data_vermin = data6 %>%
  filter(data6$violation_description == "High risk vermin infestation")


SF + geom_point(data = data_vermin, 
                aes(x= business_longitude, 
                    y = business_latitude,
                    fill = "darkred"),
                alpha = 0.4,
                shape = 21,
                size = 2
)








# bar chart of distribution of violations in each risk category

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





#### TO DO

# 1.  make a shiny app for map of violations, 
#     where you can select/filter by: 
#       -violation type (e.g. "High risk vermin investation")
#       -violate risk level (low, med, high)
#       -violation score range (0-100)
#       -inspection date range (e.g. 2016 - 2017)

# 2.  group all inspections per location (business name) 
#     & date into single observation (combine name + date)
#     and include score, etc. 
