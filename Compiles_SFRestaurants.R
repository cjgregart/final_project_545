## Final Documented R Code - SF Restaurants

 
# Load All Libraries 


library(ggplot2)
library(lubridate)
library(maps)
library(ggmap)
library(dplyr)
library(corrplot)
library(psych)
library(rpart)
library(wordcloud)
library(tm)
library(SnowballC)
library(tau)




#  Data Cleansing and structuring 


data2 = read.csv("SFrestaurants.csv")

## Data cleaning

data2$address2 = paste(data2$business_address, 
                       data2$business_city, 
                       data2$business_state, 
                       sep = ", ")

# Since a lot of the coordinated a are missing we run a gecode script to fill in all missing coordinates

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

## Reload the clean data 


data3 = read.csv("SFrestaurantsData.csv")

## convert dates from excel format

data3$inspection_date = as.Date(data3$inspection_date, origin = "1899-12-30")


# filter out observations that have "Off the Grid" for address; these are generally food trucks or otherwise mobile entities
# filter out obviously wrong lat/lon (some of geocode's results were in cities like New York, Denver, Chicago); these limits on lat/lon make sure the results are in the bay area


data4 = data3 %>%
  filter(business_address != "Off The Grid",
         between(business_latitude, 37, 38),
         between(business_longitude, -123, -122))


#-------------------------------------------------------------NIRANJAN------------------------------------------------------------#

## Linear Regression


# We start the analysis by running a correlation plot and regression to understand basic relationships in data .


# Eliminate Blank Risk Categories

data3 <- data3[-which(data3$risk_category == ""), ]


# Choose only relevant data Categories for the regression


myvars <- c("risk_category","inspection_date" ,"business_postal_code","inspection_type" ,"med_house_inc" , "inspection_score")

data3 <- data3[myvars]


### Function to change levels

change.to.levels = function (dataset, category.levels, given.labels = NULL) {
  labels = NULL
  if(!is.null(given.labels)) {
    labels = given.labels
  } else {
    labels = c(1:summary(category.levels)['Length'])
  }
  dataset = factor(dataset, levels=category.levels, labels=labels)
  dataset <- as.numeric(dataset)
}



# Eliminate NA dat for the Regression

data3 <- na.omit(data3)

## Change Categorical Varibales to Numerical Levels for Regression 

risk_category.values = levels(data3$risk_category)
data3$risk_category = change.to.levels(data3$risk_category, risk_category.values)


inspection_type.values = levels(data3$inspection_type)
data3$inspection_type = change.to.levels(data3$inspection_type, inspection_type.values)


business_postal_code.values = levels(data3$business_postal_code)
data3$business_postal_code = change.to.levels(data3$business_postal_code, business_postal_code.values)



## Correlation Plot 

corrdata = cor(data3)
corrplot(corrdata, method = "color", type = "upper")

# Divide into Training and Test Set

train=sample(c(1:dim(data3)[1]), dim(data3)[1]*0.70)

train.set=data3[train,]
test.set=data3[-train,]


# Regression Model

M1 = lm(risk_category~., data = test.set)
summary(M1)

]

#-------------------------------------------------------------NIRANJAN-------------------------------------------------------#

#-------------------------------------------------------------CJ-------------------------------------------------------------#

## Exploratory Daata Analysis 



## Spatial Analysis

# Download Map of SF using qmap

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


## Overlay the Inspection Data on top of the SF map to see how the scores are spread out by dividing them into different ranges

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

#  Filter the lower inspection scores to find any abnormal concentrations



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

# Understand if Vermin infestation in concentrated to one particular area of SF

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




#Distribution of violations in each risk category

data5 = data4 %>%
  filter(data4$risk_category != "")

risk_hist = ggplot(data5, aes(x = data5$risk_category)) +
  geom_histogram(stat = "count", fill ="lightblue", color ="red") +
  scale_x_discrete(limits = c("Low Risk", "Moderate Risk", "High Risk")) +
  xlab("") +
  ylab("")+
  ggtitle("Distribution of violation risk levels")

risk_hist  


# Distribution of scores across different bucket to understand dsitribution

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
  geom_col(fill ="lightblue", color ="red") +
  geom_text(aes(label = n),vjust = -0.25)+
  scale_y_continuous(limits = c(0,15000),
                     breaks = seq(0,15000,5000))+
  xlab("Score (0-100)") +
  ylab("Count")+
  ggtitle("Distribution of inspection scores") 


score_dist


#-------------------------------------------------------------CJ-------------------------------------------------------------#

#-------------------------------------------------------------AY-------------------------------------------------------------#



##Understand Impact of Median Income on Different scores 

data7 = read.csv("SFrestaurantsData.csv")

data7 = data6 %>%
  group_by(business_postal_code) %>%
  summarise(mean_inspection = mean(inspection_score), house_inc = mean(med_house_inc)) %>%
  filter(is.na(house_inc)==F)



ggplot(data7, aes(x= house_inc, y = mean_inspection)) +
  geom_point(color = "darkblue") +
  scale_y_continuous(name = "Mean Inspection Scores", 
                     limits = c(60, 100),
                     breaks = c(60, 70, 80, 90, 100),
                     labels = c(60, 70, 80, 90, 100)) +
  scale_x_continuous(name = "Household Income",
                     breaks = seq(0,200000, 25000)) +
  ggtitle("Inspection Scores by Median Household Income") +
  theme_bw()




## Trend of violations by top 10 restaurants with most violations

data8 = data6 %>%
  group_by(business_name,year(inspection_date)) %>%
  summarise(num_violations = n())

top10 = data8 %>%
  filter(`year(inspection_date)` == 2015) %>%
  arrange(-num_violations) %>%
  head(10)

top10trends = data8 %>%
  filter(business_name %in% top10$business_name)

names(top10trends)[2] = "Inspection_Year"

top10trends$Inspection_Year = as.integer(top10trends$Inspection_Year)

ggplot(top10trends, aes(x = Inspection_Year, 
                        y = num_violations, 
                        color = business_name)) +
  geom_line() +
  scale_x_continuous(limits = c(2015, 2018), 
                     breaks = c(2015, 2016, 2017, 2018),
                     labels = c(2015, 2016, 2017, 2018)) +
  ylab('Number of Violations') +
  xlab('Inspection Year') +
  ggtitle('Top 10 Violators from 2015 Over Time')



#-------------------------------------------------------------AY-------------------------------------------------------------#

#-------------------------------------------------------------Miles-----------------------------------------------------------#



# To find the key words used in vilations in form of a wordcloud

violaC = read.csv('SFrestaurantsData.csv', stringsAsFactors = FALSE)

violationCorpus = VCorpus(VectorSource(violaC$violation_description))
violationCorpus = tm_map(violationCorpus, content_transformer(tolower))
violationCorpus = tm_map(violationCorpus, removePunctuation)
violationCorpus = tm_map(violationCorpus, PlainTextDocument)
violationCorpus = tm_map(violationCorpus, removeWords, stopwords('english'))
violationCorpus = tm_map(violationCorpus, removeWords, c("food","risk","inadequate","improper","contact","holding", "safety"))


pal2= brewer.pal(8,"Dark2")
wordcloud(violationCorpus, max.words = 100, random.order = FALSE, rot.per = .15, colors = pal2)


### Understand which words are Frequently used in violations

dtm = TermDocumentMatrix(violationCorpus)
m = as.matrix(dtm)
v = sort(rowSums(m), decreasing = TRUE)
d = data.frame(word = names(v), freq = v)
head(d, 20)


#-------------------------------------------------------------Miles-----------------------------------------------------------#

#-------------------------------------------------------------Tom Wu-----------------------------------------------------------#

## understand the impact of icnome levels on inspection Scores


data7 = read.csv("SFrestaurantsData.csv")

median(data7$inspection_score, na.rm = T)

data7$med_house_inc = as.numeric(data7$med_house_inc)

data7$income_level = cut(data7$med_house_inc, 
                         breaks = c(50000,70000, 100000, 140000,200000),
                         labels = c("low","medium","high", "very high"))

data7 = data7 %>%
  filter(data7$income_level != "NA")

# Faceted density graph

ggplot(data = data7, aes (x=inspection_score, fill=income_level)) +
  geom_density(alpha = 0.3) +
  facet_wrap (~ income_level)


ggplot(data = data7, aes (x=income_level, y=inspection_score)) +
  geom_boxplot()

# (below) Stacked bar. Pretty (kind of), but useless.
ggplot(data = data7, aes (x=inspection_score, fill=income_level)) +
  geom_bar()

# (below) scatterplot (faceted by risk). Pretty useless...
ggplot(data = data4, aes (x = inspection_date, y=inspection_score)) +
  geom_point() +
  facet_wrap(~ risk_category)



#-------------------------------------------------------------Tom Wu-----------------------------------------------------------#





##------------------------------------------------------Shiny App--------------------------------------------------------------#
