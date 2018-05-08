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
## Donot Re-Run the Below For Loop

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



## Linear Regression


# We start the analysis by running a correlation plot and regression to understand basic relationships in data .

data3 = data4

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



rmse <- function(x, y){ sqrt(mean ((x-y)^2)) }

M1 = lm(inspection_score~., data = test.set)
summary(M1)

plot(M1)



# training set
predict.train = predict(M1, data = train.set)
rmse(predict.train, train.set$risk_category) 

# testing set
predict.test = predict(M1, data = test.set)
rmse(predict.test, test.set$risk_category) 



## Exploratory Daata Analysis 



## Spatial Analysis

# Download Map of SF using qmap

SF = qmap("San Francisco",
          zoom = 12,
          maptype = "roadmap",
          color = "bw")

SF2 = qmap("San Francisco",
           zoom = 13,
           maptype = "roadmap",
           color = "bw")



SF2 


data6 = data4


data6 = data6 %>%
  filter(data6$inspection_score != "NA")

data6$gr = cut(data6$inspection_score, 
               breaks = c(39,49,59,69,79,89,99,100),
               labels = c(40,50,60,70,80,90,100))



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


SF2 + geom_point(data = data_vermin, 
                 aes(x= business_longitude, 
                     y = business_latitude
                 ),
                 alpha = 0.2,
                 shape = 21,
                 size = 2,
                 fill = "darkred", color = "darkred"
) +
  ggtitle("High Risk Vermin Infestations") +
  theme(legend.position = "none")


## heat map of inspection scores

SF + geom_point(data = data6, 
                aes(x= business_longitude, 
                    y = business_latitude,
                    fill = data6$inspection_score,
                    color = data6$inspection_score),
                
                alpha = 100/((data6$inspection_score)^2),
                
                shape = 21,
                size = (100/data6$inspection_score)^3
) +
  scale_fill_gradient2(midpoint= 50,mid = "darkred", high = NA, name = "Score")+
  scale_color_gradient(low = NA, high = NA, guide = FALSE)



SF + geom_point(data = data6, 
                aes(x= business_longitude, 
                    y = business_latitude),
                alpha = 1,
                
                shape = 19,
                size = 1
)





#Distribution of violations in each risk category

data5 = data4 %>%
  filter(data4$risk_category != "")

risk_hist = ggplot(data5, aes(x = data5$risk_category)) +
  geom_histogram(stat = "count" , fill = "#BA032E") +
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
  geom_col(fill = "#BA032E") +
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



data7 = data6 %>%
  group_by(business_postal_code) %>%
  summarise(mean_inspection = mean(inspection_score), house_inc = mean(med_house_inc)) %>%
  filter(is.na(house_inc)==F)



ggplot(data7, aes(x= house_inc, y = mean_inspection)) +
  geom_point(color = "red") +
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


## understand the impact of icnome levels on inspection Scores


data4 = read.csv("SFrestaurantsData.csv")

## Income analysis section

dataincome = data4

dataincome$med_house_inc = as.numeric(data4$med_house_inc)

dataincome$income_level = cut(dataincome$med_house_inc, 
                              breaks = c(50000,70000, 90000, 120000,200000),
                              labels = c("low","medium","high", "very high"))

dataincome = dataincome %>%
  filter(dataincome$income_level != "NA")

# Inspection Score Distribution by Income Level Faceted density graph.

ggplot(data = dataincome, aes (x=inspection_score, fill=income_level)) +
  geom_density(alpha = 0.3) +
  facet_wrap (~ income_level) +
  labs(x = "Inspection Scores", y = "# of Inspections", fill = "Income Levels") +
  ggtitle ("Inspection Score Distribution by Income Level")

# Inspection Score Distribution by Income Level Boxplot.

ggplot(data = dataincome, aes (x=income_level, y=inspection_score)) +
  geom_boxplot() +
  labs(x = "Income Levels", y = "Inspection Score") +
  ggtitle ("Inspection Score Distribution by Income Level")

# Income Level Count by Inspection Score. 
ggplot(data = dataincome, aes (x=inspection_score, fill=income_level)) +
  geom_bar() +
  labs(x = "Inspection Scores", y = "# of Inspections", fill = "Income Levels") +
  ggtitle ("Income Level Count by Inspection Score")


#  Stacked bar chart indicating income level share of inspection scores. 

dataincomeperc <- dataincome %>%
  group_by(inspection_score, income_level) %>%
  summarise (count=n()) %>%
  mutate (perc = count/sum(count))


ggplot(dataincomeperc, aes(x = factor(inspection_score), y = perc*100, fill = factor(income_level))) +
  geom_bar(stat="identity", width = 0.7) +
  labs(x = "Inspection Scores", y = "% of Inspections", fill = "Income Levels") +
  scale_x_discrete(breaks = c(50, 60, 70, 80, 90)) +
  ggtitle ("Income Level Share by Inspection Score")



##------------------------------------------------------Shiny App - Please Select and Run Seperately --------------------------------------------------------------#




library(shiny)
library(ggplot2)
library(lubridate)
library(maps)
library(ggmap)
library(dplyr)

library(wordcloud)
library(tm)
library(SnowballC)
library(tau)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("SF Restaurant Health Violations Map and Wordcloud"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      helpText("What do you want to see?"),
      sliderInput("threshold",
                  "Health score:",
                  min = 50,
                  max = 100,
                  value = c(50,100),
                  step = 5),
      selectInput("riskcat",
                  "Risk Category:",
                  c("All","High Risk","Moderate Risk","Low Risk"),
                  selected = "Moderate Risk"),
      sliderInput("maxwords",
                  "Max cloud size:",
                  min = 0,
                  max = 200,
                  value = 100,
                  step = 10)
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("mappy"),
      plotOutput("wordcloud"),
      plotOutput("wordchart")
      
    )
  )
)

# Define server logic required to draw the map
server <- function(input, output) {
  
  output$mappy <- renderPlot({
    
    ### START MAP!
    
    data3 = read.csv("SFrestaurantsData.csv")
    
    
    
    # filter out observations that have "Off the Grid" for address; these are generally food trucks or otherwise mobile entities
    # filter out obviously wrong lat/lon (some of geocode's results were in cities like New York, Denver, Chicago); these limits on lat/lon make sure the results are in the bay area
    
    
    data4 = data3 %>%
      filter(business_address != "Off The Grid",
             between(business_latitude, 37, 38),
             between(business_longitude, -123, -122))
    
    
    SF = qmap("San Francisco",
              zoom = 12,
              maptype = "roadmap",
              color = "bw")
    
    
    data6 = data4
    
    
    data6 = data6 %>%
      filter(data6$inspection_score >= 0)
    
    
    # filter by score range
    
    data6$inspection_score = as.numeric(data6$inspection_score)
    
    data_lowIS = data6 %>%
      filter(data6$inspection_score >= input$threshold[1] & data6$inspection_score <= input$threshold[2]) 
    
    
    # graph it!...
    
    SF + geom_point(data = data_lowIS, 
                    aes(x = business_longitude, 
                        y = business_latitude,
                        fill = as.numeric(inspection_score)),
                    alpha = 0.5,
                    shape = 21,
                    size = 1.5) +
      scale_fill_gradient(low = "red", high = "green") +
      labs(fill = "Health Scores")
    
    # END MAP!
    
  })
  
  
  
  
  
  output$wordcloud <- renderPlot({
    
    violaRaw = read.csv('SFrestaurantsData.csv', stringsAsFactors = FALSE)
    
    ## filter by risk category
    
    if (input$riskcat == "All") {violaC = violaRaw} else
    {violaC = violaRaw %>%
      filter(violaRaw$risk_category == input$riskcat)}
    
    ## filter by score range
    
    violaC = violaC %>%
      filter(violaC$inspection_score >= input$threshold[1] & violaC$inspection_score <= input$threshold[2]) 
    
    violationCorpus = VCorpus(VectorSource(violaC$violation_description))
    violationCorpus = tm_map(violationCorpus, content_transformer(tolower))
    violationCorpus = tm_map(violationCorpus, removePunctuation)
    violationCorpus = tm_map(violationCorpus, PlainTextDocument)
    violationCorpus = tm_map(violationCorpus, removeWords, stopwords('english'))
    violationCorpus = tm_map(violationCorpus, removeWords, c("food","moderate", "high", "risk","inadequate","improper","contact","holding", "safety"))
    
    
    pal2= brewer.pal(8,"Dark2")
    wordcloud(violationCorpus, max.words = input$maxwords, random.order = FALSE, rot.per = .15, colors = pal2)
    
    
    
    
  })
  
  output$wordchart <- renderPlot({
    
    violaRaw = read.csv('SFrestaurantsData.csv', stringsAsFactors = FALSE)
    
    ## filter by risk category
    
    if (input$riskcat == "All") {violaC = violaRaw} else
    {violaC = violaRaw %>%
      filter(violaRaw$risk_category == input$riskcat)}
    
    ## filter by score range
    
    violaC = violaC %>%
      filter(violaC$inspection_score >= input$threshold[1] & violaC$inspection_score <= input$threshold[2]) 
    
    
    violationCorpus = VCorpus(VectorSource(violaC$violation_description))
    violationCorpus = tm_map(violationCorpus, content_transformer(tolower))
    violationCorpus = tm_map(violationCorpus, removePunctuation)
    violationCorpus = tm_map(violationCorpus, PlainTextDocument)
    violationCorpus = tm_map(violationCorpus, removeWords, stopwords('english'))
    violationCorpus = tm_map(violationCorpus, removeWords, c("food","moderate", "high", "risk","inadequate","improper","contact","holding", "safety"))
    
    ### Frequent words
    
    dtm = TermDocumentMatrix(violationCorpus)
    m = as.matrix(dtm)
    v = sort(rowSums(m), decreasing = TRUE)
    d = data.frame(word = names(v), freq = v)
    
    barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
            col ="lightblue", main ="Most frequent words",
            ylab = "Word frequencies")
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)



