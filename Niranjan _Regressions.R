### Niranjan Regresssion Anakysis



library(ggplot2)
library(lubridate)
library(maps)
library(ggmap)
library(dplyr)
library(corrplot)
library(psych)
library(rpart)


data3 = read.csv("SFrestaurantsdata.csv")

data3 <- data3[-which(data3$risk_category == ""), ]


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


head(data3)


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


train=sample(c(1:dim(data3)[1]), dim(data3)[1]*0.70)

train.set=data3[train,]
test.set=data3[-train,]

rmse <- function(x, y){ sqrt(mean ((x-y)^2)) }

M1 = lm(risk_category~., data = test.set)
summary(M1)

# training set
predict.train = predict(M1, data = train.set)
rmse(predict.train, train.set$risk_category) 

# testing set
predict.test = predict(M1, data = test.set)
rmse(predict.test, test.set$risk_category) 















