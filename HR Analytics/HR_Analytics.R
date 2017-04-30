
# Set working direcotory
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
 
library(dplyr)
library(rpart)
library(rpart.plot)
library(party)
library(readr)

#Feature Engineering

train <- read_csv('HR_comma_sep.csv')

train$sales <- as.factor(train$sales)
train$salary <- as.factor(train$salary)

#Fitting a Conditional Inference Tree
set.seed <- (1987)
fit <- cforest(as.factor(left) ~ .,
               data = train,
               controls = cforest_unbiased(ntree = 2000, mtry = 3))

#Viewing a sample tree
party:::prettytree(fit@ensemble[[1]], names(fit@data@get("input")))

#Predicting for the test dataset using CIT
prediction <- predict(fit, train, OOB = TRUE, type = "response")

submit <- data.frame(Actual = train$left,
                     Predicted = prediction)

prop.table(table(submit$Actual, submit$Predicted),1)
