
# Set working direcotory
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

library(dplyr)
library(rpart)
library(rpart.plot)
library(party)
library(readr)
library(corrplot)

train <- read.csv('train.csv', stringsAsFactors = TRUE)
test <- read.csv('test.csv', stringsAsFactors = TRUE)
macro <- read.csv('macro.csv', stringsAsFactors = TRUE)

train <- merge(train, macro, by = 'timestamp')
test <- merge(test, macro, by = 'timestamp')

fit <- rpart(price_doc ~ .,
             data = train[-c(1,2,13)],
             control = rpart.control(cp = .001))

variables <- data.frame(fit$variable.importance)
variables$variable <- row.names(variables)
row.names(variables) <- NULL
colnames(variables) <- c('importance','variable')

train_subset <- train %>% select(price_doc, one_of(variables$variable))

glm_fit <- glm(price_doc ~ .,
               family = 'poisson',
               data = train_subset)

prediction <- predict(glm_fit, test)

submit <- data.frame(id = test$id,
                     price_doc = prediction)

