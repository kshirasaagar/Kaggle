
# Set working direcotory
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

#setwd("~/Documents/KAGGLE/Kaggle/Sberbank")

library(dplyr)
library(rpart)
library(rpart.plot)
library(party)
library(readr)
library(corrplot)
library(Boruta)
library(caret)
library(pROC)

train <- read.csv('train.csv', stringsAsFactors = TRUE)
test <- read.csv('test.csv', stringsAsFactors = TRUE)
macro <- read.csv('macro.csv', stringsAsFactors = TRUE)

train <- merge(train, macro, by = 'timestamp')
test <- merge(test, macro, by = 'timestamp')

# summary(train$price_doc)
# summary(train$full_sq)
# summary(train$price_doc/train$full_sq)

train$dataset <- 'train'
test$dataset <- 'test'

#GUESS 1 - Just guessing based on logical distributions
test$price_doc <- test$full_sq * 135300 + (rnorm(nrow(test), mean = 6274, sd = 4740))

###################   MISSING VALUES  ###############################

#Overall dataset for easier missing value and outlier treatment
overall <- rbind(train, test)

#Missing Value Treatment
overall$state[is.na(overall$state)] <- 0

overall$build_year[is.na(overall$build_year)] <- 2020
overall$build_year[overall$build_year < 1600] <- 2017 - overall$build_year[overall$build_year < 1600]
overall$build_year[overall$build_year > 2020] <- 2009

train <- overall %>% filter(dataset == "train")
test <- overall %>% filter(dataset == "test")

####################################################################

#GUESS 2 - Estimated based on key variables numerically
price_by_area <- train %>% 
                       group_by(state, sub_area, product_type) %>% 
                       summarise(med_price = median(price_doc, na.rm = TRUE)/median(full_sq, na.rm = TRUE))

test2 <- merge(test, price_by_area, by = c('state','sub_area','product_type'), all.x = TRUE)
sum(is.na(test2$med_price))

test2$med_price[is.na(test2$med_price)] <- 135300

test2$price_doc <- test2$full_sq * test2$med_price + (rnorm(nrow(test), mean = 6274, sd = 4740))

####################################################################

#GUESS 3 - Feature Importance using CART
#CART to identify top 100 features
fit <- rpart(price_doc ~ .,
             data = train[-c(1,2,13)],
             control = rpart.control(cp = .05))

variables <- data.frame(fit$variable.importance)
variables$variable <- row.names(variables)
row.names(variables) <- NULL
colnames(variables) <- c('importance','variable')

train_subset <- train %>% select(price_doc, one_of(variables$variable))

glm_fit <- glm(price_doc ~ .,
               data = train_subset)

prediction <- predict(glm_fit, test)
prediction[is.na(prediction)] <- mean(prediction, na.rm = TRUE)
prediction[prediction <= 0] <- mean(prediction, na.rm = TRUE)

prediction <- test$full_sq * 135300 + (prediction/1000)

####################################################################

#Submission Codes
submit <- data.frame(id = test$id,
                     price_doc = prediction)

submit <- test2 %>% select(id, price_doc)

write.csv(submit,paste0('submit',Sys.Date(),'.csv'),row.names = FALSE)
