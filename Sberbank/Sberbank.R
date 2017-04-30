
# Set working direcotory
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

library(dplyr)
library(rpart)
library(rpart.plot)
library(party)
library(readr)
library(corrplot)

test <- read.csv('test.csv', stringsAsFactors = TRUE)
train <- read.csv('train.csv', stringsAsFactors = TRUE)
macro <- read.csv('macro.csv', stringsAsFactors = TRUE)

train <- merge(train, macro, by = 'timestamp')

fit <- rpart(price_doc ~ .,
             data = train[-c(1,2,13)],
             control = rpart.control(cp = .05))

attr(fit$terms,"term.labels")

# train_corr <- train %>% 
#               select(price_doc,
#                      cpi,
#                      ppi,
#                      eurrub,
#                      usdrub,
#                      mortgage_value,
#                      mortgage_growth,
#                      mortgage_rate,
#                      salary,
#                      employment,
#                      marriages_per_1000_cap,
#                      construction_value) %>% 
#               cor(use = "complete.obs")
# 
# corrplot.mixed(train_corr)
