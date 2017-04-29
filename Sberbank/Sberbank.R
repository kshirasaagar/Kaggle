
# Set working direcotory
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

library(dplyr)
library(rpart)
library(rpart.plot)
library(party)
library(readr)

test <- read_csv('test.csv')
train <- read_csv('train.csv')
macro <- read_csv('macro.csv')