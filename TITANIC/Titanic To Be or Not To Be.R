
setwd("~/Documents/KAGGLE/TITANIC")
library(dplyr)
library(rpart)
library(rpart.plot)
library(party)

train <- read.csv('train.csv')
train$dataset <- 'train'
test <- read.csv('test.csv')
test$dataset <- 'test'
test$Survived <- -1

#Feature Engineering

overall <- rbind(train, test)

overall$Title <- sub(" .*","",sub(".*, ","",overall$Name))
overall$Title[overall$Title %in% c('Don.', 'Sir.')] <- 'Sir.'
overall$Title[overall$Title %in% c('Capt.', 'Major.','Col.','Jonkheer.')] <- 'Army.'
overall$Title[overall$Title %in% c('Dona.', 'Lady.', 'the', 'Mme.', 'Mlle.')] <- 'Lady.'

overall$Family_Name <- sub(",.*","",overall$Name)

overall$Family_Size <- as.numeric(overall$SibSp + 1 + overall$Parch)

overall$FamilyID <- paste(overall$Family_Name, overall$Family_Size, sep = "_")
overall$FamilyID[(overall$SibSp + 1 + overall$Parch) <= 1] <- 'Small_2'

overall$FamilyID <- as.factor(overall$FamilyID)
overall$Title <- as.factor(overall$Title)

typical_age <- overall %>% 
  group_by(Title, Sex,  Pclass, Embarked) %>%
  summarise(Age = median(Age, na.rm = TRUE))

overall_missing_age <- overall[is.na(overall$Age),] %>% 
  select(-Age) %>%
  inner_join(typical_age, by = c("Title","Sex","Pclass", "Embarked"))

overall <- rbind(overall, overall_missing_age)
overall <- overall[!is.na(overall$Age),]

overall$Embarked[overall$Embarked == ''] <- 'S'
overall$Embarked <- as.factor(overall$Embarked)

overall$Fare[is.na(overall$Fare)] <- median(overall$Fare, na.rm = TRUE)

overall$

#Extract Cabin Num from Cabin
overall$Cabin[is.na(overall$Cabin)] <- ''
overall$Cabin_Num<-sapply(as.character(overall$Cabin),function(x) strsplit(x,'[A-Z]')[[1]][2])
overall$Cabin_Num<-as.numeric(overall$Cabin_Num)
overall$Cabin_Pos<-'Unknown'

#Categorize 1-50 as Front, 50-100 as Middle, >100 as End
overall$Cabin_Pos[overall$Cabin_Num<50]<-'Front'
overall$Cabin_Pos[overall$Cabin_Num>=50 & overall$Cabin_Num<100]<-'Middle'
overall$Cabin_Pos[overall$Cabin_Num>=100]<-'End'
overall$Cabin_Pos<-factor(overall$Cabin_Pos)

overall$Age_Class <- overall$Age * overall$Pclass

overall$Ticket_Group <- gsub("[[[:punct:][:space:][:digit:]]","",overall$Ticket)
overall$Ticket_Number <- substr(gsub("[[[:punct:][:space:][:alpha:]]","",overall$Ticket),1,1)

overall$Ticket_Group <- as.factor(overall$Ticket_Group)
overall$Ticket_Number <- as.numeric(overall$Ticket_Number)
overall$Ticket_Number[is.na(overall$Ticket_Number)] <- 0

train <- overall %>% filter(dataset == 'train')
test <- overall %>% filter(dataset == 'test')

#Fitting a Conditional Inference Tree
set.seed <- (1987)
fit <- cforest(as.factor(Survived) ~ Pclass + Age + Fare + Sex + Embarked + Title + Family_Size + FamilyID + Cabin_Pos + Age_Class + Ticket_Group,
               data = train,
               controls = cforest_unbiased(ntree = 3000, mtry = 3))

#Viewing a sample tree
party:::prettytree(fit@ensemble[[1]], names(fit@data@get("input")))

#Predicting for the test dataset using CIT
prediction <- predict(fit, test, OOB = TRUE, type = "response")

submit <- data.frame(PassengerId = test$PassengerId,
                     Survived = prediction)

prop.table(table(submit$Survived))

write.csv(submit,'submit.csv',row.names = FALSE)