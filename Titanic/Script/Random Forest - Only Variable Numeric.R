#REMOVE ALL INFORMATIONS
rm(list = ls())

#LOAD LIBRARY
require(tidyverse)
require(caTools)
require(caret)
require(randomForest)
require(rpart)
require(rattle)

#SET DIR AND LOAD FILES
setwd("C:/Kaggle/Titanic/Dataset")
list.files()
df_train <- read.csv("train.csv", stringsAsFactors = F)

#SEE INFORMATIONS ABOUT DATASET
str(df_train)
#how many na is in the dataset?
sapply(df_train, function(x) sum(is.na(x)))
#replace na for median in variable age
df_train$Age <- ifelse(is.na(df_train$Age) == "TRUE", 
                       median(df_train$Age, na.rm = T), df_train$Age)

#put value 1 to sex male and 0 female
df_train$Sex <- ifelse(df_train$Sex == "male", 1, 0)

#SPLIT TEST AND SPLIT
set.seed(123)
divisao <- sample.split(df_train$Survived, SplitRatio = 0.7)
base_treinamento <- subset(df_train, divisao == TRUE)
base_teste <- subset(df_train, divisao == FALSE)

#FIRST WE'LL USE ONLY NUMERIC VARIABLES
#RANDOM FOREST
model_rf <- rpart(factor(Survived) ~ factor(Pclass) + factor(Sex) + Age + SibSp + Parch + Fare,
                  data = base_treinamento)
model2_rf <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare,
                  data = base_treinamento)

#summary
summary(model_rf)
#plot
fancyRpartPlot(model_rf)
#var importance
varImp(model_rf) %>% 
  rownames_to_column() %>% 
  arrange(desc(Overall))


#PREDICTION
base_teste2 <- base_teste[,c(2,3,5,6,7,8,10)]
base_teste2$Pclass <- factor(base_teste2$Pclass)
base_teste2$Sex <- factor(base_teste2$Sex)


previsoes <- predict(model2_rf, newdata = base_teste2[-1])
previsoes
#condiction
result <- data.frame(previsoes)
result$Survived <- ifelse(result$X0 > result$X1, 0,1)
result <- result[3]

#confusion matrix
table(result$Survived, base_teste2$Survived)
(148+63)/268

#USED DATASET TEST
list.files()
df_test <- read.csv("test.csv", stringsAsFactors = F)
names(df_test)
df_test <- df_test[,c(1,2,4,5,6,7,9)]

#SAME THINGS WE DO FOR TRAIN DATASET
sapply(df_test, function(x) sum(is.na(x)))
#replace na for median in variable age
df_test$Age <- ifelse(is.na(df_test$Age) == "TRUE", 
                       median(df_test$Age, na.rm = T), df_test$Age)
df_test$Fare <- ifelse(is.na(df_test$Fare) == "TRUE", 
                      median(df_test$Fare, na.rm = T), df_test$Fare)


#put value 1 to sex male and 0 female
df_test$Sex <- ifelse(df_test$Sex == "male", 1, 0)

#factor
df_test$Pclass <- factor(df_test$Pclass)
df_test$Sex <- factor(df_test$Sex)

#new result
submission <- data.frame(ifelse(final$X0 > final$X1, 0 ,1))
submission$PassengerID <- df_test[1]

submission <- final_result[,c(2,1)]

colnames(submission) <- c("PassengerId", "Survived")

write.csv(submission, "submission.csv")
