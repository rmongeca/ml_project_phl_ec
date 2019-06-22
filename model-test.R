# Library and set-up
rm(list=ls())
set.seed(42)
library(class)
library(dplyr)
library(e1071)
library(klaR)
library(MASS)
library(randomForest)
library(TunePareto)

## Variables to select dataset to test
oversampled <- FALSE
var <- 'num'

## Get taining data according to dataset decision
path <- "data/"
if(oversampled == T) {
  path <- paste(path,"smote/",sep="")
}
train <- read.delim(paste(path,"training_", var, ".csv",sep=""),
                   header = TRUE, sep = ",", dec = ".", na.strings = "", row.names = 1)
test <- read.delim(paste(path,"test_", var, ".csv",sep=""),
                   header = TRUE, sep = ",", dec = ".", na.strings = "", row.names = 1)

## Get target variable index (depends on chosen dataset)
target <- grep("P_HABITABLE", colnames(test))


################## FINAL MODEL TEST ##################

## Parameter set depending on dataset
mtry <- 4
if(var == 'cat') {
  mtry <- 2
}
ntree <- 100
if(var == 'num') {
  ntree <- 200
}

##### Random Forest #####

## Train model
rf.model <- randomForest(P_HABITABLE~., data=train, mtry=mtry, ntree=ntree, importance=T)
print("TRAINING MEASURES:")
ct <- rf.model$confusion
print("Habitable accuracy:")
prop.table(ct, 1)[1,1]
print("Habitable false negative rate:")
prop.table(ct,1)[1,2]
## Predict test data
pred <- predict(rf.model, newdata=test[,-c(target)])
(ct <- table(Truth=test[,target], Pred=pred))
## Print accuracy measures
print("TEST PREDICTION MEASURES:")
print("Habitable accuracy:")
prop.table(ct, 1)[1,1]
print("Habitable false negative rate:")
prop.table(ct,1)[1,2]
## Plot feature importance
varImpPlot(rf.model, main="Feature importance for Random Forest final model")

##### QDA #####

if(var != 'all') {
  qda.model <- qda(P_HABITABLE~.,train)
  pred <- predict(qda.model, test[,-c(target)])
  (ct <- table(Truth=test[,target], Pred=pred$class))
  ## Really low habitable accuracy, really high false positives
  diag(prop.table(ct, 1))
  print("Habitable accuracy:")
  diag(prop.table(ct, 1))[1]
  print("Habitable false negative rate:")
  prop.table(ct,1)[1,2]
}

##### NAIVE BAYES #####

if(var == 'all') {
  naive.model <- naiveBayes(P_HABITABLE~., data=train)
  pred <- predict(naive.model, newdata=test)
  (ct <- table(Truth=test[,target], Pred=pred))
  diag(prop.table(ct, 1))
  print("Habitable accuracy:")
  diag(prop.table(ct, 1))[1]
  print("Habitable false negative rate:")
  prop.table(ct,1)[1,2]
}

##### KNN #####

if(oversampled == T & var == 'num') {
  knn.model.train <- knn(train[,-c(target)], test[,-c(target)], train[,target], k = 5)
  (ct <- table(Truth=test[,target], Pred=knn.model.train))
  print("Habitable accuracy:")
  diag(prop.table(ct, 1))[1]
  print("Habitable false negative rate:")
  prop.table(ct,1)[1,2]
}
