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
var <- 'all'

## Get taining data according to dataset decision
path <- "data/"
if(oversampled == T) {
  path <- paste(path,"smote/",sep="")
}
data <- read.delim(paste(path,"data_", var, ".csv",sep=""),
                   header = TRUE, sep = ",", dec = ".", na.strings = "", row.names = 1) 
train <- read.delim(paste(path,"training_", var, ".csv",sep=""),
                   header = TRUE, sep = ",", dec = ".", na.strings = "", row.names = 1)
test <- read.delim(paste(path,"test_", var, ".csv",sep=""),
                   header = TRUE, sep = ",", dec = ".", na.strings = "", row.names = 1)

## Fix levels of ftors in training/test
if(var != 'num') {
  good.levels <- levels(data$P_TYPE)
  levels(train$P_TYPE) <- good.levels
  levels(test$P_TYPE) <- good.levels
  good.levels <- levels(data$S_TYPE_TEMP)
  levels(train$S_TYPE_TEMP) <- good.levels
  levels(test$S_TYPE_TEMP) <- good.levels
  rm(good.levels)
}

## Get target variable index (depends on chosen dataset)
target <- grep("P_HABITABLE", colnames(test))


################## FINAL MODEL TEST ##################

## Parameter set depending on dataset
mtry <- 4
if(var == 'cat') {
  mtry <- 5
}
ntree <- 300
if(var == 'all') {
  ntree <- 200
}

##### Random Forest #####

## Train model
rf.model <- randomForest(P_HABITABLE~., data=train, mtry=mtry, ntree=ntree, importance=T)
rf.model
## Predict test data
pred <- predict(rf.model, newdata=test[,-c(target)])
(ct <- table(Truth=test[,target], Pred=pred))
## Print accuracy measures
print("TEST PREDICTION MEASURES:")
print("Habitable class accuracy:")
prop.table(ct, 1)[1,1]
print("Habitable false negative rate:")
prop.table(ct,1)[1,2]
print("Model general accuracy:")
1-mean(rf.model$err.rate[,1])
## Plot feature importance
varImpPlot(rf.model, main="Feature importance for Random Forest final model")

##### QDA #####

if(var == 'cat' || (var == 'all' && oversampled == T)) {
  qda.model <- qda(P_HABITABLE~.,train)
  qda.model
  pred <- predict(qda.model, test[,-c(target)])
  (ct <- table(Truth=test[,target], Pred=pred$class))
  diag(prop.table(ct, 1))
  print("Habitable class accuracy:")
  print(diag(prop.table(ct, 1))[1])
  print("Habitable false negative rate:")
  print(prop.table(ct,1)[1,2])
  print("Model general accuracy:")
  print(sum(diag(prop.table(ct))))
  
}

##### LDA #####

if(var == 'cat'  && oversampled == T) {
  lda.model <- lda(P_HABITABLE~.,train)
  lda.model
  pred <- predict(lda.model, test[,-c(target)])
  (ct <- table(Truth=test[,target], Pred=pred$class))
  diag(prop.table(ct, 1))
  print("Habitable class accuracy:")
  print(diag(prop.table(ct, 1))[1])
  print("Habitable false negative rate:")
  print(prop.table(ct,1)[1,2])
  print("Model general accuracy:")
  print(sum(diag(prop.table(ct))))
  
}

##### NAIVE BAYES #####

if(oversampled == F || ( oversampled = T && var == 'all')) {
  naive.model <- naiveBayes(P_HABITABLE~., data=train)
  naive.model
  pred <- predict(naive.model, newdata=test)
  (ct <- table(Truth=test[,target], Pred=pred))
  print("Habitable class accuracy:")
  print(diag(prop.table(ct, 1))[1])
  print("Habitable false negative rate:")
  print(prop.table(ct,1)[1,2])
  print("Model general accuracy:")
  print(sum(diag(prop.table(ct))))
}

##### KNN #####

if(oversampled == T & var == 'num') {
  knn.model.train <- knn(train[,-c(target)], test[,-c(target)], train[,target], k = 5)
  (ct <- table(Truth=test[,target], Pred=knn.model.train))
  print("Habitable class accuracy:")
  print(diag(prop.table(ct, 1))[1])
  print("Habitable false negative rate:")
  print(prop.table(ct,1)[1,2])
  print("Model general accuracy:")
  print(sum(diag(prop.table(ct))))
}

