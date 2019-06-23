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
data <- read.delim(paste(path,"training_", var, ".csv",sep=""),
                   header = TRUE, sep = ",", dec = ".", na.strings = "", row.names = 1)

## Get target variable index (depends on chosen dataset)
target <- grep("P_HABITABLE", colnames(data))
  
################### LDA MODEL ###################

## LDA visualization plot
set.seed(42)
if(var == 'num') {
  lda.model <- lda (x=data[, -c(target)], grouping=data[,target], CV=F)
  loadings <- as.matrix(data[,-c(target)]) %*% as.matrix(lda.model$scaling)
  boxplot(loadings~data[,target], main="Linear discriminants against classes",
          ylab="LD1 loadings",xlab="Target modalities")
  ## In general really bad separation between classes
  ## Also, we only have one LD
}

## Predition model
lda.model <- lda (x=data[, -c(target)], grouping=data[,target], CV=TRUE)
(ct <- table(Truth=data[,target], Pred=lda.model$class))
## Really low habitable accuracy, really high false positives
print("Habitable class accuracy:")
prop.table(ct, 1)[1,1]
print("Habitable false negative rate:")
prop.table(ct,1)[1,2]
print("Model general accuracy:")
sum(diag(prop.table(ct)))

################### QDA MODEL ###################

## QDA visualization plot
set.seed(42)
if(var == 'num') {
  qda.model <- qda (x=data[, -c(target)], grouping=data[,target], CV=F)
  loadings <- as.matrix(data[which(data$P_HABITABLE == 'non-habitable'),-c(target)]) %*% as.matrix(qda.model[["scaling"]][,1:2,2])
  plot(loadings, col="green", ylim=c(-12,7), xlim = c(-2,6),
       main="Quadratic discriminants against classs",
       ylab="QD2", xlab="QD1")
  loadings <- as.matrix(data[which(data$P_HABITABLE == 'habitable'),-c(target)]) %*% as.matrix(qda.model[["scaling"]][,1:2,1])
  points(loadings, col="red")
  legend('bottomright',legend=c("Non-habitable","Habitable"), pch=c(1,1), col=c("green", "red"))
  ## In general really good separation between classes
}

## Predition model
qda.model <- qda (x=data[, -c(target)], grouping=data[,target], CV=TRUE)
(ct <- table(Truth=data[,target], Pred=qda.model$class))

print("Habitable class accuracy:")
prop.table(ct, 1)[1,1]
print("Habitable false negative rate:")
prop.table(ct,1)[1,2]
print("Model general accuracy:")
sum(diag(prop.table(ct)))

################### NAIVE BAYES MODEL ###################

set.seed(42)
k <- 5
folds <- generateCVRuns(data$P_HABITABLE, ntimes=1, nfold=k, stratified=TRUE)
cv.results <- matrix (rep(0,4*k),nrow=k)
colnames (cv.results) <- c("fold","Hab.accur","Hab.FN","Model.accur")

## CV loop for accuracy measures
for(i in 1:k) {
  va <- unlist(folds[[1]][[i]])
  naive.model <- naiveBayes(P_HABITABLE~., data=data[-va,])
  
  ## Get predictions accuracy measures for fold
  pred <- predict(naive.model, newdata=data[va,])
  ct <- table(Truth=data[va,target], Pred=pred)
  cv.results[i,"Hab.accur"] <- prop.table(ct, 1)[1,1]
  cv.results[i,"Hab.FN"] <- prop.table(ct,1)[1,2]
  cv.results[i,"Model.accur"] <- sum(diag(prop.table(ct)))
  cv.results[i,"fold"] <- i
  
}
rm(i, k, folds, pred, va)

## Get predictions accuracy measures
print("Habitable class accuracy:")
mean(cv.results[,"Hab.accur"])
print("Habitable false negative rate:")
mean(cv.results[,"Hab.FN"])
print("Model general accuracy:")
mean(cv.results[,"Model.accur"])
  

################### RANDOM FOREST MODEL ###################
set.seed(42)

## Tune mtry number of predictors
seq.times <- ncol(data)-2
k <- 5
folds <- generateCVRuns(data$P_HABITABLE, ntimes=seq.times, nfold=k, stratified=TRUE)

mtry <- seq(2,ncol(data)-1,length.out = seq.times)
mtry <- as.integer(mtry)

seq.results <- matrix(rep(0,4*seq.times),nrow=seq.times)
colnames (seq.results) <- c("Mtry","Hab.accur","Hab.FN","Model.accur")
cv.results <- matrix (rep(0,4*k),nrow=k)
colnames (cv.results) <- c("fold","Hab.accur","Hab.FN","Model.accur")

## Paramter tuning loop 
for(j in 1:seq.times) {
  seq.results[j, "Mtry"] <- mtry[j]
  for(i in 1:k) {
    va <- unlist(folds[[j]][[i]])
    rf.model <- randomForest(P_HABITABLE~., data=data[-va,], mtry=mtry[j], importance=F, proximity=F)
    
    ## Get predictions accuracy measures for fold
    pred <- predict(rf.model, newdata=data[va,])
    ct <- table(Truth=data[va,target], Pred=pred)
    cv.results[i,"fold"] <- i
    cv.results[i,"Hab.accur"] <- diag(prop.table(ct, 1))[1]
    cv.results[i,"Hab.FN"] <- prop.table(ct,1)[1,2]
    cv.results[i,"Model.accur"] <- sum(diag(prop.table(ct)))
    
  }
  ## Get predictions accuracy measures
  seq.results[j,"Hab.accur"] <- mean(cv.results[,"Hab.accur"])
  seq.results[j,"Hab.FN"] <- mean(cv.results[,"Hab.FN"])
  seq.results[j,"Model.accur"] <- mean(cv.results[,"Model.accur"])
  
}
rm(j, i, pred, va, ct, mtry)

## Get tuned parameter
plot(seq.results, type='b', col='red')
seq.results <- seq.results[order(-seq.results[,2], seq.results[,1]),]
mtry <- seq.results[1,1]


## Tune ntrees
seq.times <- 5
k <- 5
folds <- generateCVRuns(data$P_HABITABLE, ntimes=seq.times, nfold=k, stratified=TRUE)

ntree <- seq(100,500,length.out = seq.times)
ntree <- as.integer(ntree)

seq.results <- matrix(rep(0,4*seq.times),nrow=seq.times)
colnames (seq.results) <- c("ntree","Hab.accur","Hab.FN","Model.accur")
cv.results <- matrix (rep(0,4*k),nrow=k)
colnames (cv.results) <- c("fold","Hab.accur","Hab.FN","Model.accur")

## Paramter tuning loop 
for(j in 1:seq.times) {
  seq.results[j, "ntree"] <- ntree[j]
  for(i in 1:k) {
    va <- unlist(folds[[j]][[i]])
    rf.model <- randomForest(P_HABITABLE~., data=data[-va,], mtry=mtry, ntree=ntree[j], importance=F, proximity=F)
    
    ## Get predictions accuracy measures for fold
    pred <- predict(rf.model, newdata=data[va,])
    ct <- table(Truth=data[va,target], Pred=pred)
    cv.results[i,"fold"] <- i
    cv.results[i,"Hab.accur"] <- diag(prop.table(ct, 1))[1]
    cv.results[i,"Hab.FN"] <- prop.table(ct,1)[1,2]
    cv.results[i,"Model.accur"] <- sum(diag(prop.table(ct)))
    
  }
  ## Get predictions accuracy measures
  seq.results[j,"Hab.accur"] <- mean(cv.results[,"Hab.accur"])
  seq.results[j,"Hab.FN"] <- mean(cv.results[,"Hab.FN"])
  seq.results[j,"Model.accur"] <- mean(cv.results[,"Model.accur"])
  
}
rm(j, i, k, cv.results, folds, pred, va, ct, ntree)

## Get tuned parameter
plot(seq.results, type='b', col='red')
seq.results <- seq.results[order(-seq.results[,2], seq.results[,1]),]
ntree <- seq.results[1,1]
rm(seq.results, seq.times)

## Train tuned random forest and check custom accuracy measures
k <- 5
folds <- generateCVRuns(data$P_HABITABLE, ntimes=1, nfold=k, stratified=TRUE)
cv.results <- matrix (rep(0,4*k),nrow=k)
colnames (cv.results) <- c("fold","Hab.accur","Hab.FN","Model.accur")

## CV for model accuracy
for(i in 1:k) {
  va <- unlist(folds[[1]][[i]])
  rf.model <- randomForest(P_HABITABLE~., data=data[-va,], mtry=mtry, ntree=ntree, importance=T, proximity=F)
  
  ## Get predictions accuracy measures for fold
  pred <- predict(rf.model, newdata=data[va,])
  ct <- table(Truth=data[va,target], Pred=pred)
  cv.results[i,"fold"] <- i
  cv.results[i,"Hab.accur"] <- diag(prop.table(ct, 1))[1]
  cv.results[i,"Hab.FN"] <- prop.table(ct,1)[1,2]
  cv.results[i,"Model.accur"] <- sum(diag(prop.table(ct)))
  
}
rm(i, k, folds, pred, va, ct)

## Get predictions accuracy measures
print("Habitable class accuracy:")
mean(cv.results[,"Hab.accur"])
print("Habitable false negative rate:")
mean(cv.results[,"Hab.FN"])
print("Model general accuracy:")
mean(cv.results[,"Model.accur"])




################### KNN CLASSIFIER MODEL ###################
set.seed(1234)
if(var == 'num') {
  neighbors <- 10
  kfolds <- 5
  folds <- generateCVRuns(data$P_HABITABLE, ntimes=neighbors, nfold=kfolds, stratified=TRUE)
  
  ## Create auxiliary to store values
  cv.results <- matrix (rep(0,4*kfolds),nrow=kfolds)
  colnames (cv.results) <- c("fold","Hab.accur","Hab.FN","Model.accur")
  aux <- matrix(rep(0, 4*neighbors), nrow=neighbors)
  colnames(aux)<-c("# neighbors","mean Hab.accur", "mean Hab.FN","Model.accur")
  
  ## CV tuning loops
  for(n in 1:neighbors){
    for(i in 1:kfolds) {
      va <- unlist(folds[[n]][[i]])
      knn.model.train <- knn(data[-va,-c(target)], data[va,-c(target)], data[-va,target], k = n)
      ## Get predictions accuracy measures for fold
      ct <- table(Truth=data[va,target], Pred=knn.model.train)
      cv.results[i,"fold"] <- i
      cv.results[i,"Hab.accur"] <- diag(prop.table(ct, 1))[1]
      cv.results[i,"Hab.FN"] <- prop.table(ct,1)[1,2]
      cv.results[i,"Model.accur"] <- sum(diag(prop.table(ct)))
      
    }
    ## Get predictions accuracy measures
    aux[n, "# neighbors"] <- n
    aux[n,"mean Hab.accur"] <- mean(cv.results[,2])
    aux[n, "mean Hab.FN"] <- mean(cv.results[,3])
    aux[n, "Model.accur"] <- mean(cv.results[,4])
    
  }
  rm(i, n, kfolds, va)
  
  ## Plot results
  plot(aux, type='b', col='red')
  
  ## Print resulting values
  print("Best Habitable accuracy:")
  max.acc <- max(aux[,"mean Hab.accur"])
  print(max.acc)
  print("Min Habitable false negative rate:")
  min.fn <- min(aux[,"mean Hab.FN"])
  print(min.fn)
  
  bestk <- aux[which(aux[,2] == max.acc & aux[,3] == min.fn),]
  print("Best neighbour Model general accuracy:")
  print(bestk[4])
  print("Best neighbour number for knn:")
  print(bestk[1])
}


