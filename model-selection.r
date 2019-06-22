# Library and set-up
rm(list=ls())
set.seed(42)
library(class)
library(dplyr)
library (e1071)
library(klaR)
library(MASS)
library(randomForest)
library(TunePareto)


data.all <- read.delim("data/data_all.csv", header = TRUE, sep = ",", dec = ".", na.strings = "", row.names = 1)
data.num <- read.delim("data/data_num.csv", header = TRUE, sep = ",", dec = ".", na.strings = "", row.names = 1)
data.cat <- read.delim("data/data_cat.csv", header = TRUE, sep = ",", dec = ".", na.strings = "", row.names = 1)

## Select data to mode with
var <- 'num'
data <- data.all
if(var == 'num') {
  data <- data.num
}
if(var == 'cat') {
  data <- data.cat
}

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
diag(prop.table(ct, 1))
print("Habitable accuracy:")
diag(prop.table(ct, 1))[1]
print("Habitable false negative rate:")
prop.table(ct,1)[1,2]

################### QDA MODEL ###################

## QDA visualization plot
set.seed(42)
if(var == 'num') {
  qda.model <- qda (x=data[, -c(target)], grouping=data[,target], CV=F)
  loadings <- as.matrix(data[which(data$P_HABITABLE == 'non-habitable'),-c(target)]) %*% as.matrix(qda.model[["scaling"]][,1:2,2])
  plot(loadings, col="green", ylim=c(-12,10), xlim = c(-3,6),
          main="Quadratic discriminants against classs")
  loadings <- as.matrix(data[which(data$P_HABITABLE == 'habitable'),-c(target)]) %*% as.matrix(qda.model[["scaling"]][,1:2,1])
  points(loadings, col="red")
  ## In general really good separation between classes
}

## Predition model
qda.model <- qda (x=data[, -c(target)], grouping=data[,target], CV=TRUE)
(ct <- table(Truth=data[,target], Pred=qda.model$class))
## Really low habitable accuracy, really high false positives
diag(prop.table(ct, 1))
print("Habitable accuracy:")
diag(prop.table(ct, 1))[1]
print("Habitable false negative rate:")
prop.table(ct,1)[1,2]

################### NAIVE BAYES MODEL ###################

set.seed(42)
k <- 10
folds <- generateCVRuns(data$P_HABITABLE, ntimes=1, nfold=k, stratified=TRUE)
cv.results <- matrix (rep(0,3*k),nrow=k)
colnames (cv.results) <- c("fold","Hab.accur","Hab.FN")

## CV loop for accuracy measures
for(i in 1:k) {
  va <- unlist(folds[[1]][[i]])
  naive.model <- naiveBayes(P_HABITABLE~., data=data[-va,])
  
  ## Get predictions accuracy measures for fold
  pred <- predict(naive.model, newdata=data[va,])
  ct <- table(Truth=data[va,target], Pred=pred)
  cv.results[i,"Hab.accur"] <- diag(prop.table(ct, 1))[1]
  cv.results[i,"Hab.FN"] <- prop.table(ct,1)[1,2]
  cv.results[i,"fold"] <- i
  
}
rm(i, k, folds, pred, va)

## Get predictions accuracy measures
print("Habitable accuracy:")
mean(cv.results[,"Hab.accur"])
print("Habitable false negative rate:")
mean(cv.results[,"Hab.FN"])
  

################### RANDOM FOREST MODEL ###################

set.seed(42)
seq.times <- 5
k <- 3
folds <- generateCVRuns(data$P_HABITABLE, ntimes=seq.times, nfold=k, stratified=TRUE)

## Tune mtry number of predictors
mtry <- seq(1,ncol(data)-1,length.out = seq.times)
mtry <- unique(as.integer(mtry))
seq.results <- matrix(rep(0,3*seq.times),nrow=seq.times)
colnames (seq.results) <- c("Mtry","Hab.accur","Hab.FN")
cv.results <- matrix (rep(0,3*k),nrow=k)
colnames (cv.results) <- c("fold","Hab.accur","Hab.FN")

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
    
  }
  ## Get predictions accuracy measures
  seq.results[j,"Hab.accur"] <- diag(prop.table(ct, 1))[1]
  seq.results[j,"Hab.FN"] <- prop.table(ct,1)[1,2]
  
}
rm(j, i, pred, va, ct, mtry)

## Get tuned parameter
plot(seq.results, type='b', col='red')
seq.results <- seq.results[order(-seq.results[,2]),]
mtry <- seq.results[1,1]


## Tune ntrees
ntree <- seq(100,1000,length.out = seq.times)
ntree <- unique(as.integer(ntree))
colnames (seq.results) <- c("ntree","Hab.accur","Hab.FN")

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
  }
  ## Get predictions accuracy measures
  seq.results[j,"Hab.accur"] <- diag(prop.table(ct, 1))[1]
  seq.results[j,"Hab.FN"] <- prop.table(ct,1)[1,2]
  
}
rm(j, i, k, cv.results, folds, pred, va, ct, ntree)

## Get tuned parameter
plot(seq.results, type='b', col='red')
seq.results <- seq.results[order(-seq.results[,2]),]
ntree <- seq.results[1,1]
rm(seq.results, seq.times)

## Train tuned random forest and check custom accuracy measures
set.seed(42)
k <- 10
folds <- generateCVRuns(data$P_HABITABLE, ntimes=1, nfold=k, stratified=TRUE)
cv.results <- matrix (rep(0,3*k),nrow=k)
colnames (cv.results) <- c("fold","Hab.accur","Hab.FN")

## CV for model accuracy
for(i in 1:k) {
  va <- unlist(folds[[1]][[i]])
  rf.model <- randomForest(P_HABITABLE~., data=data[-va,], mtry=mtry, ntree=ntree, importance=F, proximity=F)
  
  ## Get predictions accuracy measures for fold
  pred <- predict(rf.model, newdata=data[va,])
  ct <- table(Truth=data[va,target], Pred=pred)
  cv.results[i,"fold"] <- i
  cv.results[i,"Hab.accur"] <- diag(prop.table(ct, 1))[1]
  cv.results[i,"Hab.FN"] <- prop.table(ct,1)[1,2]
  
}
rm(i, k, folds, pred, va, ct)

## Get predictions accuracy measures
print("Habitable accuracy:")
mean(cv.results[,"Hab.accur"])
print("Habitable false negative rate:")
mean(cv.results[,"Hab.FN"])




################### KNN CLASSIFIER MODEL ###################
set.seed(42)
if(var == 'num') {
  neighbors <- 15
  kfolds <- 10
  folds <- generateCVRuns(data$P_HABITABLE, ntimes=neighbors, nfold=kfolds, stratified=TRUE)
  cv.results <- matrix (rep(0,3*kfolds),nrow=kfolds)
  colnames (cv.results) <- c("fold","Hab.accur","Hab.FN")
  
  ## Create auxiliary to store values
  aux <- matrix(rep(0, 3*neighbors), nrow=neighbors)
  colnames(aux)<-c("# neighbors","mean Hab.accur", "mean Hab.FN")
  aux[,"mean Hab.accur"]<-0
  aux[,"mean Hab.FN"]<-0
  
  ## CV tuning loops
  for(n in 1:neighbors){
    for(i in 1:kfolds) {
      va <- unlist(folds[[n]][[i]])
      knn.model.train <- knn(data[-va,-c(target)], data[va,-c(target)], data[-va,target], k = n)
      ## Get predictions accuracy measures for fold
      ct <- table(Truth=data[va,target], Pred=knn.model.train)
      cv.results[i,"Hab.accur"] <- diag(prop.table(ct, 1))[1]
      cv.results[i,"Hab.FN"] <- prop.table(ct,1)[1,2]
      cv.results[i,"fold"] <- i
      
    }
    ## Get predictions accuracy measures
    aux[n, "# neighbors"] <- n
    aux[n,"mean Hab.accur"] <- mean(cv.results[,2])
    aux[n, "mean Hab.FN"] <- mean(cv.results[,3])
    
  }
  rm(i, n, kfolds, va)
  ## Print resulting values
  print("Best Habitable accuracy:")
  max.acc <- max(aux[,"mean Hab.accur"])
  print(max.acc)
  print("Min Habitable false negative rate:")
  min.fn <- min(aux[,"mean Hab.FN"])
  print(min.fn)
  
  # plot results
  plot(aux, type='b', col='red')
  
  (bestk <- aux[which(aux[,2] == max.acc & aux[,3] == min.fn),1])
}


