# Library and set-up
rm(list=ls())
set.seed(42)
library(dplyr)
library(caret)
library (e1071)
library(klaR)
library(MASS)


data.all <- read.delim("data/data_all.csv", header = TRUE, sep = ",", dec = ".", na.strings = "", row.names = 1)
data.num <- read.delim("data/data_num.csv", header = TRUE, sep = ",", dec = ".", na.strings = "", row.names = 1)
data.cat <- read.delim("data/data_cat.csv", header = TRUE, sep = ",", dec = ".", na.strings = "", row.names = 1)

## Select data to mode with
#data <- data.all
data <- data.num
#data <- data.cat
  
################### LDA MODEL ###################

## LDA visualization plot
lda.model <- lda (x=data[, -c(7)], grouping=data[,7], CV=F)
loadings <- as.matrix(data[,-c(7)]) %*% as.matrix(lda.model$scaling)
boxplot(loadings~data[,7], main="Linear discriminants against classes",
        ylab="LD1 loadings",xlab="Target modalities")
## In general really bad separation between classes
## Also, we only have one LD

## Predition model
lda.model <- lda (x=data[, -c(7)], grouping=data[,7], CV=TRUE)
(ct <- table(Truth=data[,7], Pred=lda.model$class))
## Really low habitable accuracy, really high false positives
diag(prop.table(ct, 1))
print("Habitable accuracy:")
diag(prop.table(ct, 1))[1]
print("Habitable false negative rate:")
prop.table(ct,1)[1,2]

## QDA visualization plot
qda.model <- qda (x=data[, -c(7)], grouping=data[,7], CV=F)
loadings <- as.matrix(data[which(data$P_HABITABLE == 'non-habitable'),-c(7)]) %*% as.matrix(qda.model[["scaling"]][,1:2,2])
plot(loadings, col="green", ylim=c(-12,10), xlim = c(-3,6),
        main="Quadratic discriminants against classs")
loadings <- as.matrix(data[which(data$P_HABITABLE == 'habitable'),-c(7)]) %*% as.matrix(qda.model[["scaling"]][,1:2,1])
points(loadings, col="red")
## In general really good separation between classes

## Predition model
qda.model <- qda (x=data[, -c(7)], grouping=data[,7], CV=TRUE)
(ct <- table(Truth=data[,7], Pred=qda.model$class))
## Really low habitable accuracy, really high false positives
diag(prop.table(ct, 1))
print("Habitable accuracy:")
diag(prop.table(ct, 1))[1]
print("Habitable false negative rate:")
prop.table(ct,1)[1,2]

## Naive bayes
control <- trainControl(method='cv',number=10)
naive.model <- train(P_HABITABLE~., data=data, method='nb',
                     trControl=control)
