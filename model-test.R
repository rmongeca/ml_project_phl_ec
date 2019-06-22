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
var <- 'cat'

## Get taining data according to dataset decision
path <- "data/"
if(oversampled == T) {
  path <- paste(path,"smote/",sep="")
}
data <- read.delim(paste(path,"test_", var, ".csv",sep=""),
                   header = TRUE, sep = ",", dec = ".", na.strings = "", row.names = 1)

## Get target variable index (depends on chosen dataset)
target <- grep("P_HABITABLE", colnames(data))

