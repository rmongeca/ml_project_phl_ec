# Library and set-up
rm(list=ls())
set.seed(42)
library(missForest)

data <- read.delim("data/phl_hec_all_confirmed.csv", header = TRUE, sep = ",", dec = ".", na.strings = "")

# Summary
summary(data)
colnames(data)
rownames(data) <- data$P..Name

## Drop useless variables
no.variables <- c(1:3,9,11,22:24,26:27,32:38,43:50,59,65:68)
data <- data[,-no.variables]

## Impute missing values
#imp <- missForest(data) ## Crashes

## Separate each class' instances
nonhab <- data[which(data$P..Habitable == 0),]
habitab <- data[which(data$P..Habitable == 1),]

## Check missings for all data and for every class
apply(data, 2, function(x) round(sum(is.na(x))*100/nrow(data), digits = 2))
apply(nonhab, 2, function(x) round(sum(is.na(x))*100/nrow(nonhab), digits = 2))
apply(habitab, 2, function(x) round(sum(is.na(x))*100/nrow(habitab), digits = 2))
