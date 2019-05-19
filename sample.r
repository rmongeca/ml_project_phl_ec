# Library and set-up
rm(list=ls())
set.seed(42)

data <- read.delim("data/phl_hec_all_confirmed.csv", header = TRUE, sep = ",", dec = ".", na.strings = "")

# Summary
summary(data)
colnames(data)

## Drop useless variables
no.variables <- c(1:3,9,11,26:27,32:37,45:50,65:68)
data <- data[,-no.variables]

## Separate each class' instances
nonhab <- data[which(data$P..Habitable == 0),]
habitab <- data[which(data$P..Habitable == 1),]

## Check missings of every class
apply(data, 2, function(x) round(sum(is.na(x))*100/nrow(data), digits = 2))
apply(nonhab, 2, function(x) round(sum(is.na(x))*100/nrow(nonhab), digits = 2))
apply(habitab, 2, function(x) round(sum(is.na(x))*100/nrow(habitab), digits = 2))
