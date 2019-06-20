# Library and set-up
rm(list=ls())
set.seed(42)
library(missForest)

data <- read.delim("data/phl_exoplanet_catalog.csv", header = TRUE, sep = ",", dec = ".", na.strings = "")

# Summary
summary(data)
colnames(data)
rownames(data) <- data$P..Name

## Drop useless variables
#no.info.variables <- c(1:3,11,22:24,26:27,32:38,43:50,59,65:68)
#data <- data[,-no.info.variables]
#colnames(data)
#reduced.variables <- c(6,9:10, 12:14, 16, 19:22, 24:27, 37:39)
#reduced <- data[,reduced.variables]
#summary(reduced)

## Check missings for all data and for every class
apply(data, 2, function(x) round(sum(is.na(x))*100/nrow(data), digits = 2))
## Plot ESI index 
plot(data$P..ESI)
