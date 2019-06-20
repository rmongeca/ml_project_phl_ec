# Library and set-up
rm(list=ls())
set.seed(42)
library(dplyr)
library(missForest)

data <- read.delim("data/phl_exoplanet_catalog.csv", header = TRUE, sep = ",", dec = ".", na.strings = "")

################### INTEREST DATA SELECTION ###################

## Remove non-confirmed exoplanets
data <- data[which(data$P_STATUS == 3),]

## Remove unnecesary columns
colnames(data)
rownm <- data[,1]
drop.ind <- c(1:2,4:5,7:10,12:13,15:16,18:19,21:22,24:25,27:32,35:48,50:51,53:54,56:61,63:68,73,79:83,87:89,91:98,107:109)
drop.col <- colnames(data)[drop.ind]
data <- data[,-drop.ind]

## Assign rownames
rownames(data) <- rownm
rm(rownm)

################### COLS/ROWS WITH MISSINGS REMOVAL ###################

## See missing values percentage per column
(var.na <- apply(data, 2, function(x) 100*(sum(is.na(x))/nrow(data))))

## Change planet radius, mass and semi axis for estimates (without many missings)
## Reorder columns, first planet's properties then star's
colnames(data)
order.col <- c(37,36,38,3,19:24,13:14,25:28,31:35)
drop.col <- drop.col %>%
  append(colnames(data)[-order.col])
data <- data[,order.col]
rm(order.col)

## See missing values percentage per row
ind.na <- apply(data, 1, function(x) 100*(sum(is.na(x))/ncol(data)))
plot(ind.na)
abline(h=10, col="red")

## Drop rows with more than 10% missing values
ind.dp <- which(ind.na > 10)
drop.ind <- data[ind.dp,]
data <- data[-ind.dp,]
rm(ind.dp)

################### FURTHER PRE-PROCESSING ###################

# Summary
summary(data)
colnames(data)

## Plot ESI index and habitable planets
plot(data$P_ESI)
plot(data$P_HABITABLE)

## Missings for habitable planets
hab.pl <- data[which(data$P_HABITABLE > 0),]
hab.na <- hab.pl %>%
  apply(., 1, function(x) 100*(sum(is.na(x))/ncol(.)))
plot(hab.na)
abline(h=10, col="red")

