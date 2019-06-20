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
d.index <- c(1:2,4:5,7:10,12:13,15:16,18:19,21:22,24:25,27:32,34:48,50:51,53:54,56:61,63:68,73,77,79:80,82:83,87:89,93:101,107:109)
drop.col <- as.data.frame(colnames(data)[d.index])
data <- data[,-d.index]
rm(d.index)

## Assign rownames
rownames(data) <- rownm
rm(rownm)

################### COLS/ROWS WITH MISSINGS REMOVAL ###################

## Change planet radius, mass and semi axis for estimates (without many missings)
## Reorder columns, first planet's properties then star's
colnames(data)
order.col <- c(35,34,36,3,5:9,14:23,10:13,24:28,29:33)
drop.col <- drop.col %>%
  append(colnames(data)[-order.col])
data <- data[,order.col]
rm(order.col)

## See missing values percentage per column
(var.na <- apply(data, 2, function(x) 100*(sum(is.na(x))/nrow(data))))
plot(var.na)
abline(h=10, col="red")
## Remove columns with more than 10% missings
col.dp <- which(var.na > 10)
drop.col <- drop.col %>%
  append(colnames(data)[col.dp])
data <- data[,-col.dp]
rm(col.dp, var.na)

## See missing values percentage per row
ind.na <- apply(data, 1, function(x) 100*(sum(is.na(x))/ncol(data)))
plot(ind.na)
abline(h=10, col="red")
## Drop rows with more than 10% missing values
ind.dp <- which(ind.na > 10)
drop.ind <- data[ind.dp,]
data <- data[-ind.dp,]
rm(ind.dp, ind.na)

################### FURTHER PRE-PROCESSING ###################

# Summary
summary(data)
colnames(data)

## Join P_HABITABLE AS BINARY FACTOR
data$P_HABZONE_OPT <- as.factor(ifelse(data$P_HABZONE_CON > 0 | data$P_HABZONE_OPT > 0, 1, 0))
colnames(data)[which(colnames(data)=="S_HZ_OPT_MIN")] <- "S_HZ_MIN"
colnames(data)[which(colnames(data)=="S_HZ_OPT_MAX")] <- "S_HZ_MAX"
colnames(data)[which(colnames(data)=="P_HABZONE_OPT")] <- "P_HABZONE"
data <- data[,-which(colnames(data)=="P_HABZONE_CON")]

## Turn P_HABITABLE into factor
data$P_HABITABLE <- as.factor(data$P_HABITABLE)

## Plot ESI index and habitable planets
plot(data$P_ESI)
plot(data$P_HABITABLE)

## Missings for habitable planets
hab.pl <- data[which(data$P_HABITABLE > 0),]
