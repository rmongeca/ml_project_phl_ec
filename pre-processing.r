# Library and set-up
rm(list=ls())
set.seed(42)
library(corrplot)
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

## Turn P_HABITABLE into binary factor
data$P_HABITABLE <- as.factor(ifelse(data$P_HABITABLE > 0, "habitable", "non-habitable"))

## Remove S_RADIUS for S_RADIUS_EST
data <- data[,-which(colnames(data)=="S_RADIUS")]

## Plot ESI index and habitable planets
plot(data$P_ESI)
plot(data$P_HABITABLE)

## Corrplot between numerical variables
colnames(data)
data.cor <- cor(data[,-c(10,13,17:20)], use="complete.obs")
corrplot(data.cor)
rm(data.cor)

# Function to compute and show the most mosthighly correlated variables
mosthighlycorrelated = function(mydataframe,numtoreport)
{
    # find the correlations
    cormatrix = cor(mydataframe)
    # set the correlations on the diagonal or lower triangle to zero,
    # so they will not be reported as the highest ones:
    diag(cormatrix) = 0
    cormatrix[lower.tri(cormatrix)] = 0
    # flatten the matrix into a dataframe for easy sorting
    fm = as.data.frame(as.table(cormatrix))
    # assign human-friendly names
    names(fm) = c("First.Variable", "Second.Variable","Correlation")
    # sort and print the top n correlations
    head(fm[order(abs(fm$Correlation),decreasing=TRUE),],n=numtoreport)
}
mosthighlycorrelated(data[,-c(10,13,17:20)],10)

################### UNIVARIATE OUTLIER DETECTION ###################

## Boxplots for each feature
for(i in c(1:9,11:12,14:16)) {
  boxplot(data[,i], drop=T, main=paste("Boxplot for ",colnames(data)[i]))
}
rm(i)

## Planet mass outliers
out.p_mass <- data[which(data$P_MASS_EST > 1.5e4),]
## MASS IS WRONG for this instance

## Planet radius outliers
out.p_rad <- data[which(data$P_RADIUS_EST > 30),]
## MASS, RADIUS, SEMIAXIS WRONG for this instance

## Planet distance outliers
out.p_dist <- data[which(data$P_DISTANCE > 700),]

## Planet flux & temperature outliers
out.p_flux <- data[which(data$P_FLUX > 3e5 & data$P_TEMP_EQUIL > 5000),]

## Star temperature outliers
out.s_temp <- data[which(data$S_TEMPERATURE > 20000),]

## Star radius estimation & luminosity & habitable zone min/max outliers
out.s_lum <- data[which(data$S_RADIUS_EST > 60 & data$S_LUMINOSITY > 1000 & data$S_HZ_MIN > 25 & data$S_HZ_MAX > 60),]

drop.out <- rbind(out.p_mass, out.p_rad, out.p_dist, out.p_flux, out.s_lum, out.s_temp)

#rm(out.p_dist, out.p_flux, out.p_mass, out.p_rad, out.s_lum, out.s_temp)

################### IMPUTATION OF MISSING VALUES ########################

# impute with Multiple Imputation by Chained Equations
library(mice)

m <- mice(data, m=5, print=FALSE)
names <- rownames(data)
data <- complete(m)
rownames(data) <- names
summary(data)

# # impute with K-nearest neighborous
# # BUT, we need to deal with the categorical data
# library(class)
# 
# # first make a copy of the data to resolve errors
# planets <- data[,-c(10,13)]
# attach(planets)
# 
# aux = subset (planets, select = names(planets)[names(planets) != "P_PERIOD"])
# aux1 = aux[!is.na(planets$P_PERIOD),]
# aux2 = aux[is.na(planets$P_PERIOD),]
# 
# cl <- data_imp$P_PERIOD[!is.na(data_imp$P_PERIOD)]
# knn.inc = knn (aux1,aux2, P_PERIOD[!is.na(P_PERIOD)])



