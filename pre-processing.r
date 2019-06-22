# Library and set-up
rm(list=ls())
set.seed(42)
library(corrplot)
library(dplyr)
library(DMwR)
library(FSelector)
library(mice)
library(car)
library(randomForest)

data <- read.delim("data/phl_exoplanet_catalog.csv", header = TRUE, sep = ",", dec = ".", na.strings = "")

## Variables to select datasets to generate
## (will generate whole data, training and test for given parameters)
oversampled <- FALSE
var <- 'all'
## Variable to select imputation method
imp.method <- 'knn'

################### INTEREST DATA SELECTION ###################

## Remove non-confirmed exoplanets
data <- data[which(data$P_STATUS == 3),]

## Remove unnecesary columns
colnames(data)
rownm <- data[,1]
d.index <- c(1:2,4:5,7:10,12:13,15:16,18:19,21:22,24:25,27:32,34:48,50:51,53:54,56:61,63:68,73,77,79:80,82:83,87:89,93:101,107:109)
drop.col <- colnames(data)[d.index]
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

## Plot habitable planets
plot(data$P_HABITABLE)

## Remove unnecessary target variables
data <- data[,-which(colnames(data) %in% c("P_HABZONE","P_TYPE_TEMP","P_ESI"))]

################### UNIVARIATE OUTLIER DETECTION ################### 

## Boxplots for each feature
non.numeric <- which(colnames(data) %in% c("P_TYPE","P_TYPE_TEMP","S_TYPE_TEMP","P_HABZONE","P_HABITABLE","P_ESI"))
num <- 1:ncol(data)
for(i in num[-non.numeric]) {
  boxplot(data[,i], drop=T, main=paste("Boxplot for ",colnames(data)[i]))
}
rm(i,num)

## Drop unrealistic/incorrect or out-of-scale rows for each feature
## Planet mass outliers
out.p_mass <- data[which(data$P_MASS_EST < 1e-4 | data$P_MASS_EST > 1.5e4),]
## MASS IS WRONG for this instance
## Planet radius outliers
out.p_rad <- data[which(data$P_RADIUS_EST < 1e-4 | data$P_RADIUS_EST > 30),]
## MASS, RADIUS, SEMIAXIS WRONG for this instance
## Planet distance outliers
out.p_dist <- data[which(data$P_SEMI_MAJOR_AXIS_EST < 1e-4 | data$P_SEMI_MAJOR_AXIS_EST > 700),]
## Planet period outliers
out.p_period <- data[which(data$P_PERIOD > 3e4),]
## Planet flux & temperature outliers
out.p_flux <- data[which(data$P_FLUX > 2e4 & data$P_TEMP_EQUIL > 3000),]
## Star temperature outliers
out.s_temp <- data[which(data$S_TEMPERATURE > 20000),]
## Star radius estimation & luminosity & habitable zone min/max outliers
out.s_lum <- data[which(data$S_RADIUS_EST > 60 & data$S_LUMINOSITY > 1000 & data$S_HZ_MIN > 25 & data$S_HZ_MAX > 60),]
## Joined dropped instances, even repeated ones
drop.out <- rbind(out.p_mass, out.p_rad, out.p_dist, out.p_period, out.p_flux, out.s_lum, out.s_temp)
## Drop from dataset
data <- data[!rownames(data) %in% rownames(drop.out),]
## Removed used variables
rm(out.p_dist, out.p_flux, out.p_mass, out.p_rad, out.s_lum, out.s_temp, out.p_period)

################### FEATURE TRANSFOMRATION ################### 

## Transform variables with huge range in to logarithm of variables
data$P_MASS_EST <- log(data$P_MASS_EST + 1)
data$P_PERIOD <- log(data$P_PERIOD + 1)
data$P_SEMI_MAJOR_AXIS_EST <- log(data$P_SEMI_MAJOR_AXIS_EST + 1)
data$P_DISTANCE <- log(data$P_DISTANCE + 1)
data$P_APASTRON <- log(data$P_APASTRON + 1)
data$P_PERIASTRON <- log(data$P_PERIASTRON + 1)
data$P_FLUX <- log(data$P_FLUX + 1)
data$P_TEMP_EQUIL <- log(data$P_TEMP_EQUIL + 1)
data$S_TEMPERATURE <- log(data$S_TEMPERATURE + 1)
data$S_LUMINOSITY <- log(data$S_LUMINOSITY + 1)

## Boxplots for each feature
non.numeric <- which(colnames(data) %in% c("P_TYPE","P_TYPE_TEMP","S_TYPE_TEMP","P_HABZONE","P_HABITABLE","P_ESI"))
num <- 1:ncol(data)
for(i in num[-non.numeric]) {
  boxplot(data[,i], drop=T, main=paste("Boxplot for ",colnames(data)[i]))
}
rm(i,num)

################### IMPUTATION OF MISSING VALUES ########################

## Impute with Multiple Imputation by Chained Equations
if(imp.method == 'mice') {
  m <- mice(data, m=5, print=FALSE)
  names <- rownames(data)
  data <- complete(m)
  rownames(data) <- names
  summary(data)  
}
## Impute with K nearest neighbours
if(imp.method == 'knn') {
  data <- knnImputation(data, k=1, scale=T, meth = 'weighAvg')
  summary(data)
}

################### FEATURE SELECTION ########################

## Corrplot between numerical variables
non.numeric <- which(colnames(data) %in% c("P_TYPE","P_TYPE_TEMP","S_TYPE_TEMP","P_HABZONE","P_HABITABLE","P_ESI"))
data.cor <- cor(data[,-non.numeric], use="complete.obs")
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
mosthighlycorrelated(data[,-non.numeric],20)

var.sel <- which(colnames(data) %in% c("P_MASS_EST","P_SEMI_MAJOR_AXIS_EST","P_TEMP_EQUIL", "P_PERIOD","S_LUMINOSITY","S_RADIUS_EST"))
var.sel.all <- which(colnames(data) %in% c("P_TYPE","P_SEMI_MAJOR_AXIS_EST","P_TEMP_EQUIL", "P_PERIOD","S_TYPE_TEMP","S_RADIUS_EST"))

if(selection == 'num') {
  hab <- data$P_HABITABLE
  data <- data[,var.sel]
  data$P_HABITABLE <- hab
  rm(hab)
}
if(selection == 'cat') {
  hab <- data$P_HABITABLE
  data <- data[,var.sel.all]
  data$P_HABITABLE <- hab
  rm(hab)
}

## Standardize the data, non.numeric ones
for (i in 1:ncol(data)) {
  if (!is.factor(data[,i])) {
    data[,i] <- scale(data[,i])
  }
}
rm(i)

################### SAMPLE SELECTION/SPLITTING ########################

## Synthetic Minority Oversample Technique (SMOTE)
if(oversample == T) {
  data <- SMOTE(P_HABITABLE~., data = data, perc.over = 500, perc.under = 1200)
}

# create training and test sample
# 66% trauning and rest test
summary(data)
sample.size <- floor(0.66 *nrow(data)) 
train <- sample(seq_len(nrow(data)), size = sample.size)

data.training <- data[train, ]
data.test <- data[-train, ]


# (subset.CFS <- cfs (P_HABITABLE~., data))
# (subset.Consistency <- consistency (P_HABITABLE~., data))
# 
# glm.model <- glm (P_HABITABLE~., family = binomial(link=logit), data = data)
# (glm.model.form <- step(glm.model)$formula)
# 
# (rf.importace <- random.forest.importance(P_HABITABLE~., data, importance.type = 1))
# 
if(oversample == F) {
  write.csv(data, file = paste("data/data_", selection,".csv", sep = ""))
  write.csv(data.training, file = paste("data/training_", selection, ".csv", sep = ""))
  write.csv(data.test, file = paste("data/test_", selection, ".csv", sep = ""))  
}
if(oversample == T) {
  write.csv(data, file = paste("data/smote/data_", selection,".csv", sep = ""))
  write.csv(data.training, file = paste("data/smote/training_", selection, ".csv", sep = ""))
  write.csv(data.test, file = paste("data/smote/test_", selection, ".csv", sep = ""))
}

