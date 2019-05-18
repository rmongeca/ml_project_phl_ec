data <- read.delim("data/phl_hec_all_confirmed.csv", header = TRUE, sep = ",", dec = ".", na.strings = "")
# Summary
summary(data)
# Response class distribution
table(data$P..Habitable.Class)
