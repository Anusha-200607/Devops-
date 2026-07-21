############################################################
# MODULE 2 - DATA PREPROCESSING
############################################################

rm(list = ls())

library(readr)
library(dplyr)
library(ggplot2)

# Read dataset
data <- read.csv("DataSet/Synthetic_Threat_Propagation_1000_Records.csv")

# Display basic information
head(data)
str(data)
summary(data)

# Missing values
cat("Missing Values:", sum(is.na(data)), "\n")

# Convert categorical variables
data$Threat <- as.factor(data$Threat)
data$Attack <- as.factor(data$Attack)
data$Compromised <- as.factor(data$Compromised)

# Count number of vulnerabilities
data$Number_of_Vulnerabilities <-
  sapply(strsplit(data$Vulnerabilities, ";"), length)

# Create Risk Level
data$Risk_Level <- cut(
  data$Risk_Score,
  breaks = c(0,3,6,8,10),
  labels = c("Low","Medium","High","Critical")
)

# Save processed dataset
write.csv(
  data,
  "Results/Processed_Dataset.csv",
  row.names = FALSE
)

cat("Processed Dataset Saved Successfully\n")

# Threat Frequency
print(table(data$Threat))

# Attack Frequency
print(table(data$Attack))

# Risk Level Frequency
print(table(data$Risk_Level))

# Compromised Frequency
print(table(data$Compromised))