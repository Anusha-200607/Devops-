############################################################
# Threat Propagation Project
# Module 1 - Dataset Loading and Initial Analysis
############################################################

# Clear workspace
rm(list = ls())

# Load Libraries
library(readxl)
library(ggplot2)
library(dplyr)

# Read Excel Dataset
data <- read_excel("DataSet/Synthetic_Threat_Propagation_1000_Records.xlsx")

# Convert to Data Frame
data <- as.data.frame(data)

# Display first 6 rows
cat("\n========== FIRST 6 ROWS ==========\n")
print(head(data))

# Display structure
cat("\n========== DATA STRUCTURE ==========\n")
str(data)

# Display summary
cat("\n========== SUMMARY ==========\n")
summary(data)

# Display dimensions
cat("\n========== DIMENSIONS ==========\n")
print(dim(data))

# Check missing values
cat("\n========== MISSING VALUES ==========\n")
print(sum(is.na(data)))

# Display column names
cat("\n========== COLUMN NAMES ==========\n")
print(colnames(data))

# Save a CSV copy for future modules
write.csv(
  data,
  "DataSet/Synthetic_Threat_Propagation_1000_Records.csv",
  row.names = FALSE
)

cat("\nCSV file created successfully!\n")

# Histogram of Risk Score (only if column exists)
if ("Risk_Score" %in% colnames(data)) {
  
  ggplot(data, aes(x = Risk_Score)) +
    geom_histogram(
      bins = 20,
      fill = "steelblue",
      color = "black"
    ) +
    labs(
      title = "Risk Score Distribution",
      x = "Risk Score",
      y = "Frequency"
    )
  
} else {
  
  cat("\nRisk_Score column not found.\n")
  
}