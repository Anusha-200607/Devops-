############################################################
# MODULE 5 - EVALUATION AND STATISTICAL ANALYSIS
############################################################

rm(list = ls())

library(ggplot2)
library(dplyr)

############################################################
# 1. LOAD ORIGINAL DATASET
############################################################

original_data <- read.csv(
  "Results/Processed_Dataset.csv"
)

############################################################
# 2. LOAD MODULE 4 RESULTS
############################################################

simulation_data <- read.csv(
  "Results/MCMC_Final_State.csv"
)

cat("Original Dataset Records:",
    nrow(original_data), "\n")

cat("Simulation Dataset Records:",
    nrow(simulation_data), "\n")

############################################################
# 3. CREATE ACTUAL LABEL
############################################################

# Original compromised status
# 0 = Safe
# 1 = Compromised

actual <- ifelse(
  original_data$Compromised == 1,
  "Compromised",
  "Safe"
)

############################################################
# 4. CREATE PREDICTED LABEL
############################################################

# Consider Compromised and Propagating
# as positive threat states

predicted <- ifelse(
  simulation_data$Current_State %in%
    c("Compromised", "Propagating"),
  "Compromised",
  "Safe"
)

############################################################
# 5. CREATE CONFUSION MATRIX
############################################################

confusion_matrix <- table(
  Actual = actual,
  Predicted = predicted
)

cat("\n========== CONFUSION MATRIX ==========\n")

print(confusion_matrix)

############################################################
# 6. EXTRACT VALUES
############################################################

TP <- sum(
  actual == "Compromised" &
    predicted == "Compromised"
)

TN <- sum(
  actual == "Safe" &
    predicted == "Safe"
)

FP <- sum(
  actual == "Safe" &
    predicted == "Compromised"
)

FN <- sum(
  actual == "Compromised" &
    predicted == "Safe"
)

############################################################
# 7. CALCULATE METRICS
############################################################

accuracy <- (TP + TN) /
  (TP + TN + FP + FN)

precision <- TP /
  ifelse((TP + FP) == 0, 1, TP + FP)

recall <- TP /
  ifelse((TP + FN) == 0, 1, TP + FN)

specificity <- TN /
  ifelse((TN + FP) == 0, 1, TN + FP)

f1_score <- 2 *
  (precision * recall) /
  ifelse(
    (precision + recall) == 0,
    1,
    precision + recall
  )

############################################################
# 8. DISPLAY METRICS
############################################################

cat("\n========== EVALUATION METRICS ==========\n")

cat("True Positive  :", TP, "\n")
cat("True Negative  :", TN, "\n")
cat("False Positive :", FP, "\n")
cat("False Negative :", FN, "\n")

cat("\nAccuracy       :", round(accuracy, 4), "\n")
cat("Precision      :", round(precision, 4), "\n")
cat("Recall         :", round(recall, 4), "\n")
cat("Specificity    :", round(specificity, 4), "\n")
cat("F1 Score       :", round(f1_score, 4), "\n")

############################################################
# 9. MEAN AND STANDARD DEVIATION
############################################################

mean_risk <- mean(
  original_data$Risk_Score
)

sd_risk <- sd(
  original_data$Risk_Score
)

cat("\n========== RISK STATISTICS ==========\n")

cat(
  "Mean Risk Score:",
  round(mean_risk, 3),
  "\n"
)

cat(
  "Standard Deviation:",
  round(sd_risk, 3),
  "\n"
)

############################################################
# 10. Z-TEST FOR RISK SCORE
############################################################

# Test whether mean risk score
# differs from neutral risk value 5

z_score <- (
  mean_risk - 5
) / (
  sd_risk / sqrt(nrow(original_data))
)

p_value_z <- 2 *
  (1 - pnorm(abs(z_score)))

cat("\n========== Z-TEST ==========\n")

cat(
  "Z-Score:",
  round(z_score, 4),
  "\n"
)

cat(
  "P-Value:",
  round(p_value_z, 6),
  "\n"
)

############################################################
# 11. CREATE RESULTS TABLE
############################################################

evaluation_results <- data.frame(
  
  Metric = c(
    "Accuracy",
    "Precision",
    "Recall",
    "Specificity",
    "F1_Score",
    "Mean_Risk",
    "SD_Risk",
    "Z_Score",
    "P_Value"
  ),
  
  Value = c(
    accuracy,
    precision,
    recall,
    specificity,
    f1_score,
    mean_risk,
    sd_risk,
    z_score,
    p_value_z
  )
  
)

############################################################
# 12. SAVE EVALUATION RESULTS
############################################################

write.csv(
  evaluation_results,
  "Results/Evaluation_Results.csv",
  row.names = FALSE
)

############################################################
# 13. SAVE CONFUSION MATRIX
############################################################

write.csv(
  as.data.frame(confusion_matrix),
  "Results/Confusion_Matrix.csv",
  row.names = FALSE
)

############################################################
# 14. CREATE METRICS GRAPH
############################################################

metric_data <- data.frame(
  
  Metric = c(
    "Accuracy",
    "Precision",
    "Recall",
    "Specificity",
    "F1 Score"
  ),
  
  Score = c(
    accuracy,
    precision,
    recall,
    specificity,
    f1_score
  )
  
)

ggplot(
  metric_data,
  aes(
    x = Metric,
    y = Score
  )
) +
  
  geom_col() +
  
  geom_text(
    aes(
      label = round(Score, 3)
    ),
    vjust = -0.5
  ) +
  
  ylim(0, 1.1) +
  
  labs(
    title = "Threat Propagation Model Evaluation",
    x = "Evaluation Metric",
    y = "Score"
  )

############################################################
# 15. RISK SCORE DISTRIBUTION
############################################################

ggplot(
  original_data,
  aes(
    x = Risk_Score
  )
) +
  
  geom_histogram(
    bins = 20
  ) +
  
  labs(
    title = "Risk Score Distribution",
    x = "Risk Score",
    y = "Frequency"
  )

############################################################
# 16. COMPLETION MESSAGE
############################################################

cat(
  "\n========================================\n"
)

cat(
  "MODULE 5 COMPLETED SUCCESSFULLY\n"
)

cat(
  "========================================\n"
)