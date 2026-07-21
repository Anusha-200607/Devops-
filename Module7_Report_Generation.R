---
  title: "Threat Propagation Simulation Using Hybrid ABM, DES and Markov Chain Modeling"
author: "Anusha Bhaskar"
date: "`r Sys.Date()`"
output:
  html_document:
  toc: true
toc_float: true
---
  
  # 1. Introduction
  
  Cybersecurity threats can propagate through interconnected computer networks when vulnerable systems are compromised. This project develops a synthetic threat propagation simulation model to study how vulnerabilities, threats, and attacks influence the spread of cyber incidents.

The proposed framework combines Agent-Based Modeling (ABM), Discrete Event Simulation (DES), and probabilistic Markov state transitions to model threat propagation over time.

# 2. Objectives

The main objectives of this project are:
  
  - To generate a synthetic cybersecurity dataset containing 1000 records.
- To represent multiple vulnerabilities, threats, and attack types.
- To preprocess and classify cybersecurity risk.
- To simulate threat propagation through an interconnected network.
- To model changes in threat states over time.
- To evaluate the performance of the simulation.
- To visualize cybersecurity threat propagation and risk patterns.

# 3. Dataset Description

The synthetic dataset contains 1000 cybersecurity records.

The main variables include:
  
  - Record ID
- Node ID
- Vulnerabilities
- Threat
- Attack
- Risk Score
- Compromised Status

```{r dataset-summary, echo=FALSE}

data <- read.csv(
  "../Results/Processed_Dataset.csv"
)

cat("Number of records:", nrow(data))
cat("\nNumber of variables:", ncol(data))

```

# 4. Data Preprocessing

The dataset was processed by:
  
  - Checking missing values.
- Converting categorical variables.
- Counting vulnerabilities.
- Creating risk-level categories.
- Saving the processed dataset.

```{r preprocessing, echo=FALSE}

summary(data)

```

# 5. Threat Distribution

The following figure shows the distribution of threat types in the synthetic dataset.

```{r threat-distribution, echo=FALSE, fig.width=8, fig.height=5}

library(ggplot2)

threat_data <- data |>
  dplyr::count(Threat)

ggplot(
  threat_data,
  aes(
    x = reorder(Threat, n),
    y = n
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Threat Type Distribution",
    x = "Threat Type",
    y = "Number of Records"
  ) +
  theme_minimal()

```

# 6. Attack Distribution

```{r attack-distribution, echo=FALSE, fig.width=8, fig.height=5}

attack_data <- data |>
  dplyr::count(Attack)

ggplot(
  attack_data,
  aes(
    x = reorder(Attack, n),
    y = n
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Attack Type Distribution",
    x = "Attack Type",
    y = "Number of Records"
  ) +
  theme_minimal()

```

# 7. Risk Score Distribution

```{r risk-distribution, echo=FALSE, fig.width=8, fig.height=5}

ggplot(
  data,
  aes(x = Risk_Score)
) +
  geom_histogram(
    bins = 20
  ) +
  labs(
    title = "Risk Score Distribution",
    x = "Risk Score",
    y = "Frequency"
  ) +
  theme_minimal()

```

# 8. Risk Level Distribution

```{r risk-level, echo=FALSE, fig.width=8, fig.height=5}

risk_data <- data |>
  dplyr::count(Risk_Level)

ggplot(
  risk_data,
  aes(
    x = Risk_Level,
    y = n
  )
) +
  geom_col() +
  labs(
    title = "Risk Level Distribution",
    x = "Risk Level",
    y = "Number of Records"
  ) +
  theme_minimal()

```

# 9. Threat Propagation Simulation

The simulation models the propagation of cyber threats through interconnected nodes. The ABM component represents individual nodes as agents, while the DES component represents propagation across discrete simulation time steps.

```{r propagation, echo=FALSE}

timeline <- read.csv(
  "../Results/Propagation_Timeline.csv"
)

```

```{r propagation-plot, echo=FALSE, fig.width=8, fig.height=5}

ggplot(
  timeline,
  aes(
    x = Time,
    y = Infected
  )
) +
  geom_line(
    linewidth = 1
  ) +
  geom_point() +
  labs(
    title = "Threat Propagation Over Time",
    x = "Simulation Time Step",
    y = "Number of Infected Nodes"
  ) +
  theme_minimal()

```

# 10. Markov Threat State Simulation

The probabilistic state model represents cybersecurity states as:
  
  **Safe → Exposed → Compromised → Propagating → Recovered**
  
  ```{r markov-data, echo=FALSE}

mcmc_history <- read.csv(
  "../Results/MCMC_State_History.csv"
)

```

```{r markov-plot, echo=FALSE, fig.width=9, fig.height=5}

mcmc_long <- tidyr::pivot_longer(
  mcmc_history,
  cols = c(
    Safe,
    Exposed,
    Compromised,
    Propagating,
    Recovered
  ),
  names_to = "State",
  values_to = "Count"
)

ggplot(
  mcmc_long,
  aes(
    x = Time,
    y = Count,
    group = State,
    color = State
  )
) +
  geom_line(
    linewidth = 1
  ) +
  labs(
    title = "Threat State Transition Over Time",
    x = "Simulation Time",
    y = "Number of Nodes"
  ) +
  theme_minimal()

```

# 11. Model Evaluation

The model was evaluated using accuracy, precision, recall, specificity, and F1-score.

```{r evaluation, echo=FALSE}

evaluation <- read.csv(
  "../Results/Evaluation_Results.csv"
)

knitr::kable(
  evaluation,
  digits = 4,
  caption = "Model Evaluation Results"
)

```

# 12. Statistical Analysis

The risk score statistics and statistical test results are presented below.

```{r statistics, echo=FALSE}

evaluation |>
  dplyr::filter(
    Metric %in% c(
      "Mean_Risk",
      "SD_Risk",
      "Z_Score",
      "P_Value"
    )
  ) |>
  knitr::kable(
    digits = 4,
    caption = "Statistical Analysis Results"
  )

```

# 13. Results and Discussion

The simulation demonstrates how cybersecurity threats can propagate through interconnected systems. The combination of risk-based modeling and probabilistic state transitions provides a framework for studying the evolution of cyber threats.

The results can be used to identify high-risk systems, analyze propagation patterns, and understand how vulnerabilities influence the probability of compromise.

# 14. Conclusion

This project developed a synthetic cybersecurity threat propagation framework using ABM, DES, and probabilistic Markov state modeling. The framework provides a structured approach for analyzing cybersecurity threats, attack patterns, vulnerabilities, and risk propagation.

Future work can include the use of advanced synthetic data generation techniques such as Conditional Tabular GANs (CTGAN), larger enterprise network topologies, real-world cybersecurity datasets, and more advanced Bayesian or MCMC inference methods.

# 15. Generated Project Files

The project produces:
  
  - Processed dataset
- Threat propagation results
- Propagation timeline
- Markov state history
- Model evaluation results
- Confusion matrix
- Visualization graphs