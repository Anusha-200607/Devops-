# Cybersecurity Threat Propagation Simulation

## Project Overview

This project develops a synthetic cybersecurity threat propagation simulation framework using Agent-Based Modeling (ABM), Discrete Event Simulation (DES), and Markov Chain state transition modeling.

The objective is to analyze how vulnerabilities, threats, attacks, and risk levels influence the propagation of cybersecurity incidents across interconnected systems.

## Dataset

The project uses a synthetic cybersecurity dataset containing 1000 records.

The dataset includes:

- 10 vulnerability types
- 4 threat types
- 3 attack types
- Risk scores
- Compromised system status
- Network node identifiers

## Methodology

The project is divided into the following modules:

### Module 1 – Dataset Generation and Loading

Loads and prepares the synthetic cybersecurity dataset containing 1000 records.

### Module 2 – Data Preprocessing

Performs data validation, missing-value checking, risk classification, and preprocessing.

### Module 3 – ABM + DES Threat Propagation

Simulates cybersecurity threat propagation through interconnected network nodes using Agent-Based Modeling and Discrete Event Simulation.

### Module 4 – Markov State Simulation

Models transitions between the following cybersecurity states:

Safe → Exposed → Compromised → Propagating → Recovered

### Module 5 – Model Evaluation

Evaluates the simulation using:

- Accuracy
- Precision
- Recall
- Specificity
- F1 Score
- Mean Risk Score
- Standard Deviation
- Z Score
- P Value

### Module 6 – Visualization

Generates visualizations for:

- Threat propagation over time
- Threat distribution
- Attack distribution
- Vulnerability distribution
- Risk score distribution
- Risk level distribution
- Markov state transitions
- Model evaluation

### Module 7 – Report Generation

Generates a final research report using R Markdown.

## Technologies

- R
- RStudio
- R Markdown
- ggplot2
- dplyr
- tidyr
- igraph
- Agent-Based Modeling
- Discrete Event Simulation
- Markov Chain Modeling

## Project Structure

```text
ThreatPropagationProject/
│
├── DataSet/
├── Results/
├── Graphs/
├── Report/
│
├── Module1_Dataset.R
├── Module2_Preprocessing.R
├── Module3_Simulation.R
├── Module4_MCMC_Simulation.R
├── Module5_Evaluation.R
├── Module6_Final_Visualization.R
├── Module7_Report_Generation.R
│
└── README.md
