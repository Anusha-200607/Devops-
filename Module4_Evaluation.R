############################################################
# MODULE 4
# MARKOV CHAIN THREAT STATE SIMULATION
############################################################

# Clear workspace
rm(list = ls())

############################################################
# Load Libraries
############################################################

library(ggplot2)
library(dplyr)

############################################################
# Load Processed Dataset
############################################################

data <- read.csv(
  "Results/Processed_Dataset.csv"
)

cat("Dataset Loaded Successfully\n")
cat("Number of Records:", nrow(data), "\n")

############################################################
# Define Threat States
############################################################

states <- c(
  "Safe",
  "Exposed",
  "Compromised",
  "Propagating",
  "Recovered"
)

############################################################
# Initialize State
############################################################

# Start all nodes as Safe
data$Current_State <- "Safe"

# Nodes marked as compromised in the original dataset
# are initially considered Compromised

data$Current_State[
  data$Compromised == 1
] <- "Compromised"

############################################################
# Risk-Based Transition Probability
############################################################

# Function to calculate transition probabilities

calculate_transition <- function(
    current_state,
    risk_score
){
  
  # Normalize risk score
  risk <- min(
    max(risk_score / 10, 0),
    1
  )
  
  ##########################################################
  # SAFE
  ##########################################################
  
  if(current_state == "Safe"){
    
    return(
      c(
        Safe = 0.75 - (0.20 * risk),
        Exposed = 0.20 + (0.15 * risk),
        Compromised = 0.05 + (0.05 * risk),
        Propagating = 0,
        Recovered = 0
      )
    )
    
  }
  
  ##########################################################
  # EXPOSED
  ##########################################################
  
  if(current_state == "Exposed"){
    
    return(
      c(
        Safe = 0.10,
        Exposed = 0.40,
        Compromised = 0.35 + (0.10 * risk),
        Propagating = 0.05 + (0.05 * risk),
        Recovered = 0
      )
    )
    
  }
  
  ##########################################################
  # COMPROMISED
  ##########################################################
  
  if(current_state == "Compromised"){
    
    return(
      c(
        Safe = 0,
        Exposed = 0.10,
        Compromised = 0.40,
        Propagating = 0.40 + (0.10 * risk),
        Recovered = 0
      )
    )
    
  }
  
  ##########################################################
  # PROPAGATING
  ##########################################################
  
  if(current_state == "Propagating"){
    
    return(
      c(
        Safe = 0,
        Exposed = 0,
        Compromised = 0.20,
        Propagating = 0.50,
        Recovered = 0.30
      )
    )
    
  }
  
  ##########################################################
  # RECOVERED
  ##########################################################
  
  if(current_state == "Recovered"){
    
    return(
      c(
        Safe = 0.80,
        Exposed = 0.10,
        Compromised = 0.05,
        Propagating = 0,
        Recovered = 0.05
      )
    )
    
  }
  
}

############################################################
# Simulation Settings
############################################################

set.seed(123)

number_of_steps <- 20

############################################################
# Store State History
############################################################

state_history <- data.frame()

############################################################
# Markov Chain Simulation
############################################################

for(step in 1:number_of_steps){
  
  new_states <- character(nrow(data))
  
  for(i in 1:nrow(data)){
    
    current_state <- data$Current_State[i]
    
    risk_score <- data$Risk_Score[i]
    
    # Get transition probabilities
    
    probabilities <- calculate_transition(
      current_state,
      risk_score
    )
    
    # Select next state
    
    new_states[i] <- sample(
      names(probabilities),
      size = 1,
      prob = probabilities
    )
    
  }
  
  # Update states
  
  data$Current_State <- new_states
  
  ##########################################################
  # Count States
  ##########################################################
  
  state_counts <- data.frame(
    Time = step,
    Safe = sum(data$Current_State == "Safe"),
    Exposed = sum(data$Current_State == "Exposed"),
    Compromised = sum(
      data$Current_State == "Compromised"
    ),
    Propagating = sum(
      data$Current_State == "Propagating"
    ),
    Recovered = sum(
      data$Current_State == "Recovered"
    )
  )
  
  ##########################################################
  # Save History
  ##########################################################
  
  state_history <- rbind(
    state_history,
    state_counts
  )
  
}

############################################################
# Display Final State Distribution
############################################################

cat("\nFinal State Distribution:\n")

print(
  table(data$Current_State)
)

############################################################
# Save Final Dataset
############################################################

write.csv(
  data,
  "Results/MCMC_Final_State.csv",
  row.names = FALSE
)

############################################################
# Save State History
############################################################

write.csv(
  state_history,
  "Results/MCMC_State_History.csv",
  row.names = FALSE
)

############################################################
# Convert History to Long Format
############################################################

state_long <- state_history %>%
  
  tidyr::pivot_longer(
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

############################################################
# Plot State Transitions
############################################################

ggplot(
  state_long,
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
  
  geom_point() +
  
  labs(
    title = "Threat State Transition Over Time",
    x = "Simulation Time Step",
    y = "Number of Nodes",
    color = "Threat State"
  ) +
  
  theme_minimal()

############################################################
# Completion Message
############################################################

cat(
  "\nModule 4 Markov Chain Simulation Completed Successfully\n"
)