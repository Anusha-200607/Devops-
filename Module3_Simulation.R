############################################################
# MODULE 3B
# Hybrid ABM + DES Threat Propagation
############################################################

rm(list = ls())

library(igraph)
library(ggplot2)
library(dplyr)

# Read processed dataset
data <- read.csv("Results/Processed_Dataset.csv")

set.seed(123)

# Create network
network <- sample_smallworld(
  dim = 1,
  size = nrow(data),
  nei = 4,
  p = 0.05
)

V(network)$name <- data$Node_ID
V(network)$status <- "Safe"

# Initial infected nodes
initial <- sample(V(network), 10)
V(network)$status[initial] <- "Infected"

# Store infection timeline
timeline <- data.frame(
  Time = integer(),
  Infected = integer(),
  Safe = integer()
)

############################################################
# DES Simulation
############################################################

for(time in 1:20){
  
  current <- which(V(network)$status == "Infected")
  
  for(node in current){
    
    nbrs <- neighbors(network, node)
    
    for(n in nbrs){
      
      if(V(network)$status[n] == "Safe"){
        
        risk <- data$Risk_Score[as.numeric(n)]
        
        probability <- risk / 10
        
        if(runif(1) < probability){
          
          V(network)$status[n] <- "Infected"
          
        }
        
      }
      
    }
    
  }
  
  timeline <- rbind(
    timeline,
    data.frame(
      Time = time,
      Infected = sum(V(network)$status == "Infected"),
      Safe = sum(V(network)$status == "Safe")
    )
  )
  
}

############################################################
# Save Results
############################################################

data$Final_Status <- V(network)$status

write.csv(
  data,
  "Results/Threat_Propagation_Result.csv",
  row.names = FALSE
)

write.csv(
  timeline,
  "Results/Propagation_Timeline.csv",
  row.names = FALSE
)

############################################################
# Timeline Graph
############################################################

ggplot(timeline, aes(Time, Infected)) +
  geom_line(size = 1) +
  geom_point() +
  labs(
    title = "Threat Propagation Timeline",
    x = "Time Step",
    y = "Number of Infected Nodes"
  )

############################################################
# Final Network
############################################################

plot(
  network,
  vertex.color = ifelse(
    V(network)$status == "Infected",
    "red",
    "green"
  ),
  vertex.size = 5,
  vertex.label = NA,
  main = "Final Threat Propagation"
)

cat("\nHybrid ABM + DES Simulation Completed Successfully\n")