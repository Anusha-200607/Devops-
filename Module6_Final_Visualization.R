############################################################
# MODULE 6 - FINAL PROJECT VISUALIZATION
############################################################

rm(list = ls())

library(ggplot2)
library(dplyr)

############################################################
# LOAD DATA
############################################################

data <- read.csv(
  "Results/Processed_Dataset.csv"
)

timeline <- read.csv(
  "Results/Propagation_Timeline.csv"
)

mcmc_history <- read.csv(
  "Results/MCMC_State_History.csv"
)

############################################################
# CREATE GRAPHS FOLDER
############################################################

if(!dir.exists("Graphs")){
  dir.create("Graphs")
}

############################################################
# GRAPH 1
# THREAT PROPAGATION OVER TIME
############################################################

p1 <- ggplot(
  timeline,
  aes(
    x = Time,
    y = Infected
  )
) +
  
  geom_line(
    linewidth = 1
  ) +
  
  geom_point(
    size = 2
  ) +
  
  labs(
    title =
      "Cyber Threat Propagation Over Time",
    
    subtitle =
      "ABM + DES Threat Propagation Simulation",
    
    x =
      "Simulation Time Step",
    
    y =
      "Number of Infected Nodes"
  )

print(p1)

ggsave(
  "Graphs/01_Threat_Propagation_Over_Time.png",
  p1,
  width = 10,
  height = 6
)

############################################################
# GRAPH 2
# SAFE VS COMPROMISED
############################################################

status_data <- data %>%
  
  group_by(Compromised) %>%
  
  summarise(
    Count = n()
  )

status_data$Status <- ifelse(
  status_data$Compromised == 1,
  "Compromised",
  "Safe"
)

p2 <- ggplot(
  status_data,
  aes(
    x = Status,
    y = Count
  )
) +
  
  geom_col() +
  
  labs(
    title =
      "Safe vs Compromised Systems",
    
    x =
      "System Status",
    
    y =
      "Number of Systems"
  )

print(p2)

ggsave(
  "Graphs/02_Safe_vs_Compromised.png",
  p2,
  width = 8,
  height = 6
)

############################################################
# GRAPH 3
# THREAT DISTRIBUTION
############################################################

threat_data <- data %>%
  
  group_by(Threat) %>%
  
  summarise(
    Count = n()
  )

p3 <- ggplot(
  threat_data,
  aes(
    x = reorder(Threat, Count),
    y = Count
  )
) +
  
  geom_col() +
  
  coord_flip() +
  
  labs(
    title =
      "Cyber Threat Distribution",
    
    x =
      "Threat Type",
    
    y =
      "Number of Records"
  )

print(p3)

ggsave(
  "Graphs/03_Threat_Distribution.png",
  p3,
  width = 10,
  height = 6
)

############################################################
# GRAPH 4
# ATTACK DISTRIBUTION
############################################################

attack_data <- data %>%
  
  group_by(Attack) %>%
  
  summarise(
    Count = n()
  )

p4 <- ggplot(
  attack_data,
  aes(
    x = reorder(Attack, Count),
    y = Count
  )
) +
  
  geom_col() +
  
  coord_flip() +
  
  labs(
    title =
      "Cyber Attack Distribution",
    
    x =
      "Attack Type",
    
    y =
      "Number of Records"
  )

print(p4)

ggsave(
  "Graphs/04_Attack_Distribution.png",
  p4,
  width = 10,
  height = 6
)

############################################################
# GRAPH 5
# RISK SCORE DISTRIBUTION
############################################################

p5 <- ggplot(
  data,
  aes(
    x = Risk_Score
  )
) +
  
  geom_histogram(
    bins = 20
  ) +
  
  labs(
    title =
      "Cybersecurity Risk Score Distribution",
    
    x =
      "Risk Score",
    
    y =
      "Frequency"
  )

print(p5)

ggsave(
  "Graphs/05_Risk_Score_Distribution.png",
  p5,
  width = 10,
  height = 6
)

############################################################
# GRAPH 6
# RISK LEVEL DISTRIBUTION
############################################################

risk_data <- data %>%
  
  group_by(Risk_Level) %>%
  
  summarise(
    Count = n()
  )

p6 <- ggplot(
  risk_data,
  aes(
    x = Risk_Level,
    y = Count
  )
) +
  
  geom_col() +
  
  labs(
    title =
      "Cybersecurity Risk Level Distribution",
    
    x =
      "Risk Level",
    
    y =
      "Number of Systems"
  )

print(p6)

ggsave(
  "Graphs/06_Risk_Level_Distribution.png",
  p6,
  width = 8,
  height = 6
)

############################################################
# GRAPH 7
# MCMC STATE TRANSITION
############################################################

mcmc_long <- mcmc_history %>%
  
  tidyr::pivot_longer(
    cols = c(
      Safe,
      Exposed,
      Compromised,
      Propagating,
      Recovered
    ),
    
    names_to =
      "State",
    
    values_to =
      "Count"
  )

p7 <- ggplot(
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
  
  geom_point() +
  
  labs(
    title =
      "Markov Threat State Transition",
    
    subtitle =
      "Evolution of Threat States Over Simulation Time",
    
    x =
      "Simulation Time",
    
    y =
      "Number of Nodes",
    
    color =
      "Threat State"
  )

print(p7)

ggsave(
  "Graphs/07_MCMC_State_Transition.png",
  p7,
  width = 10,
  height = 6
)

############################################################
# GRAPH 8
# VULNERABILITY FREQUENCY
############################################################

vulnerability_list <- unlist(
  strsplit(
    data$Vulnerabilities,
    ";"
  )
)

vulnerability_data <- data.frame(
  Vulnerability =
    vulnerability_list
) %>%
  
  group_by(
    Vulnerability
  ) %>%
  
  summarise(
    Count = n()
  )

p8 <- ggplot(
  vulnerability_data,
  aes(
    x = Vulnerability,
    y = Count
  )
) +
  
  geom_col() +
  
  labs(
    title =
      "Vulnerability Distribution",
    
    x =
      "Vulnerability",
    
    y =
      "Frequency"
  )

print(p8)

ggsave(
  "Graphs/08_Vulnerability_Distribution.png",
  p8,
  width = 8,
  height = 6
)

############################################################
# COMPLETION MESSAGE
############################################################

cat(
  "\n========================================\n"
)

cat(
  "MODULE 6 VISUALIZATION COMPLETED\n"
)

cat(
  "All graphs saved in the Graphs folder\n"
)

cat(
  "========================================\n"
)