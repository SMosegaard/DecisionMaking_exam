# Load libraries and set seed

print("[INFO]: Loading libraries")

install.packages("pacman")
pacman::p_load(R2jags, parallel, polspline, ggplot2, glue)

set.seed(123)
setwd('/work/SofieNørboMosegaard#5741/DecisionMaking_exam')


# Define a function for calculating the maximum of the posterior density
MPD <- function(x) {density(x)$x[which(density(x)$y==max(density(x)$y))]}

# Set parameters
ntypes <- 3      # 3 types/conditions
ngroups <- 8     # 2 same-class poor, 2 same-class rich, 4 mixed-class
group_size <- 6  # 6 subject per group 
ntrials <- 1

alpha <- c(2, 2.5, 3, 1.5, 2.8, 3.1)
rho <- c(0.7, 0.8, 0.6, 0.9, 0.75, 0.85)


# Source CC simulation (aka. agent)
source("CC")

# Run the simulations
CC_sims <- CC_simulation(group_size, ntrials, alpha, rho)

# Plotting to check that simulations make sense
#par(mfrow = c(2,2))
#plot(CC_sims$c)

plot(c)
plot(Gb)
plot(total_contribution) # payoff


# Re-assigning sim variables to the corresponding variable names in JAGS
x <- ORL_sims$x
X <- ORL_sims$X


#...









