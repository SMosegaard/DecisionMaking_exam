
# Load libraries and set seed
print("Loading libraries")
install.packages("pacman")
pacman::p_load(R2jags, ggplot2)
set.seed(123)
setwd('/work/SofieNørboMosegaard#5741/DecisionMaking_exam')

# Number of subjects to simulate
nsub <- 48

# Randomly assign subjects into type/condition
type1_poor <- sample(nsub, 12) # 12 poor subjects, type 1
remaining_subjects <- setdiff(all_subjects, type1_poor) # remove poor subjects from remaining pool

type2_rich <- sample(remaining_subjects, 12) # 12 rich subjects, type 2
remaining_subjects <- setdiff(remaining_subjects, type2_rich) # remove rich subjects from remaining pool

type3_poor <- sample(remaining_subjects, 12)
remaining_subjects <- setdiff(remaining_subjects, type3_poor)
type3_rich <- remaining_subjects

# Assign groups to each type
## type1_poor = same-class poor = 2 groups of 6 poor subjects each
## type2_rich = same-class rich = 2 groups of 6 rich subjects each
## type3 = mixed-class = 4 groups of 6 subjects, each with 3 poor and 3 rich
type1_groups <- list(group1 = type1_poor[1:6], group2 = type1_poor[7:12])
type2_groups <- list(group1 = type2_rich[1:6], group2 = type2_rich[7:12])
type3_groups <- list(group1 = c(type3_poor[1:3], type3_rich[1:3]),
                     group2 = c(type3_poor[4:6], type3_rich[4:6]),
                     group3 = c(type3_poor[7:9], type3_rich[7:9]),
                     group4 = c(type3_poor[10:12], type3_rich[10:12]))
group <- list(type1 = type1_groups,
              type2 = type2_groups,
              type3 = type3_groups)

# Set parameters for simulation
alpha_poor <- runif(12, 0, 3)
alpha_rich <- runif(12, 0, 3)
rho_poor <- runif(12, 0.1, 0.9)
rho_rich <- runif(12, 0.1, 0.9)

# Create alpha and rho for all subjects and types
alpha <- c(alpha_poor, alpha_rich, alpha_poor, alpha_rich)  # Concatenate for all types
rho <- c(rho_poor, rho_rich, rho_poor, rho_rich)

# Simulate subjects
source("CC.R")
simulated_data <- CC_simulation(nsub, group, alpha, rho)

# Store the c and Gb from the simulation
c <- simulated_data$c
Gb <- simulated_data$Gb



# recover parameters
param_recover_list <- list(nsub = nsub,
                           c = c,
                           alpha = alpha,
                           rho = rho)
params <- c("alpha", "rho")






