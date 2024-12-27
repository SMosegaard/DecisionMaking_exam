
#### Subject-level: simulate subjects and recover subject-level parameters ####

# Load libraries and set seed
print("Loading libraries")
install.packages("pacman")
pacman::p_load(R2jags, polspline, ggplot2, glue)
set.seed(123)
setwd('/work/SofieNørboMosegaard#5741/DecisionMaking_exam')

# Number of subjects to simulate
nsub <- 48

# Set parameter values for simulation
alpha <- runif(nsub, 0, 3)
rho <- runif(nsub, 0.1, 0.9)

# Source CC simulation (aka. agent)
source("CC.R")
simulated_data <- CC_simulation(nsub,
                                alpha,
                                rho)

# Store the c and Gb from the simulation
c <- simulated_data$c
Gb <- simulated_data$Gb

simulated_data_df <- data.frame(subjects = rep(1:nsub, each = 1),
                                c = c,
                                Gb = Gb,
                                rho = rho,
                                alpha = alpha)


# visualize simulated data
# Scatter plot: contribution and Gb
scatter_plot <- ggplot(simulated_data_df, aes(x = Gb, y = c)) +
  geom_point() +
  labs(title = "Contribution (c) vs Belief (Gb)",
       x = "Belief about others' contributions (Gb)",
       y = "Contribution (c)") +
  theme_minimal()
ggsave("plots/scatter_plot.png", scatter_plot, width = 6, height = 4)

# Distribution of c
hist_c <- ggplot(simulated_data_df, aes(x = c)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  labs(title = "Distribution of Contribution (c)",
       x = "c", y = "Frequency") +
  theme_minimal()
ggsave("plots/hist_c.png", hist_c, width = 6, height = 4)

# Distribution of Gb
hist_Gb <- ggplot(simulated_data_df, aes(x = Gb)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  labs(title = "Distribution of Belief about others' contributions (Gb)",
       x = "Gb", y = "Frequency") +
  theme_minimal()
ggsave("plots/hist_Gb.png", hist_Gb, width = 6, height = 4)

# Distribution of rho
hist_rho <- ggplot(simulated_data_df, aes(x = rho)) +
  geom_histogram(binwidth = 0.05, fill = "lightblue", color = "black") +
  labs(title = "Distribution of Readiness to Cooperate (rho)",
       x = "rho", y = "Frequency") +
  theme_minimal()
ggsave("plots/hist_rho.png", hist_rho, width = 6, height = 4)

# Distribution of alpha
hist_alpha <- ggplot(simulated_data_df, aes(x = alpha)) +
  geom_histogram(binwidth = 0.05, fill = "lightyellow", color = "black") +
  labs(title = "Distribution of Optimism about Initial Contributions (alpha)",
       x = "alpha", y = "Frequency") +
  theme_minimal()
ggsave("plots/hist_alpha.png", hist_alpha, width = 6, height = 4)


# recover subject-level parameters
param_recover_list <- list(nsub = nsub,
                           c = c,
                           alpha = alpha,
                           rho = rho)
params <- c("alpha", "rho")


