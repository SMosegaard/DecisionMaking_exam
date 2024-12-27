
#### Subject-level: simulate subjects and recover subject-level parameters ####

# Load libraries and set seed
print("[INFO]: Loading libraries")
install.packages("pacman")
pacman::p_load(R2jags, parallel, polspline, ggplot2, glue)
set.seed(123)
setwd('/work/SofieNørboMosegaard#5741/DecisionMaking_exam')

# Number of subjects to simulate
nsub <- 48

# Set parameter values for simulation
alpha <- runif(nsub, 0, 3)
rho <- runif(nsub, 0.1, 0.9)

# simulate subjects

# Source CC simulation (aka. agent)
source("CC")
simulated_data <- CC_simulation(nsub, 
                         #type,
                         #ngroups,
                         #group_size,
                         alpha,
                         rho)

c <- simulated_data$c

# visualize simulated subjects
#...
sim_sub_plot <- function(c, filename="sim_sub_plot.png"){
  
  c_avg <- colMeans(c)
  
  plot <- ggplot() +
    geom_point(aes(x = 1:12, y = c_avg), size=2, shape="square") +
    geom_line(aes(x = 1:12, y = c_avg), lwd=0.3) +
    theme_bw() +
    theme(panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          strip.background = element_rect(fill = "transparent", colour = NA),
          strip.text = element_text(size = 12, face = "bold")) +
    ylab("Average contribution") +
    xlab("Trial") +
    ylim(-2, 6) +
    scale_x_continuous(breaks = 1:12)
  
  ggsave(paste0("plots/", filename), width = 6, height = 4)
  print(paste0("[INFO]: Saved plots/", filename))
}
sim_sub_plot(c, filename = "sim_sub_plot.png")


# recover subject-level parameters
param_recover_list <- list(nsub = nsub, c = c)
params <- c("alpha", "rho")


# Parameter estimation
#print("[INFO]: Parameter estimation")
#samples <- jags.parallel(data = param_recover_list,
#                         inits = NULL,
#                         parameters.to.save = params,
#                         model.file = "CC.txt",
#                         n.chains = 3,
#                         n.iter = 100,
#                         n.burnin = 100,
#                         n.thin = 1,
#                         jags.seed = 123)

# Save samples
#save(samples, file = "outputs/sub_recov_samples.RData")

# Extract the recovered parameters
#alpha_recov <- array(NA, c(nsub))
#rho_recov <- array(NA, c(nsub))

# Define a function for calculating the maximum of the posterior density
#MPD <- function(x) {density(x)$x[which(density(x)$y==max(density(x)$y))]}

#for (s in 1:nsub){
#  alpha_recov[s] <- MPD(samples$BUGSoutput$sims.list$alpha[,s])
#  rho_recov[s] <- MPD(samples$BUGSoutput$sims.list$rho[,s])
#}

# collect in data frame
#df <- data.frame(parameter = rep(c("alpha", "rho"), each = nsub),
#                 true = c(alpha, rho),
#                 recov = c(alpha_recov, rho_recov))

# visualize recovered parameters vs. true parameters
#print("[INFO]: Done")


