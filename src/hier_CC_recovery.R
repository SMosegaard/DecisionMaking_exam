

# Load libraries and set seed
install.packages("pacman")
pacman::p_load(R2jags, parallel, ggpubr, extraDistr, truncnorm, ggplot2, gridExtra)
set.seed(123)
setwd('/work/SofieNørboMosegaard#5741/DecisionMaking_exam/src')


# Define function for calculating the maximum of the posterior density 
MPD <- function(x) {density(x)$x[which(density(x)$y==max(density(x)$y))]}


# Set parameters for the simulation
nsub <- 24
group_size <- 6
ngroups <- nsub / group_size
ntypes <- 2

# mu
true_mu_alpha <- array(NA, c(nsub, ngroups, ntypes))
true_mu_rho <- array(NA, c(nsub, ngroups, ntypes))

infer_mu_alpha<- array(NA, c(nsub, ngroups, ntypes))
infer_mu_rho <- array(NA, c(nsub, ngroups, ntypes))

# sigma
true_sigma_alpha <- array(NA, c(nsub, ngroups, ntypes))
true_sigma_rho <- array(NA, c(nsub, ngroups, ntypes))

infer_sigma_alpha<- array(NA, c(nsub, ngroups, ntypes))
infer_sigma_rho <- array(NA, c(nsub, ngroups, ntypes))


for (t in 1:ntypes) {
  for (g in 1:ngroups) {
    for (s in 1:nsub) {
      
      # mean for alpha and rho
      mu_alpha <- runif(1, 0, 10) # one can contribute w 0-10 tokens
      mu_rho <- runif(1, 0.1, 0.9) # between 0 and 1 without the extreme values
      
      # standard deviation for alpha and rho
      sigma_alpha <- runif(1, 0.01, 0.1)
      sigma_rho <- runif(1, 0.01, 0.1)
      
      # Simulate data
      source("CC.R")
      CC_sims <- CC_simulation(nsub, ngroups, ntypes, mu_alpha, mu_rho, sigma_alpha, sigma_rho)
      
      # Store the c and Gb from the simulation
      c <- CC_sims$c
      Gb <- CC_sims$Gb
      
      # set up jags and run model
      data <- list("c", "Gb", "nsub", "ngroups", "ntypes") 
      params <- c("mu_alpha","mu_rho", "sigma_alpha", "sigma_rho")
      samples <- jags.parallel(data,
                               inits = NULL,
                               params,
                               model.file = "CC.txt",
                               n.chains = 3, 
                               n.iter = 5000,
                               n.burnin = 1500,
                               n.thin = 1,
                               n.cluster = 3,
                               jags.seed = 123)
      
      # store true parameters
      true_mu_alpha[s, g, t] <- mu_alpha
      true_mu_rho[s, g, t] <- mu_rho
      
      true_sigma_alpha[s, g, t] <- sigma_alpha
      true_sigma_rho[s, g, t] <- sigma_rho
      
      # extract recovered parameters (maximum a posteriori)
      Y <- samples$BUGSoutput$sims.list
      
      infer_mu_alpha[s, g, t] <- MPD(Y$mu_alpha)
      infer_mu_rho[s, g, t] <- MPD(Y$mu_rho)
      
      infer_sigma_alpha[s, g, t] <- MPD(Y$sigma_alpha)
      infer_sigma_rho[s, g, t] <- MPD(Y$sigma_rho)
      
    }
  }
}

# Plotting!
source("plot_functions.R")

# had some "length" issues with plotting --> works when flattening to 1D vectors:
true_mu_alpha_vec <- as.vector(true_mu_alpha)
infer_mu_alpha_vec <- as.vector(infer_mu_alpha)
true_mu_rho_vec <- as.vector(true_mu_rho)
infer_mu_rho_vec <- as.vector(infer_mu_rho)
true_sigma_alpha_vec <- as.vector(true_sigma_alpha)
infer_sigma_alpha_vec <- as.vector(infer_sigma_alpha)
true_sigma_rho_vec <- as.vector(true_sigma_rho)
infer_sigma_rho_vec <- as.vector(infer_sigma_rho)


df <- data.frame(parameter = rep(c("mu_alpha", "mu_rho", "sigma_alpha", "sigma_rho"), 
                                 each = length(true_mu_alpha_vec)),
                 true = c(true_mu_alpha_vec, true_mu_rho_vec, 
                          true_sigma_alpha_vec, true_sigma_rho_vec),
                 infer = c(infer_mu_alpha_vec, infer_mu_rho_vec, 
                           infer_sigma_alpha_vec, infer_sigma_rho_vec))

plot_mu <- recovery_plots(df, parameter_filter = c("mu_alpha", "mu_rho"))
plot_sigma <- recovery_plots(df, parameter_filter = c("sigma_alpha", "sigma_rho"))

ggsave("../plots/recov_plot_mu.png", plot_mu, width = 8, height = 6, dpi = 300)
ggsave("../plots/recov_plot_sigma.png", plot_sigma, width = 8, height = 6, dpi = 300)

combined_plot <- grid.arrange(plot_mu, plot_sigma, nrow = 2)
ggsave("../plots/recov_combined_plot.png", combined_plot, width = 12, height = 12, dpi = 300)


# Additional plotting
pl1 <- recov_plot(true_mu_alpha_vec, infer_mu_alpha_vec, c("true mu_alpha", "inferred mu_alpha"), 'smoothed linear fit')
ggsave("../plots/recovery_mu_alpha.png", plot = pl1, width = 6, height = 6, dpi = 300)

pl2 <- recov_plot(true_mu_rho_vec, infer_mu_rho_vec, c("true mu_rho", "inferred mu_rho"), 'smoothed linear fit')
ggsave("../plots/recovery_mu_rho.png", plot = pl2, width = 6, height = 6, dpi = 300)

combined_plot1 <- ggarrange(pl1, pl2)
ggsave("../plots/recovery_mu_alpha_rho.png", plot = combined_plot1, width = 12, height = 6, dpi = 300)

pl3 <- recov_plot(true_sigma_alpha_vec, infer_sigma_alpha_vec, c("true sigma_alpha", "inferred sigma_alpha"), 'smoothed linear fit')
ggsave("../plots/recovery_sigma_alpha.png", plot = pl3, width = 6, height = 6, dpi = 300)

pl4 <- recov_plot(true_sigma_rho_vec, infer_sigma_rho_vec, c("true sigma_rho", "inferred sigma_rho"), 'smoothed linear fit')
ggsave("../plots/recovery_sigma_rho.png", plot = pl4, width = 6, height = 6, dpi = 300)

combined_plot2 <- ggarrange(pl3, pl4)
ggsave("../plots/recovery_sigma_alpha_rho.png", plot = combined_plot2, width = 12, height = 6, dpi = 300)


# Save data
save(samples,
     nsub,
     ngroups,
     ntypes,
     true_mu_alpha,
     true_mu_rho,
     infer_mu_alpha,
     infer_mu_rho,
     true_sigma_alpha,
     true_sigma_rho,
     infer_sigma_alpha,
     infer_sigma_rho,
     file = "../jags_output/hier_CC_recovery_samples.RData")






