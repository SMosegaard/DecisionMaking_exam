
# Load libraries and set seed
print("Loading libraries")
install.packages("pacman")
pacman::p_load(R2jags, ggplot2)
set.seed(123)
setwd('/work/SofieNørboMosegaard#5741/DecisionMaking_exam')


# Define function for calculating the maximum of the posterior density 
MPD <- function(x) {density(x)$x[which(density(x)$y==max(density(x)$y))]}


# Create task environment
nsub <- 100
group_size <- 6
ngroups <- nsub * group_size
ntypes <- 4

ntokens <- 10
vals <- seq(0, ntokens ,1)

# define parameter values for simulation
alpha <- runif(nsub, 0, 10) # subs were given 10,000 LBP and can contribute any amount (in 1,000 LBP increments) 
rho <- runif(nsub, 0.1, 0.9) # as rho ranges from 0 to 1


### obs - lav alpha og rho på samme måde som laura
# array(), som ender med parametrene for group og type level også
 # array(NA, c(nsub, ngroups, ntypes))


# Simulate data
source("CC.R")
CC_sims <- CC_simulation(nsub, group, alpha, rho)

# Store the c and Gb from the simulation
c <- CC_sims$c
Gb <- CC_sims$Gb

par(mfrow = c(2,2))
plot(CC_sims$Ev[,1])
plot(CC_sims$Ev[,2])
plot(CC_sims$Ev[,3])
plot(CC_sims$Ev[,4])

x <- ORL_sims$x
X <- ORL_sims$X

# recover subject-level parameters
data <- list(nsub = nsub,
             Ga = Ga,
             c = c)

# set up jags and run jags model
data <- list("x","X","ntrials") 
params<-c("a_rew","a_pun","K","omega_f","omega_p")

# recover parameters
param_recover_list <- list(nsub = nsub,
                           ngroups = ngroups,
                           ntypes = ntypes, 
                           c = c,
                           alpha = alpha,
                           rho = rho)

params <- c("alpha", "rho")

# Set up jags and run jags model - parameter estimation
samples <- jags.parallel(data,
                         inits = NULL,
                         params,
                         model.file ="CC.txt",
                         n.chains = 3, 
                         n.iter = 5000,
                         n.burnin = 1000,
                         n.thin = 1,
                         n.cluster = 3,
                         jags.seed = 123)

# list true parameters
true_alpha[i] <- alpha
true_rho[i] <- rho

# extract recovered parameters
A <- samples$BUGSoutput$sims.list$alpha
recov_alpha[i] <- MPD(A)

R <- samples$BUGSoutput$sims.list$rho
recov_rho[i] <- MPD(R)

# Plot scatter plots
par(mfrow = c(2,2))
plot(true_alpha, recov_alpha)
plot(true_rho, recov_rho)

# More plotting
source('recovery_plot.R')
pl1 <- recov_plot(true_alpha, recov_alpha, c("true alpha", "inferred alpha"), 'smoothed linear fit')
pl2 <- recov_plot(true_rho, recov_rho, c("true rho", "inferred rho"), 'smoothed linear fit')
ggarrange(pl1, pl2)


# save data
save(samples,
     nsub,
     ngroups,
     ntypes,
     true_alpha,
     true_rho,
     recov_alpha,
     recov_rho,
     file = "jags_output/subject_recovery_samples.RData")






