
# Load libraries and set seed
install.packages("pacman")
pacman::p_load(R2jags, haven, dplyr, parallel, ggplot2)
set.seed(123)
setwd('/work/SofieNørboMosegaard#5741/DecisionMaking_exam/src')


# Define function for calculating the maximum of the posterior density 
MPD <- function(x) {density(x)$x[which(density(x)$y==max(density(x)$y))]}

# Load data
Leb_PGG_3_Clean_FINAL <- read_dta('../data/Leb_PGG_3_Clean_FINAL.dta')

# Prepare data
df <- Leb_PGG_3_Clean_FINAL %>%
  
  # filter to only include Shia Female
  filter(sect == 'Sh', female == 1) %>%
  filter(final_assign == 1 | final_assign == 3) %>% 
  
  # Assign class_type (rich/poor)
  mutate(class_type = case_when(
    rich == 1 & poor == 0 ~ "rich",
    poor == 1 & rich == 0 ~ "poor",)) %>% 
  
  # Assign type (mix, same)
  mutate(type = case_when(
    T_mixed_class == 1 ~ "mix",
    T_mixed_class == 0 ~ "same",)) %>% 
  
  # Assign group_class (same_class_rich, same_class_poor, mixed_class)
  group_by(gid) %>%
  mutate(group_class = case_when(
    all(class_type == "rich") ~ "same_class_rich",
    all(class_type == "poor") ~ "same_class_poor",
    any(class_type == "rich") & any(class_type == "poor") ~ "mixed_class",)) %>% 
  
  # Scale contribution by 1000, so the "token range" is 0:10
  mutate(pgg_rd1 = pgg_rd1 / 1000) %>% 
  
  # Remove NAs
  filter(!is.na(pgg_rd1)) %>% 
  
  # Select columns of interest
  select(gid, pid, type, class_type, group_class, female, sect, pre_q2_age, pgg_rd1, pgg_rd1_totalpot)


# Split data into mix and same types
type_mix <- df %>% filter(type == 'mix')
type_same <- df %>% filter(type == 'same')

# Define nsub, ngroups, ntypes
subID_mix <- unique(type_mix$pid)
nsub_mix <- length(subID_mix) # 24
groupID_mix <- unique(type_mix$gid)
ngroups_mix <- length(groupID_mix) # 4 (with 6 subs in each)
ntypeID_mix <- unique(type_mix$type)
ntypes_mix <- length(ntypeID_mix) # 1

subID_same <- unique(type_same$pid)
nsub_same <- length(subID_same) # 24
groupID_same <- unique(type_same$gid)
ngroups_same <- length(groupID_same) # 4
ntypeID_same <- unique(type_same$type)
ntypes_same <- length(ntypeID_same) # 1

# Initialize empty arrays for c
c_mix <- array(NA, c(nsub_mix, ngroups_mix, 1))
c_same <- array(NA, c(nsub_same, ngroups_same, 1))

# Define function that loops through each group to assign subjects' pgg_rd1 (i.e. c) value
setup_arrays_c <- function(data, subID, groupID, c_array, nsub, ngroups) {
  for (g in 1:ngroups) {
    for (s in 1:nsub) {
      subject_data <- data %>%
        filter(gid == groupID[g] & pid == subID[s])
      if (nrow(subject_data) == 1) {
        c_array[s, g, 1] <- subject_data$pgg_rd1
      } else if (nrow(subject_data) > 1) {
        c_array[s, g, 1] <- subject_data$pgg_rd1[1] # Assign first value, caused error without 
      }
    }
  }
  
  return(c_array)
}

c_mix <- setup_arrays_c(type_mix, subID_mix, groupID_mix, c_mix, nsub_mix, ngroups_mix)
c_same <- setup_arrays_c(type_same, subID_same, groupID_same, c_same, nsub_same, ngroups_same)

# prepare for jags 
data <- list(c_mix = c_mix,
             c_same = c_same,
             nsub_mix = nsub_mix,
             nsub_same = nsub_same,
             ngroups_mix = ngroups_mix,
             ngroups_same = ngroups_same,
             ntypes_mix = ntypes_mix,
             ntypes_same = ntypes_same)

params <- c("diff_alpha", "diff_rho")

# Run jags
samples <- jags.parallel(data, inits = NULL, params,
                         model.file = "group_comparison.txt",
                         n.chains = 3, n.iter = 20000, n.burnin = 5000,
                         n.thin = 1, n.cluster = 4, jags.seed = 123)

save(samples, file = "../jags_output/group_diff_estimation.RData")

# Save the sample table as .txt by captureing the printed output
output <- capture.output(print(samples))
writeLines(output, con = "../jags_output/comparison_summary.txt")

# Plotting!
source("plot_functions.R")

df <- data.frame(parameter = rep(c("alpha", "rho"),
                                 each = length(samples$BUGSoutput$sims.list$diff_alpha)),
                 samples = c(as.vector(samples$BUGSoutput$sims.list$diff_alpha), 
                             as.vector(samples$BUGSoutput$sims.list$diff_rho)))

pl1 <- diff_plot(df)
ggsave("../plots/post_dens_diff_means.png", plot = pl1, width = 6, height = 6, dpi = 300)


# diff_alpha, diff_rho
png("../plots/post_dens_diff_means2.png", width = 800, height = 600)
par(mfrow = c(1,2))
plot(density(rnorm(10000, 0, 1/sqrt(1))), ylim = c(0,.7), main = "Δα")
lines(density(samples$BUGSoutput$sims.list$diff_alpha), col = "cornflowerblue")
plot(density(rnorm(10000, 0, 1/sqrt(1))), ylim = c(0,.7), main = "Δρ")
lines(density(samples$BUGSoutput$sims.list$diff_rho), col = "cornflowerblue")
dev.off()


# trace plot, diff-alpha and diff-rho
png("../plots/traceplot_group_comparison.png", width = 800, height = 600)
traceplot(samples, mfrow = c(3, 1))
dev.off()





