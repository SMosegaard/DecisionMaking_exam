
# Load libraries and set seed
install.packages("pacman")
pacman::p_load(R2jags, haven, dplyr, parallel, ggplot2)
set.seed(123)
setwd('/work/SofieNørboMosegaard#5741/DecisionMaking_exam')


# Define function for calculating the maximum of the posterior density 
MPD <- function(x) {density(x)$x[which(density(x)$y==max(density(x)$y))]}

# Load data
Leb_PGG_3_Clean_FINAL <- read_dta('data/Leb_PGG_3_Clean_FINAL.dta')

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
    any(class_type == "rich") & any(class_type == "poor") ~ "mixed_class",)
  )

# Select needed columns
df <- df %>%
  select(gid, pid, type, class_type, group_class, female, sect,
         pre_q2_age, pgg_rd1, pgg_rd1_totalpot)


subID <- unique(df$pid)
nsub <- length(subID) # 48

groupID <- unique(df$gid)
ngroups <- length(groupID) # 8 (4 mix, 4 same)

utypes <- unique(df$type)
ntypes <- length(utypes)
ntypes # 2 = mix, same


for (t in 1:ntypes) {
  for (g in 1:ngroups) {
    for (s in 1:nsub) {
      
      data <- list("c", "Gb", "nsub", "ngroups", "ntypes") # change
      params <- c("mu_alpha","mu_rho", "sigma_alpha", "sigma_rho") # change
      samples <- jags.parallel(data,
                               inits = NULL,
                               params,
                               model.file = "CC.txt",
                               n.chains = 3, 
                               n.iter = 5000,
                               n.burnin = 1000,
                               n.thin = 1,
                               n.cluster = 3,
                               jags.seed = 123)
      
      
    }
  }
}
      





