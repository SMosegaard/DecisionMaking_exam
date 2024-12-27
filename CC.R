

CC_simulation <- function(nsub,         # number of subjects
                          group,        # list of types and groups
                          alpha,        # optimism about initial contributions
                          rho           # readiness to cooperate
                          ){
  
  # N trials
  ntrials <- 1
  
  # Initialize empty arrays for..
  c <- array(NA, c(nsub, ntrials))  # subject contributions 
  Gb <- array(NA, c(nsub, ntrials)) # belief about others' contribution
    
  # Loop over all types (type1, type2, type3)
  for (t in names(group)) {
    
    # Loop over the groups within each type
    for (g in names(group[[t]])) {
      
      # Get the subjects in this group
      group_indices <- group[[t]][[g]]
      
      # Loop over each subject in the specific group
      for (s in group_indices) {
        
        # Group belief (Gb) on the first trial
        Gb[s] <- rpois(1, alpha[s])
        
        # Contribution (c) on the first trial
        p <- rho[s] * Gb[s]
        c[s] <- rpois(1, p)
        

        }
      }
    }
  
    
    result <- list(c = c, Gb = Gb)
    
    return(result)
    
  }

