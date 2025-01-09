
CC_simulation <- function(nsub,
                          ngroups,
                          ntypes,       
                          mu_alpha,     
                          sigma_alpha) {
  
  # Initialize empty arrays for c and Gb
  c <- array(NA, c(nsub, ngroups, ntypes))  # subject contributions 
  Gb <- array(NA, c(nsub, ngroups, ntypes)) # belief about others' contribution
  
  
  # Simulate data for each type
  for (t in 1:ntypes) {
    
    # alpha
    rate_alpha <- (mu_alpha + sqrt(mu_alpha^2 + 4*sigma_alpha^2))/(2*sigma_alpha^2) 
    shape_alpha <- 1 + mu_alpha * rate_alpha
    
    # sample parameters
    alpha <- rgamma(nsub, shape_alpha, rate_alpha)
    rho <- rep(1, nsub)  #rho is set to 1 for all subjects
    
    ##
    
    # Simulate 
    for (g in 1:ngroups) {
      for (s in 1:nsub) {
        
        # Group belief (Gb) on the first trial
        Gb[s, g, t] <- rpois(1, alpha[s])
        
        # Contribution (c) on the first trial
        p <- rho[s] * Gb[s, g, t]
        c[s, g, t] <- rpois(1, p)
      }
    }
  }
  
  result <- list(c = c, Gb = Gb)
  
  return(result)
  
}





