source(here("helper/noisy_update.R"))

init_lp_theta_given_z <- function(observation, 
                             theta, epsilon, 
                             alpha_theta, beta_theta, 
                             alpha_epsilon, beta_epsilon) {
  
  lp_z_given_theta(z_bar, theta, epsilon, optimize) + 
    lp_theta(theta, alpha_theta, beta_theta) + 
    lp_epsilon(epsilon, alpha_epsilon, beta_epsilon)
}



init_update <- function(
  posterior_df,
  observation, 
  alpha_theta, beta_theta, 
  alpha_epsilon, beta_epsilon, 
){
  
  profvis(
  unnormalized_log_posterior <-  mapply(function(x, y) 
    init_lp_theta_given_z(observation = observation, 
                     theta = x, 
                     epsilon = y, 
                     alpha_theta = alpha_theta, 
                     beta_theta = beta_theta,
                     alpha_epsilon = alpha_epsilon, 
                     beta_epsilon = beta_epsilon), 
    posterior_df$theta, 
    posterior_df$epsilon)
  )
  posterior_df$log_posterior <- unnormalized_log_posterior - matrixStats::logSumExp(unnormalized_log_posterior)
  
  return(posterior_df)
  
}



update_grid_with_theta_and_epsilon <- function(
  feature_i, 
  grid_theta, 
  grid_epsilon, 
  observations, 
  alpha_theta, beta_theta, 
  alpha_epsilon, beta_epsilon, 
  optimize = TRUE
){
  
  if(optimize == TRUE){
    
    
  }
  
  
  samps <- expand_grid(theta = grid_theta,
                       epsilon = grid_epsilon) 
  
  
  samps$unnormalized_log_posterior <- mapply(function(x, y) 
    lp_theta_given_z(z_bar = na.omit(observations), 
                     #mysterious_df, 
                     theta = x, 
                     epsilon = y, 
                     alpha_theta = alpha_theta, 
                     beta_theta = beta_theta,
                     alpha_epsilon = alpha_epsilon, 
                     beta_epsilon = beta_epsilon, 
                     optimize), 
    samps$theta, 
    samps$epsilon)
  
  samps$log_posterior = samps$unnormalized_log_posterior - matrixStats::logSumExp(samps$unnormalized_log_posterior)
  
  
  if(optimize == TRUE){
    samps$posterior <- exp(samps$log_posterior)
    samps$feature_index <- feature_i
    
  }else{
    samps <- samps %>%
      mutate(posterior = exp(log_posterior)) %>% 
      mutate(feature_index = feature_i)
    
  }
  
  
  
  
  return(samps)
  
}


 grid_apprxoimation_with_observation <- function(
  noisy_observation, 
  track_epsilon = TRUE, 
  grid_theta = seq(0.01, .99, .01), 
  grid_epsilon = seq(0.01, .99, .01), 
  alpha_prior = 1, 
  beta_prior = 1,
  alpha_epsilon = 10, 
  beta_epsilon = 1, 
  optimize = TRUE
  
){
  
  if(optimize){
    
    # creating a df up front and keep track of the number 
    # probably don't want this to live in the fucntion though b/c it's technically going to be the same 
    # will be expensive if we keep creating the df, we can just updating the numbers in it 
    
    posterior_df <- tibble("")
    
    posterior_df <- expand_grid(theta = grid_theta,
                epsilon = grid_epsilon,
                feature_index = seq(1, 
                                    ncol(noisy_observation[startsWith(names(noisy_observation), 
                                                                      "V")]))) 

   # not sure when do we really need the non-log one, save some $$$    
    posterior_df$log_posterior <- NA_real_

    
     
    
    
  }else{
    
    posterior_df <- lapply(seq(1, 
                               ncol(noisy_observation[startsWith(names(noisy_observation), 
                                                                 "V")]), 
                               1), 
                           function(x){
                             update_grid_with_theta_and_epsilon(
                               feature_i = x, 
                               grid_theta = grid_theta, 
                               grid_epsilon = grid_epsilon, 
                               observations = noisy_observation[,x], 
                               alpha_theta = alpha_prior, 
                               beta_theta = beta_prior,
                               alpha_epsilon = alpha_epsilon, 
                               beta_epsilon = beta_epsilon, 
                               optimize = optimize
                             )
                           }
    ) %>% 
      bind_rows()
    
  }
  
  
  
  if (track_epsilon){
    return (posterior_df)
  }else{
    return (
      
      if(optimize){
        
        posterior_df <- setNames(aggregate(posterior_df$log_posterior,             # Sum by group
                                      by = list(posterior_df$theta),
                                      FUN = matrixStats::logSumExp), 
                            c("theta", "log_posterior"))
        
        posterior_df$posterior <- exp(pos_new$log_posterior)
        return(as_tibble(pos_new))
        
      }else{
        
        posterior_df <- posterior_df %>% 
          group_by(theta) %>%
          summarise(log_posterior = matrixStats::logSumExp(log_posterior)) %>%
          mutate(posterior = exp(log_posterior))
        
      }
      )
      
      
  }


}