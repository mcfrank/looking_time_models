

grid_apprxoimation_with_observation <- function(
  noisy_observation, 
  grid_theta = seq(0.01, .99, .01), 
  grid_epsilon = seq(0.01, .99, .01), 
  alpha_prior = 1, 
  beta_prior = 1,
  alpha_epsilon = 10, 
  beta_epsilon = 1
){
  
  #grid_apprxoimation_with_observation(observations, grid_theta, grid_epsilon, alpha_theta, beta_theta, alpha_epsilon, beta_epsilon)
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
                             beta_epsilon = beta_epsilon
                           )
                         }
  ) %>% 
    bind_rows()
  
  
  
  
  
  return (posterior_df)
  
  
  
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
  
  
  samps <- expand_grid(theta = grid_theta,
                       epsilon = grid_epsilon) 
  
  
  samps$unnormalized_log_posterior <- mapply(function(x, y) 
    lp_theta_given_z(z_bar = na.omit(observations), 
                     theta = x, 
                     epsilon = y, 
                     alpha_theta = alpha_theta, 
                     beta_theta = beta_theta,
                     alpha_epsilon = alpha_epsilon, 
                     beta_epsilon = beta_epsilon), 
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

