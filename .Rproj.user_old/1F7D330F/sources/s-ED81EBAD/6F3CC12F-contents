

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

grid_apprxoimation_with_observation_kl <- function(
  noisy_observation, 
  grid_theta = seq(0.01, .99, .01), 
  grid_epsilon = seq(0.01, .99, .01), 
  alpha_prior = 1, 
  beta_prior = 1,
  alpha_epsilon = 10, 
  beta_epsilon = 1
){
  
  update_grid_with_theta_and_epsilon(
    feature_i = 1, 
    grid_theta = grid_theta, 
    grid_epsilon = grid_epsilon, 
    observations = noisy_observation, 
    alpha_theta = alpha_prior, 
    beta_theta = beta_prior,
    alpha_epsilon = alpha_epsilon, 
    beta_epsilon = beta_epsilon
  )
  
  
  
  
  
}