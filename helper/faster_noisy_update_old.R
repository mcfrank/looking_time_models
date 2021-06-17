update_lp_theta <- function(theta_value, updated_posterior){
  updated_posterior %>% 
    filter(theta == theta_value) %>% 
    select(normalized_log_posterior) %>% 
    pull()
}

update_lp_theta_given_z_after_observation <- function(new_observation, 
                                                      theta, 
                                                      epsilon, 
                                                      updated_posterior, 
                                                      alpha_epsilon, 
                                                      beta_epsilon){
  
  
  
  #sampling from the updated posterior, which is a broken beta distribution 
  
  new_lp_theta <- update_lp_theta(theta, updated_posterior)
  new_lp_epsilon <- lp_epsilon(epsilon, alpha_epsilon, beta_epsilon)  
  new_lp_z_given_theta <- lp_z_given_theta(new_observation, theta, epsilon)
  
  return (new_lp_theta + new_lp_epsilon + new_lp_z_given_theta)
  
}

faster_update_grid_with_theta_and_epsilon <- function(
  feature_i, 
  last_update_posterior_df, 
  grid_theta, 
  grid_epsilon, 
  observations, 
  alpha_theta, beta_theta, 
  alpha_epsilon, beta_epsilon
){
  
  
  samps <- expand_grid(theta = grid_theta,
                       epsilon = grid_epsilon) 
  
  if(nrow(observations == 1)){
    
    samps$unnormalized_log_posterior <- mapply(function(x, y) 
      lp_theta_given_z(z_bar = observations, 
                       theta = x, 
                       epsilon = y, 
                       alpha_theta = alpha_theta, 
                       beta_theta = beta_theta,
                       alpha_epsilon = alpha_epsilon, 
                       beta_epsilon = beta_epsilon), 
      samps$theta, 
      samps$epsilon)
  }else{
    
    samps$unnormalized_log_posterior <- mapply(function(x, y) 
      update_lp_theta_given_z_after_observation(new_observation = os %>% slice(nrow(os)) %>% select(starts_with("V")), 
                                                theta = x, 
                                                epsilon = y, 
                                                updated_posterior = last_update_posterior_df, 
                                                alpha_epsilon = alpha_epsilon, 
                                                beta_epsilon = beta_epsilon), 
      samps$theta, 
      samps$epsilon)
    
    
    
  }
  
  samps$log_posterior = samps$unnormalized_log_posterior - matrixStats::logSumExp(samps$unnormalized_log_posterior)
  
  
  theta_posterior <- samps %>%
    mutate(posterior = exp(log_posterior)) %>% 
    mutate(feature_index = feature_i)
  
  
  return(theta_posterior)
  
}


faster_grid_apprxoimation_with_observation_old <- function(
  noisy_observation, 
  last_update_posterior_df, 
  track_epsilon = TRUE, 
  grid_theta = seq(0.01, .99, .01), 
  grid_epsilon = seq(0.01, .99, .01), 
  alpha_prior = 1, 
  beta_prior = 1,
  alpha_epsilon = 10, 
  beta_epsilon = 1
  
){
  
  total_feature_number = ncol(noisy_observation %>% select(starts_with("V")))
  
  posterior_df <- lapply(seq(1, total_feature_number, 1), 
                         function(x){
                           faster_update_grid_with_theta_and_epsilon(
                             feature_i = x, 
                             last_update_posterior_df = last_update_posterior_df, 
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
  
  if (track_epsilon){
    return (posterior_df)
  }else{
    return (
      posterior_df <- posterior_df %>% 
        group_by(theta) %>%
        summarise(log_posterior = matrixStats::logSumExp(log_posterior)) %>%
        mutate(posterior = exp(log_posterior)))
  }
  
  
} 