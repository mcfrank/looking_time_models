## -------------------------------------------------------------
## grid with theta

grid_with_theta <- function(
  grid_theta = seq(0.01, .99, .01), 
  epsilon = epsilon, 
  noisy_observation, 
  alpha_prior = 1, 
  beta_prior = 1,
  alpha_epsilon, 
  beta_epsilon
){
  # special case this is for when only update based on 1 observation
  if(!is.matrix(noisy_observation)){
    feature_number = length(noisy_observation)
    
    lapply(seq(1, feature_number, 1), 
           function(x){
             update_grid_with_theta(
               feature_index = x, 
               thetas = grid_theta, 
               z_bar = noisy_observation[x], 
               epsilon = epsilon, 
               alpha_theta = alpha_prior, 
               beta_theta = beta_prior,
               alpha_epsilon = alpha_epsilon, 
               beta_epsilon = beta_epsilon
             )
           }
    ) %>% 
      bind_rows()
  }else{
    feature_number = ncol(noisy_observation)
    
    lapply(seq(1, feature_number, 1), 
           function(x){
             update_grid_with_theta(
               feature_index = x, 
               thetas = grid_theta, 
               z_bar = noisy_observation[,x], 
               epsilon = epsilon, 
               alpha_theta = alpha_prior, 
               beta_theta = beta_prior,
               alpha_epsilon = alpha_epsilon, 
               beta_epsilon = beta_epsilon
             )
           }
    ) %>% 
      bind_rows()
  }
}

## -------------------------------------------------------------
## grid with theta and epsilon (not integrating out epsilon

grid_with_theta_and_epsilon <- function(
  grid_theta = seq(0.01, .99, .01), 
  grid_epsilon = seq(0.01, .99, .01), 
  noisy_observation, 
  alpha_prior = 1, 
  beta_prior = 1,
  alpha_epsilon, 
  beta_epsilon
){
  # special case this is for when only update based on 1 observation
  
  if(!is.matrix(noisy_observation)){
    feature_number = length(noisy_observation)
    
    lapply(seq(1, feature_number, 1), 
           function(x){
             update_grid_with_theta_and_epsilon(
               feature_i = x, 
               grid_theta = grid_theta, 
               grid_epsilon = grid_epsilon, 
               observations = noisy_observation[x], 
               alpha_theta = alpha_prior, 
               beta_theta = beta_prior,
               alpha_epsilon = alpha_epsilon, 
               beta_epsilon = beta_epsilon
             )
           }
    ) %>% 
      bind_rows()
    
  }else{
    feature_number = ncol(noisy_observation)
    
    lapply(seq(1, feature_number, 1), 
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
  }
}




# single feature 
# for trial 1 when beta distribution hasn't been destroyed 

update_grid_with_theta <- function(feature_index = 1, 
                                   thetas = seq(0.01, .99, .02), 
                                   z_bar, 
                                   epsilon, 
                                   alpha_theta, 
                                   beta_theta,
                                   alpha_epsilon, 
                                   beta_epsilon){
  
  posterior_df <- tibble("theta" = thetas)
  posterior_df$unnormalized_log_posterior <- sapply(thetas, 
                                                    function(x){ 
                                                      lp_theta_given_z(z_bar = z_bar, 
                                                                       theta = x, 
                                                                       epsilon = epsilon, 
                                                                       alpha_theta = alpha_theta, 
                                                                       beta_theta = beta_theta,
                                                                       alpha_epsilon = alpha_epsilon, 
                                                                       beta_epsilon = beta_epsilon)})
  posterior_df$normalized_log_posterior <- posterior_df$unnormalized_log_posterior - matrixStats::logSumExp(posterior_df$unnormalized_log_posterior)
  posterior_df$feature_index <- feature_index
  
  return(posterior_df)
  
}




update_grid_with_theta_and_epsilon <- function(
  feature_i, 
  grid_theta, 
  grid_epsilon, 
  observations, 
  alpha_theta, beta_theta, 
  alpha_epsilon, beta_epsilon
){
  
  
  samps <- expand_grid(theta = grid_theta,
                       epsilon = grid_epsilon) 
  
  
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
  
  samps$log_posterior = samps$unnormalized_log_posterior - matrixStats::logSumExp(samps$unnormalized_log_posterior)
  
  # integrate out epsilon by summing p(theta | z) over all the different possible values of epsilon
  # that's the log-sum-exp line
  theta_posterior <- samps %>%
    group_by(theta) %>%
    summarise(log_posterior = matrixStats::logSumExp(log_posterior)) %>%
    mutate(posterior = exp(log_posterior)) %>% 
    mutate(feature_index = feature_i)
  
  return(theta_posterior)
  
}






grid_with_theta_and_epsilon_has_epsilon <- function(
  grid_theta = seq(0.01, .99, .01), 
  grid_epsilon = seq(0.01, .99, .01), 
  noisy_observation, 
  alpha_prior = 1, 
  beta_prior = 1,
  alpha_epsilon, 
  beta_epsilon
){
  # special case this is for when only update based on 1 observation
  
  # if(!is.matrix(noisy_observation)){
  #   feature_number = length(noisy_observation)
  #   
  #   lapply(seq(1, feature_number, 1), 
  #          function(x){
  #            update_grid_with_theta_and_epsilon_has_epsilon(
  #              feature_i = x, 
  #              grid_theta = grid_theta, 
  #              grid_epsilon = grid_epsilon, 
  #              observations = noisy_observation[x], 
  #              alpha_theta = alpha_prior, 
  #              beta_theta = beta_prior,
  #              alpha_epsilon = alpha_epsilon, 
  #              beta_epsilon = beta_epsilon
  #            )
  #          }
  #   ) %>% 
  #     bind_rows()
  #   
  #   
  #   
  # }else{
  #   
    
    feature_number = ncol(noisy_observation)
    
    lapply(seq(1, feature_number, 1), 
           function(x){
             update_grid_with_theta_and_epsilon_has_epsilon(
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
  
  
  
  
  
  
  
}




update_grid_with_theta_and_epsilon_has_epsilon <- function(
  feature_i, 
  grid_theta, 
  grid_epsilon, 
  observations, 
  alpha_theta, beta_theta, 
  alpha_epsilon, beta_epsilon
){
  
  
  samps <- expand_grid(theta = grid_theta,
                       epsilon = grid_epsilon) 
  
  
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
  
  samps$log_posterior = samps$unnormalized_log_posterior - matrixStats::logSumExp(samps$unnormalized_log_posterior)
  
  
  theta_posterior <- samps %>%
    mutate(posterior = exp(log_posterior)) %>% 
    mutate(feature_index = feature_i)
  
  
  return(theta_posterior)
  
}


