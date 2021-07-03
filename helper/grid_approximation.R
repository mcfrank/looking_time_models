source(here("helper/noisy_update.R"))

get_df_lp_theta_epsilon <- function( grid_theta, grid_epsilon, 
                                  alpha_theta, beta_theta, 
                                  alpha_epsilon, beta_epsilon){
  
  df_lp_thetas = tibble("theta" = grid_theta, 
                        "lp_theta" = lp_theta(grid_theta, alpha_theta, beta_theta))
  df_lp_epsilons = tibble("epsilon" = grid_epsilon, 
                          "lp_epsilon" = lp_epsilon(grid_epsilon, alpha_epsilon, beta_epsilon))
  
  df_lp_theta_epsilon = expand_grid(df_lp_thetas, df_lp_epsilons)
  return(df_lp_theta_epsilon) 
}



get_lp_z_given_theta <- function(observation, 
                                     grid_theta, 
                                     grid_epsilon) {
  
 
  df_lp_thetas = tibble("theta" = grid_theta, 
                     "lp_yi_given_theta_y_TRUE" = lp_yi_given_theta(yi = 1, theta = grid_theta), 
                     "lp_yi_given_theta_y_FALSE" = lp_yi_given_theta(yi = 0, theta = grid_theta)
                    )
  df_lp_epsilons = tibble("epsilon" = grid_epsilon, 
                      "lp_zij_given_y_y_TRUE" = lp_z_ij_given_y(zij = observation[[1]], yi = 1, epsilon = grid_epsilon), 
                      "lp_zij_given_y_y_FALSE" = lp_z_ij_given_y(zij = observation[[1]], yi = 0, epsilon = grid_epsilon))
  
  df_lp_all = expand_grid(df_lp_thetas, df_lp_epsilons)
  
  m_lpz_ij_given_thetas <- cbind(df_lp_all$lp_yi_given_theta_y_FALSE + df_lp_all$lp_zij_given_y_y_FALSE, 
                                 df_lp_all$lp_yi_given_theta_y_TRUE +df_lp_all$lp_zij_given_y_y_TRUE)
  
  return(rowLogSumExps(m_lpz_ij_given_thetas))
  
}

cheaper_update_posterior <- function(previous_posterior_df,
                                     current_posterior_df, 
                                     current_observation, 
                                     grid_theta, grid_epsilon){
  
  previous_unnormalized_log_posterior <- previous_posterior_df$unnormalized_log_posterior
  current_lp_z_given_theta <- get_lp_z_given_theta(current_observation, 
                                               grid_theta, 
                                               grid_epsilon)
  

  current_posterior_df$unnormalized_log_posterior <- previous_unnormalized_log_posterior + current_lp_z_given_theta
  current_posterior_df$log_posterior <- current_posterior_df$unnormalized_log_posterior - matrixStats::logSumExp(current_posterior_df$unnormalized_log_posterior)
  return(current_posterior_df)
}






init_update <- function(
  posterior_df,
  df_lp_theta_epsilon, 
  observation,
  grid_theta, 
  grid_epsilon,
  alpha_theta, beta_theta, 
  alpha_epsilon, beta_epsilon){
  
  
  
  lp_z_given_theta <- get_lp_z_given_theta(observation, 
                                            grid_theta, grid_epsilon)
      
  unnormalized_log_posterior <- lp_z_given_theta + df_lp_theta_epsilon$lp_theta + 
    df_lp_theta_epsilon$lp_epsilon
  
  posterior_df$unnormalized_log_posterior <- unnormalized_log_posterior
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
){
  
 
  
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
  
  
    samps$posterior <- exp(samps$log_posterior)
    samps$feature_index <- feature_i
    

  
  
  
  
  return(samps)
  
}


 grid_apprxoimation_with_observation <- function(
  noisy_observation, 
  grid_theta = seq(0.01, .99, .01), 
  grid_epsilon = seq(0.01, .99, .01), 
  alpha_prior = 1, 
  beta_prior = 1,
  alpha_epsilon = 10, 
  beta_epsilon = 1
  
){
  
 
    
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

    
     
  
  
  
  
    return (posterior_df)
  


}