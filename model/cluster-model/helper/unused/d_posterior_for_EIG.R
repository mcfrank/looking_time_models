
eig_get_df_lp_z_y_raw <- function(all_hypothetical_observations_on_this_stimulus, 
                                  grid_epsilon, df_lp_y_given_theta){
  
  df_lp_z_given_y = tibble(
    "epsilon" = grid_epsilon
  )
  
  df_lp_z_given_y$lp_z_given_y_ONE = rowSums(sapply(all_hypothetical_observations_on_this_stimulus, 
                                                      function(x){lp_z_ij_given_y(x, 1, grid_epsilon)}))
  df_lp_z_given_y$lp_z_given_y_ZERO = rowSums(sapply(all_hypothetical_observations_on_this_stimulus, 
                                                       function(x){lp_z_ij_given_y(x, 0, grid_epsilon)}))
  
  df_lp_z_y_raw <- expand_grid(df_lp_y_given_theta, df_lp_z_given_y)
}


eig_get_lp_z_given_theta_for_raw <- function(t, df_model, 
                                         current_df_lp_z_given_theta, prev_last_stimulus_observation_z_given_theta){
  
  if(df_model$stimulus_idx[[t]] == 1){
    lp_z_given_theta <-rowLogSumExps(lx = as.matrix(current_df_lp_z_given_theta[,c("lp_z_y_ONE", "lp_z_y_ZERO")]))
  }else{
      lp_z_given_theta<- rowLogSumExps(lx = as.matrix(current_df_lp_z_given_theta[,c("lp_z_y_ONE", "lp_z_y_ZERO")])) + 
        prev_last_stimulus_observation_z_given_theta$lp_z_given_theta
      # USE SOEM MAGIC TO FIGURE OUT THE CORRESPONDENCES 
  }
  
  return(lp_z_given_theta)
  
  
}

eig_get_df_lp_z_given_theta <- function(t,
                                   df_model,
                                   prev_last_stimulus_observation_z_given_theta,
                                   all_hypothetical_observations_on_this_stimulus, # contains unique combination + hypothetical scenarios 
                                   grid_theta, grid_epsilon, 
                                   df_lp_y_given_theta){
  
  current_df_lp_z_given_theta <- expand_grid(theta = grid_theta, 
                                  epsilon = grid_epsilon)
  current_df_lp_z_given_theta$lp_z_y_ONE <- NA_real_
  current_df_lp_z_given_theta$lp_z_y_ZERO <-  NA_real_
  current_df_lp_z_given_theta$lp_z_given_theta <- NA_real_
  
  df_lp_z_y_raw <-  eig_get_df_lp_z_y_raw(all_hypothetical_observations_on_this_stimulus, grid_epsilon, df_lp_y_given_theta)
  
  current_df_lp_z_given_theta$lp_z_y_ONE <-  df_lp_z_y_raw$lp_y_ONE_given_theta + df_lp_z_y_raw$lp_z_given_y_ONE
  current_df_lp_z_given_theta$lp_z_y_ZERO <- df_lp_z_y_raw$lp_y_ZERO_given_theta + df_lp_z_y_raw$lp_z_given_y_ZERO
  current_df_lp_z_given_theta$lp_z_given_theta <- eig_get_lp_z_given_theta_for_raw(t, df_model, 
                                                                               current_df_lp_z_given_theta, 
                                                                               prev_last_stimulus_observation_z_given_theta)
  
  
  return (current_df_lp_z_given_theta)
}

