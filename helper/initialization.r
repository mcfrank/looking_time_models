
initialize_model <- function(subject, eig_from_world, max_observation){
  tibble(subject_id = rep(subject, max_observation),
         t = rep(NA,max_observation),
         stimulus_idx = rep(NA,max_observation), 
         EIG = rep(NA,max_observation), 
         EIG_from_world = rep(eig_from_world,max_observation),
         p_look_away = rep(NA,max_observation), 
         look_away = rep(NA,max_observation)) 
}

initialize_m_observation <- function(feature_number, max_observation, stimuli_sequence){
  # matrix for keep track of actual observations 
  m_observation <- matrix(ncol = feature_number, 
                          nrow = max_observation)
  colnames(m_observation) <- grep("V",names(stimuli_sequence), value = TRUE)
  return(m_observation)
}


initialize_ll_df_posterior <- function(grid_theta, grid_epsilon, max_observation, feature_number){
  df_posterior <- expand_grid(theta = grid_theta,
                              epsilon = grid_epsilon)
  # not sure when do we really need the non-log one, save some $$$  
  df_posterior$unnormalized_log_posterior <- NA_real_
  df_posterior$log_posterior <- NA_real_
  df_posterior$posterior <- NA_real_
  
  ll_df_posterior <- lapply(seq(1, max_observation, 1), 
                            function(x){
                              lapply(seq(1, feature_number, 1), 
                                     function(y){
                                       df_posterior
                                     })
                            })
  return(ll_df_posterior)
}

initialize_ll_df_z_given_theta <- function(grid_theta, grid_epsilon, max_observation, feature_number){
  df_z_given_theta <- expand_grid(theta = grid_theta, 
                                  epsilon = grid_epsilon)
  df_z_given_theta$lp_z_y_ONE <- NA_real_
  df_z_given_theta$lp_z_y_ZERO <-  NA_real_
  df_z_given_theta$lp_z_given_theta <- NA_real_

  ll_df_z_given_theta <- lapply(seq(1, max_observation, 1), 
                                function(x){
                                  lapply(seq(1, feature_number, 1), 
                                         function(y){
                                           df_z_given_theta
                                         })
                                })
  return(ll_df_z_given_theta)
  
}

