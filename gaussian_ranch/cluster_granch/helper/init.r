
initialize_post_df <- function(prior_df, max_observation, feature_number){
  
  df <- prior_df 
  df$lp_z_given_mu_sig_sq <- rep(NA_real_, nrow(df))
  df$unnormalized_log_posterior <- rep(NA_real_, nrow(df))
  df$log_posterior <- rep(NA_real_, nrow(df))
  df$posterior <- rep(NA_real_, nrow(df))
  
  ll_post_df <- lapply(seq(1, max_observation, 1), 
                       function(x){
                         lapply(seq(1, feature_number, 1), 
                                function(y){
                                  df
                                })
                       })
  return(ll_post_df)
  
}


initialize_z_given_mu_sig_sq <- function(prior_df, max_observation, feature_number){
  
  df <- prior_df
  df$lp_z_bar_given_all_ys_mu_sig_sq <- rep(NA_real_, nrow(df))
  
  z_given_mu_sig_sq <- lapply(seq(1, max_observation, 1), 
                          function(x){
                            lapply(seq(1, feature_number, 1), 
                                   function(y){
                                     df
                                   })
                          })
  return(z_given_mu_sig_sq)
}


noisy_observation <- function(stimulus, epsilon){
  sapply(stimulus, function(x){rnorm(1, mean = x, sd = epsilon)}) 
}

initialize_model <- function(eig_from_world, max_observation, n_features, measurement = "EIG") {
  
  model <- tibble(t = rep(NA,max_observation),
                  stimulus_idx = rep(NA,max_observation), 
                  EIG_from_world = rep(eig_from_world,max_observation),
                  p_look_away = rep(NA,max_observation), 
                  look_away = rep(NA,max_observation)) 
  
  if(measurement == "EIG"){
    model$EIG = rep(NA,max_observation)
  }else{
    model$im = rep(NA,max_observation)
  }
  
  # initialize columns for observations
  model[,paste("f", 1:n_features, sep="")] <- NA
  
  return(model)
}



get_all_possible_observations_for_stimulus <- function(stimulus, epsilon, grid_n){
  lapply(stimulus, function(y){seq(y - epsilon *5, y + epsilon *5, epsilon * 10 / (grid_n - 1))}) %>% 
    expand.grid() %>% 
    rename_with(~gsub("Var", "V", .x))
}



initialize_model_testing_infrastructrue <- function(max_observation){
  ll_model_testing <- lapply(seq(1, max_observation, 1), 
                             function(x){
                               lapply(seq(1, 4, 1), 
                                      function(y){
                                          NULL
                                        })
                                      
                             })
  
  return(ll_model_testing)
}