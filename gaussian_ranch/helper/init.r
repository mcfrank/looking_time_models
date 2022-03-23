
initialize_post_df <- function(max_observation, feature_number){
  ll_post_df <- lapply(seq(1, max_observation, 1), 
                       function(x){
                         lapply(seq(1, feature_number, 1), 
                                function(y){
                                  NULL
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
  }else if(measurement == "KL"){
    model$KL = rep(NA,max_observation)
  }else if(measurement == "surprisal"){
    model$surprisal = rep(NA,max_observation)
  }
  
  # initialize columns for observations
  model[,paste("f", 1:n_features, sep="")] <- NA
  
  return(model)
}