main_simulation_uc <- function(subject = x,
                stimuli_sequence, 
                noise_parameter = noise_parameter, 
                eig_from_world = 0.001,
                max_observation = 50, # should this be per trial or in total? currently per trial 
                grid_theta = grid_theta, 
                grid_epsilon = grid_epsilon, 
                alpha_prior = alpha_prior, 
                beta_prior = beta_prior,
                alpha_epsilon = alpha_epsilon, 
                beta_epsilon = beta_epsilon, 
                forced_exposure = TRUE,
                forced_sample = 5,
                optimize = FALSE){
  
  
  feature_number <- ncol(stimuli_sequence[startsWith(names(stimuli_sequence), 
                                                     "V")])
  total_trial_number = max(stimuli_sequence$trial_number)
  
  # df for keeping track of model behavior, 
  df_model <-  tibble(t = rep(NA,max_observation),
                      stimulus_idx = rep(NA,max_observation), 
                      EIG = rep(NA,max_observation), 
                      EIG_from_world = rep(eig_from_world,max_observation),
                      p_look_away = rep(NA,max_observation), 
                      look_away = rep(NA,max_observation)) 
  
  # df for keep track of actual observations 
  df_observation <- data.frame(matrix(ncol = ncol(stimuli_sequence), 
                                      nrow = max_observation)) %>% 
    tibble()  
  
  colnames(df_observation) <- colnames(stimuli_sequence)
  
  df_observation$t <- seq(1, max_observation, 1)
  df_observation$trial_number <- rep(NA_integer_, max_observation)
  
  # df for all possible creatures
  # eventually this function would be deprecated because so far we can't even enumerate 50 features instance
  #df_all_possible_creatures <- get_possible_creatures()
  
  # create a list of dfs to keep track of ALL posterior distribution 
  # actually may consider keeping track of everything in matrix for multi-feature version 
  # this way we can always look up the posterior from previous observation 
  
  # material for calculating df_posterior 
  df_lp_theta_epsilon <- get_df_lp_theta_epsilon(grid_theta, grid_epsilon, 
                                                 alpha_theta, beta_theta, 
                                                 alpha_epsilon, beta_epsilon)
  # df for keep track of posterior distribution 
  df_posterior <- expand_grid(theta = grid_theta,
                              epsilon = grid_epsilon,
                              feature_index = seq(1, feature_number))
  # not sure when do we really need the non-log one, save some $$$  
  df_posterior$unnormalized_log_posterior <- NA_real_
  df_posterior$log_posterior <- NA_real_


  list_df_posterior <- lapply(seq(1, max_observation, 1), 
                              function(x){df_posterior})
  
  
  
  
  stimulus_idx <- 1
  t <- 1
  posterior_at_t <- NULL
  
 # while(stimulus_idx <= total_trial_number && t <= max_observation){
    
  while(t <= max_observation){
    df_model$t[[t]] = t
    df_model$stimulus_idx[[t]] = stimulus_idx
    
    #get stimulus
    current_stimulus <- stimuli_sequence[stimulus_idx,]
    
    #get observation 
    current_observation <- noisy_observation_creature(
      creature = current_stimulus[,str_detect(names(current_stimulus), "V")], 
      n_sample = 1, 
      epsilon = noise_parameter
    )
    
    current_observation <- TRUE
    
    # add to current observation 
    df_observation[df_observation$t == t, str_detect(names(df_observation), "V")] <-
      current_observation
    df_observation[df_observation$t == t, "trial_number"] <- stimulus_idx
    
   
    #update posterior df 
    if(t == 1){
      # do some fresh calculation
      
      list_df_posterior[[t]] <- init_update( list_df_posterior[[t]], 
                                             df_lp_theta_epsilon, 
                                             current_observation,
                                             grid_theta, grid_epsilon,
                                             alpha_theta, beta_theta, 
                                             alpha_epsilon, beta_epsilon)
    }else{
      list_df_posterior[[t]] <- update_posterior(previous_posterior_df =  list_df_posterior[[t-1]],
                                                         current_posterior_df =  list_df_posterior[[t]], 
                                                         current_observation, 
                                                         grid_theta, grid_epsilon)
    }
    
    
    
    ##get EIG
    # we will want to past list_df posterior so that we don't need to calculate posterior again
    df_model$EIG[[t]] <- get_eig_faster(na.omit(df_observation[,str_detect(names(df_observation), "V")]),
                                   grid_theta, grid_epsilon, 
                                   alpha_theta, beta_theta, 
                                   alpha_epsilon,beta_epsilon)
    
    
    ## update model behavior df 
    ## update t 
    ## maybe udpate stimulus idx 
    
    t = t+1
    #stimulus_idx = stimulus_idx + 1
  }
  return (df_model)  

}
  
  
  
  
  
