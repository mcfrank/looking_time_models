main_simulation_uc <- function(subject = x,
                stimuli_sequence = simple_stimuli, 
                noise_parameter = noise_parameter, 
                eig_from_world = env_eig,
                max_observation = max_obs, # should this be per trial or in total? currently per trial 
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
  
  # df for keeping track of model behavior, 
  #df_model <- 
  
  # df for keep track of actual observations 
  df_observation <- data.frame(matrix(ncol = ncol(stimuli_sequence), 
                                      nrow = max_observation)) %>% 
    tibble()  
  
  colnames(df_observation) <- colnames(stimuli_sequence)
  
  df_observation$t <- seq(1, max_observation, 1)
  df_observation$trial_type <- rep(NA_character_, max_observation)
  df_observation$trial_number <- rep(NA_integer_, max_observation)
  
  # df for all possible creatures 
  #based on the stimuli vector length generate permuations
  
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
  
  
  
  
  # stimulus_idx <- 1
  t <- 1
  posterior_at_t <- NULL
  
  while(stimulus_idx <= total_trial_number && t <= max_observation){
    
    #get stimulus
    current_stimulus <- stimuli_sequence[stimulus_idx,]
    
    #get observation 
    current_observation <- noisy_observation_creature(
      stimuli_df = stimuli_sequence,
      trial_index  = stimulus_idx, 
      n_sample = 1, 
      epsilon = noise_parameter
    )
    
    # add to current observation 
    df_observation[df_observation$t == t, str_detect(names(df_observation), "V")] <-
      current_observation
    df_observation[df_observation$t == t, "trial_type"] <- current_stimulus$trial_type
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
      list_df_posterior[[t]] <- cheaper_update_posterior(previous_posterior_df =  list_df_posterior[[t-1]],
                                                         current_posterior_df =  list_df_posterior[[t]], 
                                                         current_observation, 
                                                         grid_theta, grid_epsilon)
    }
    
    # accessing through dict_df_posterior 
      # if it is the first one, calculate lp_theta() and lp(epsilon)
      # if not, just use the previous value 
    #curr_posterior_df <- # udpate using prev_posterior_df and observation 
    
    
    
    #update dict_df_posterior 
    #...
    
    ##get EIG 
    # using all possible creatures df 
    # using prev_posterior_df 
    # make different scenarios using all possible creatures 
    # calculate eig 
    # technically even in the we only need to calculate two EIG: [z_bar + (znew = 1)], [z_bar + (z_new = 0)]
    # then put this in the repsective slots (feature)
    
    ## update model behavior df 
    ## update t 
    ## maybe udpate stimulus idx 
    
    
  
  }
  
}
  
  
  
  
  
  
}