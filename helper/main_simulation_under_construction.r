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
  
  
  # df for keep track of model behavior, 
  #df_model <- 
  
  # df for keep track of actual observations 
  #df_observation <- 
  
  # df for keep track of posterior distribution 
  #df_posterior <- 
  
  # df for all possible creatures 
  #based on the stimuli vector length generate permuations
  
  # create a dictionary of dfs to keep track of ALL posterior distribution 
  # this way we can always look up the posterior from previous observation  
  #dict_df_posterior <- 
  
  # stimulus_idx <- 1
  t <- 1
  posterior_at_t <- NULL
  
  while(stimulus_idx <= total_trial_number && t <= max_observation){
    
    #get observation 
    
    #update observation df 
    #df_observation ...
    
    #update posterior df 
    #prev_posterior_df <- # accessing through dict_df_posterior 
    #curr_posterior_df <- # udpate using prev_posterior_df and observation 
    
    #update dict_df_posterior 
    #...
    
    ##get EIG 
    # using all possible creatures df 
    # using prev_posterior_df 
    # make different scenarios using all possible creatures 
    # calculate eig 
    
    ## update model behavior df 
    ## update t 
    ## maybe udpate stimulus idx 
    
    
  
  }
  
}
  
  
  
  
  
  
}