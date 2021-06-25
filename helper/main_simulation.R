main_simulations <- function(
  subject_n, 
  observation_assumption, 
  stimuli_sequence, 
  noise_parameter, 
  eig_from_world = .005,
  max_observation = 500, # should this be per trial or in total? currently in total 
  grid_theta = grid_theta, 
  grid_epsilon = grid_epsilon, 
  alpha_prior = alpha_prior, 
  beta_prior = beta_prior,
  alpha_epsilon = alpha_epsilon, 
  beta_epsilon = beta_epsilon, 
  exposure_type = "forced_short",
  optimize = TRUE 
){
  
  sims <- lapply(seq(1, subject_n, 1), 
                 function(x){
                   main_simulation(subject = x,
                                   observation_assumption = "independent",
                                   stimuli_sequence = simple_stimuli, 
                                   noise_parameter = noise_parameter, 
                                   eig_from_world = eig_from_world,
                                   max_observation = max_obs, # should this be per trial or in total? currently per trial 
                                   grid_theta = grid_theta, 
                                   grid_epsilon = grid_epsilon, 
                                   alpha_prior = alpha_prior, 
                                   beta_prior = beta_prior,
                                   alpha_epsilon = alpha_epsilon, 
                                   beta_epsilon = beta_epsilon,
                                   exposure_type = "forced_short")
                 }
  ) %>% 
    bind_rows()
  
  
}








main_simulation <- function(
  subject, 
  observation_assumption, 
  stimuli_sequence, 
  noise_parameter, 
  eig_from_world = .005,
  max_observation = 500, # should this be per trial or in total? currently in total 
  grid_theta = grid_theta, 
  grid_epsilon = grid_epsilon, 
  alpha_prior = alpha_prior, 
  beta_prior = beta_prior,
  alpha_epsilon = alpha_epsilon, 
  beta_epsilon = beta_epsilon, 
  exposure_type = "forced_short", 
  optimize = TRUE 
){
  
  # set up the df that tracks eig  
  df <- tibble(t = rep(NA,max_observation),
               stimulus_idx = rep(NA,max_observation), 
               EIG = rep(NA,max_observation), 
               EIG_from_world = rep(eig_from_world,max_observation),
               p_look_away = rep(NA,max_observation), 
               look_away = rep(NA,max_observation))
  
  
  # set up the df that keep trakc of observation 
  observations <-  simple_stimuli %>% 
    # create an empty dataframe with all the info needed to be tracked 
    filter(is.na(.)) %>% 
    mutate(t = NA_integer_) 
  
  
  # the total number of stimuli 
  total_trial_number = nrow(stimuli_sequence)
  total_feature_number = stimuli_sequence %>% 
    select(starts_with("V")) %>% 
    ncol()
  
  
  
  # which stimulus are we looking at
  stimulus_idx <- 1
  t <- 1
  posterior_at_t <- NULL
  
  while(stimulus_idx <= total_trial_number && t <= max_observation){
    
    current_stimulus <- stimuli_sequence %>% 
      filter(trial_number == stimulus_idx)
    
    current_observation <- noisy_observation_creature(
      stimuli_df = stimuli_sequence,
      trial_index  = stimulus_idx, 
      n_sample = 1, 
      epsilon = noise_parameter
    )
    
    # add to current observation 
    observations <- bind_rows(observations, 
                              current_observation %>% mutate(
                                trial_number = stimulus_idx, 
                                trial_type = current_stimulus$trial_type,
                                t = t))
    
    # calculate posterior at t 
    # optimization possible!
    
    if(observation_assumption == "independent"){
      
      posterior_at_t <- grid_apprxoimation_with_observation(
        noisy_observation = observations, 
        track_epsilon = TRUE, 
        grid_theta = grid_theta, 
        grid_epsilon = grid_epsilon, 
        alpha_prior = alpha_prior, 
        beta_prior = beta_prior,
        alpha_epsilon = alpha_epsilon, 
        beta_epsilon = beta_epsilon, 
        optimize
      )
      
    }else{
      
      posterior_at_t <- faster_grid_apprxoimation_with_observation(
        timepoint = t, 
        noisy_observation = obs , 
        last_update_posterior_df = posterior_at_t, 
        track_epsilon = TRUE, 
        grid_theta = grid_theta, 
        grid_epsilon = grid_epsilon, 
        alpha_prior = alpha_prior, 
        beta_prior = beta_prior,
        alpha_epsilon = alpha_epsilon, 
        beta_epsilon = beta_epsilon
      )
      
      
      
    }
    
    
    
    
    
    # maybe needs scaling?
    
    df$t[t] = t
    df$stimulus_idx[t] = stimulus_idx
    
    # df$EIG[t] = get_eig(current_observation, 
    #                observations, 
    #                posterior_at_t, 
    #                grid_theta = grid_theta, 
    #                grid_epsilon = grid_epsilon, 
    #                alpha_prior = alpha_prior, 
    #                beta_prior = beta_prior,
    #                alpha_epsilon = alpha_epsilon, 
    #                beta_epsilon = beta_epsilon)
    
    df$EIG[t] = get_eig_toggle(
      t, 
      current_observation,
      observations, 
      observation_assumption,
      im = "kl", 
      posterior_at_t, 
      grid_theta = grid_theta, 
      grid_epsilon = grid_epsilon, 
      alpha_prior = alpha_prior, 
      beta_prior = beta_prior,
      alpha_epsilon = alpha_epsilon, 
      beta_epsilon = beta_epsilon, 
      optimize)
    
    # flip a coin with p_keep_looking weight
    df$p_look_away[t] = eig_from_world / (df$EIG[t] + eig_from_world)
    
    # try to force short exposure at the first trial 
    if(exposure_type == "forced_short"){
      
      # if this is the 5th timepoint, will change w/ EIG_env, current estimated using EIG_env 
      
      if (t > 5  && stimulus_idx == 1){
        df$look_away[t] = TRUE
      }else{
        df$look_away[t] = rbinom(1, 1, prob = df$p_look_away[t]) == 1
      }
      
      
    }else{
      df$look_away[t] = rbinom(1, 1, prob = df$p_look_away[t]) == 1
      
    }
    
   
    
    if (df$look_away[t]==TRUE) {
      stimulus_idx <- stimulus_idx + 1
    }
    
    t <- t + 1 
    
    
  }
  
  df <- df %>% mutate(id = subject)
  
  
  return(df)
}


main_simulation_toggle <- function(
  subject, 
  observation_assumption, 
  stimuli_sequence, 
  noise_parameter, 
  eig_from_world = .005,
  max_observation = 500, # should this be per trial or in total? currently in total 
  grid_theta = grid_theta, 
  grid_epsilon = grid_epsilon, 
  alpha_prior = alpha_prior, 
  beta_prior = beta_prior,
  alpha_epsilon = alpha_epsilon, 
  beta_epsilon = beta_epsilon
){
  
  # set up the df that tracks eig  
  df <- tibble(t = rep(NA,max_observation),
               stimulus_idx = rep(NA,max_observation), 
               EIG = rep(NA,max_observation), 
               p_look_away = rep(NA,max_observation), 
               look_away = rep(NA,max_observation))
  
  
  # set up the df that keep trakc of observation 
  observations <-  simple_stimuli %>% 
    # create an empty dataframe with all the info needed to be tracked 
    filter(is.na(.)) %>% 
    mutate(t = NA_integer_) 
  
  
  # the total number of stimuli 
  total_trial_number = nrow(stimuli_sequence)
  total_feature_number = stimuli_sequence %>% 
    select(starts_with("V")) %>% 
    ncol()
  
  
  
  # which stimulus are we looking at
  stimulus_idx <- 1
  t <- 1
  posterior_at_t <- NULL
  
  while(stimulus_idx <= total_trial_number && t <= max_observation){
    
    current_stimulus <- stimuli_sequence %>% 
      filter(trial_number == stimulus_idx)
    
    current_observation <- noisy_observation_creature(
      stimuli_df = stimuli_sequence,
      trial_index  = stimulus_idx, 
      n_sample = 1, 
      epsilon = noise_parameter
    )
    
    # add to current observation 
    observations <- bind_rows(observations, 
                              current_observation %>% mutate(
                                trial_number = stimulus_idx, 
                                trial_type = current_stimulus$trial_type,
                                t = t))
    
    # calculate posterior at t 
    # optimization possible!
    
    if(observation_assumption == "independent"){
      
      posterior_at_t <- grid_apprxoimation_with_observation(
        noisy_observation = observations, 
        track_epsilon = TRUE, 
        grid_theta = grid_theta, 
        grid_epsilon = grid_epsilon, 
        alpha_prior = alpha_prior, 
        beta_prior = beta_prior,
        alpha_epsilon = alpha_epsilon, 
        beta_epsilon = beta_epsilon
      )
      
    }else{
      
      posterior_at_t <- faster_grid_apprxoimation_with_observation(
        timepoint = t, 
        noisy_observation = obs , 
        last_update_posterior_df = posterior_at_t, 
        track_epsilon = TRUE, 
        grid_theta = grid_theta, 
        grid_epsilon = grid_epsilon, 
        alpha_prior = alpha_prior, 
        beta_prior = beta_prior,
        alpha_epsilon = alpha_epsilon, 
        beta_epsilon = beta_epsilon
      )
      
      
      
    }
    
    
    
    
    
    # maybe needs scaling?
    
    df$t[t] = t
    df$stimulus_idx[t] = stimulus_idx
    
    # df$EIG[t] = get_eig(current_observation, 
    #                observations, 
    #                posterior_at_t, 
    #                grid_theta = grid_theta, 
    #                grid_epsilon = grid_epsilon, 
    #                alpha_prior = alpha_prior, 
    #                beta_prior = beta_prior,
    #                alpha_epsilon = alpha_epsilon, 
    #                beta_epsilon = beta_epsilon)
    
    df$EIG[t] = get_eig_toggle(
      t, 
      current_observation,
      observations, 
      observation_assumption,
      im = "kl", 
      posterior_at_t, 
      grid_theta = grid_theta, 
      grid_epsilon = grid_epsilon, 
      alpha_prior = alpha_prior, 
      beta_prior = beta_prior,
      alpha_epsilon = alpha_epsilon, 
      beta_epsilon = beta_epsilon)
    
    # flip a coin with p_keep_looking weight
    df$p_look_away[t] = eig_from_world / (df$EIG[t] + eig_from_world)
    df$look_away[t] = rbinom(1, 1, prob = df$p_look_away[t]) == 1
    
    if (df$look_away[t]==TRUE) {
      stimulus_idx <- stimulus_idx + 1
    }
    
    t <- t + 1 
    
    
  }
  
  df <- df %>% mutate(id = subject)
  
  
  return(df)
}


main_simulation_subjects <- function(
  subjects, 
  observation_assumption, 
  stimuli_sequence, 
  noise_parameter, 
  eig_from_world = .005,
  max_observation = 500, # should this be per trial or in total? currently in total 
  grid_theta = grid_theta, 
  grid_epsilon = grid_epsilon, 
  alpha_prior = alpha_prior, 
  beta_prior = beta_prior,
  alpha_epsilon = alpha_epsilon, 
  beta_epsilon = beta_epsilon
){
  
  total_rows <- subjects * max_observaion
  
  # set up the df that tracks eig  
  df <- tibble(subject = rep(NA, total_rows),
               t = rep(NA,total_rows),
               stimulus_idx = rep(NA,total_rows), 
               EIG = rep(NA,total_rows), 
               p_look_away = rep(NA,total_rows), 
               look_away = rep(NA,total_rows))
  
  
  # set up the df that keep trakc of observation 
  observations <-  simple_stimuli %>% 
    # create an empty dataframe with all the info needed to be tracked 
    filter(is.na(.)) %>% 
    mutate(t = NA_integer_) 
  
  
  # the total number of stimuli 
  total_trial_number = nrow(stimuli_sequence)
  total_feature_number = stimuli_sequence %>% 
    select(starts_with("V")) %>% 
    ncol()
  
  
  
  # which stimulus are we looking at
  subject_id <- 1
  stimulus_idx <- 1
  t <- 1
  posterior_at_t <- NULL
  
  for(id in seq(1, ))
  
  while(stimulus_idx <= total_trial_number && t <= max_observation){
    
    current_stimulus <- stimuli_sequence %>% 
      filter(trial_number == stimulus_idx)
    
    current_observation <- noisy_observation_creature(
      stimuli_df = stimuli_sequence,
      trial_index  = stimulus_idx, 
      n_sample = 1, 
      epsilon = noise_parameter
    )
    
    # add to current observation 
    observations <- bind_rows(observations, 
                              current_observation %>% mutate(
                                trial_number = stimulus_idx, 
                                trial_type = current_stimulus$trial_type,
                                t = t))
    
    # calculate posterior at t 
    # optimization possible!
    
    if(observation_assumption == "independent"){
      
      posterior_at_t <- grid_apprxoimation_with_observation(
        noisy_observation = observations, 
        track_epsilon = TRUE, 
        grid_theta = grid_theta, 
        grid_epsilon = grid_epsilon, 
        alpha_prior = alpha_prior, 
        beta_prior = beta_prior,
        alpha_epsilon = alpha_epsilon, 
        beta_epsilon = beta_epsilon
      )
      
    }else{
      
      posterior_at_t <- faster_grid_apprxoimation_with_observation(
        timepoint = t, 
        noisy_observation = obs , 
        last_update_posterior_df = posterior_at_t, 
        track_epsilon = TRUE, 
        grid_theta = grid_theta, 
        grid_epsilon = grid_epsilon, 
        alpha_prior = alpha_prior, 
        beta_prior = beta_prior,
        alpha_epsilon = alpha_epsilon, 
        beta_epsilon = beta_epsilon
      )
      
      
      
    }
    
    
    
    
    
    # maybe needs scaling?
    
    df$t[t] = t
    df$stimulus_idx[t] = stimulus_idx
    
    # df$EIG[t] = get_eig(current_observation, 
    #                observations, 
    #                posterior_at_t, 
    #                grid_theta = grid_theta, 
    #                grid_epsilon = grid_epsilon, 
    #                alpha_prior = alpha_prior, 
    #                beta_prior = beta_prior,
    #                alpha_epsilon = alpha_epsilon, 
    #                beta_epsilon = beta_epsilon)
    
    df$EIG[t] = get_eig_toggle(
      t, 
      current_observation,
      observations, 
      observation_assumption,
      im = "kl", 
      posterior_at_t, 
      grid_theta = grid_theta, 
      grid_epsilon = grid_epsilon, 
      alpha_prior = alpha_prior, 
      beta_prior = beta_prior,
      alpha_epsilon = alpha_epsilon, 
      beta_epsilon = beta_epsilon)
    
    # flip a coin with p_keep_looking weight
    df$p_look_away[t] = eig_from_world / (df$EIG[t] + eig_from_world)
    df$look_away[t] = rbinom(1, 1, prob = df$p_look_away[t]) == 1
    
    if (df$look_away[t]==TRUE) {
      stimulus_idx <- stimulus_idx + 1
    }
    
    t <- t + 1 
    
    
  }
  
  df <- df %>% mutate(id = subject)
  
  
  return(df)
}


