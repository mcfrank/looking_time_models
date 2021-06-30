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
  forced_exposure = TRUE,
  forced_sample = 5,
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
                                   exposure_type = exposure_type, 
                                   forced_exposure = TRUE,
                                   forced_sample = 5,
                                   optimize = TRUE )
                 }
  ) %>% 
    bind_rows()
  
  
}








main_simulation <- function(
  subject, 
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
  forced_exposure = TRUE,
  forced_sample = 5,
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
  
  observations <- data.frame(matrix(ncol = ncol(simple_stimuli), 
                                    nrow = max_observation)) %>% 
    tibble()  
  
  colnames(observations) <- colnames(simple_stimuli)
  
  observations$t <- seq(1, max_observation, 1)
  observations$trial_type <- rep(NA_character_, max_observation)
  observations$trial_number <- rep(NA_integer_, max_observation)
  
  
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
    
    current_stimulus <- stimuli_sequence[stimulus_idx,]
    
    current_observation <- noisy_observation_creature(
      stimuli_df = stimuli_sequence,
      trial_index  = stimulus_idx, 
      n_sample = 1, 
      epsilon = noise_parameter
    )
    
    # add to current observation 
    observations[observations$t == t, str_detect(names(observations), "V")] <-
      current_observation
    observations[observations$t == t, "trial_type"] <- current_stimulus$trial_type
    observations[observations$t == t, "trial_number"] <- stimulus_idx
    
    
    
    # calculate posterior at t 
    # optimization possible!
    
    posterior_at_t <- grid_apprxoimation_with_observation(
      noisy_observation = observations, 
      track_epsilon = TRUE, 
      grid_theta = grid_theta, 
      grid_epsilon = grid_epsilon, 
      alpha_prior = alpha_prior, 
      beta_prior = beta_prior,
      alpha_epsilon = alpha_epsilon, 
      beta_epsilon = beta_epsilon, 
      optimize = optimize
    )
    
    df$t[t] = t
    df$stimulus_idx[t] = stimulus_idx
    
    
    df$EIG[t] = get_eig_toggle(
      t, 
      current_observation,
      observations, 
      im = "kl", 
      posterior_at_t, 
      grid_theta = grid_theta, 
      grid_epsilon = grid_epsilon, 
      alpha_prior = alpha_prior, 
      beta_prior = beta_prior,
      alpha_epsilon = alpha_epsilon, 
      beta_epsilon = beta_epsilon, 
      optimize = optimize)
    
    # flip a coin with p_keep_looking weight
    df$p_look_away[t] = eig_from_world / (df$EIG[t] + eig_from_world)
    
    # try to force short exposure at the first trial 
    if(forced_exposure){
      
      if(stimulus_idx == 1 && t >= forced_sample){
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
  
  # maybe needs scaling?
  
  
  
  # df$EIG[t] = get_eig(current_observation, 
  #                observations, 
  #                posterior_at_t, 
  #                grid_theta = grid_theta, 
  #                grid_epsilon = grid_epsilon, 
  #                alpha_prior = alpha_prior, 
  #                beta_prior = beta_prior,
  #                alpha_epsilon = alpha_epsilon, 
  #                beta_epsilon = beta_epsilon)
  
  
  

df$id  <- subject 
df$forced_sample_n <- forced_sample


return(df)
}

