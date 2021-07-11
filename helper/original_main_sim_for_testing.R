source(here("helper/old_grid_approx.r"))

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
  
  observations <- data.frame(matrix(ncol = ncol(stimuli_sequence), 
                                    nrow = max_observation)) %>% 
    tibble()  
  
  # the total number of stimuli 
  total_trial_number = nrow(stimuli_sequence)
  total_feature_number = stimuli_sequence %>% 
    select(starts_with("V")) %>% 
    ncol()
  
  # which stimulus are we looking at
  stimulus_idx <- 1
  t <- 1
  posterior_at_t <- NULL
  
  while( t <= max_observation){
    
    current_stimulus <- stimuli_sequence[stimulus_idx,]
    
    current_observation <- noisy_observation_creature(
      creature = stimuli_sequence[,str_detect(names(stimuli_sequence), "V")], 
      n_sample = 1, 
      epsilon = noise_parameter
    )
    
    current_observation = TRUE
    
    # add to current observation 
    observations[observations$t == t, str_detect(names(observations), "V")] <-
      current_observation
    observations[observations$t == t, "trial_type"] <- current_stimulus$trial_type
    observations[observations$t == t, "trial_number"] <- stimulus_idx
    
    
    
    # calculate posterior at t 
    # optimization possible!
    
    posterior_at_t <- grid_apprxoimation_with_observation(
      noisy_observation = na.omit(observations[,str_detect(names(observations), "V")]), 
      grid_theta = grid_theta, 
      grid_epsilon = grid_epsilon, 
      alpha_prior = alpha_prior, 
      beta_prior = beta_prior,
      alpha_epsilon = alpha_epsilon, 
      beta_epsilon = beta_epsilon
    )
    
    df$t[t] = t
    df$stimulus_idx[t] = stimulus_idx
    
    
    all_possible_outcomes <- get_possible_creatures(tibble("V1" = current_observation))
    
    
    
    df$EIG[[t]] = get_eig(current_observation, 
                                      observations, 
                                      posterior_at_t, 
                                      grid_theta = grid_theta, 
                                      grid_epsilon = grid_epsilon, 
                                      alpha_prior = alpha_prior, 
                                      beta_prior = beta_prior,
                                      alpha_epsilon = alpha_epsilon, 
                                      beta_epsilon = beta_epsilon)[[1]]
 
    
   
    t <- t + 1 
    
    
    
  }
  
  return(df)
}

