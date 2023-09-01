# THIS SCRIPT CONTAINS THE ONE MAIN SIMULATION FUNCTION

## ----------------- main_simulation -------------------
# runs main simulation computing EIG 
# takes a df of parameters and some globals
main_simulation_random_looking <- function(params = df,
                            grid_theta = seq(0.001, 1, 0.01),
                            grid_epsilon = c(0.000001)
                            ) {
  
  ### BOOK-KEEPING 
  total_trial_number = max(params$stimuli_sequence$data[[1]]$trial_number)
  
  # df for keeping track of model behavior
  model <-  initialize_model(params$world_EIG, params$max_observation, 
                             params$n_features)
  
  ### MAIN MODEL LOOP
  stimulus_idx <- 1
  t <- 1
  
  # while we haven't run out of stimuli or observations, 
  # sample a new observation
  # compute expected information gain
  # make a choice what to do
  while(stimulus_idx <= total_trial_number && t <= params$max_observation) {
    model$t[t] = t
    model$stimulus_idx[t] = stimulus_idx
    
    # get stimulus, observation, add to model
    current_stimulus <- params$stimuli_sequence$data[[1]][stimulus_idx,]
    current_observation <- noisy_observation_creature(
      creature = current_stimulus[,str_detect(names(current_stimulus), "V")], 
      n_sample = 1, 
      epsilon = params$noise_parameter
    )
    
    model[t, grepl("^f", names(model))] <- as.list(current_observation)
    
    # -compute new posterior grid over all possible outcomes
    # -compute KL between old and new posterior 
    model$stimulus_idx[t+1] <- stimulus_idx # pretend you're on the next stimulus

    # actual choice of whether to look away is sampled here
    model$look_away[t] = rbinom(1, 1, prob = runif(1,0,1)) == 1
    
    # if look away, increment
    if (model$look_away[t] == TRUE) {
      stimulus_idx <- stimulus_idx + 1
    }
    

    t <- t+1
 
    } # FINISH HUGE WHILE LOOP
  
  return(model)  
}

