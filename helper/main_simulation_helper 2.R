# THIS SCRIPT CONTAINS THE ONE MAIN SIMULATION FUNCTION


# runs main simulation computing EIG 
# takes a df of parameters and some globals
main_simulation <- function(params = df,
                            grid_theta = seq(0.1, 1, 0.2),
                            grid_epsilon = seq(0.1, 1, 0.2),
                            forced_exposure = FALSE,
                            forced_sample = 5) {
  
  ### BOOK-KEEPING 
  total_trial_number = max(params$stimuli_sequence$data[[1]]$trial_number)
  
  # df for keeping track of model behavior
  model <-  initialize_model(params$eig_from_world, params$max_observation, 
                             params$n_features)
  
  # list of lists of df for the posteriors and likelihoods
  post <- initialize_posterior(grid_theta, grid_epsilon, 
                               params$max_observation, params$n_features)
  z_given_theta <- initialize_z_given_theta(grid_theta, grid_epsilon, 
                                            params$max_observation, 
                                            params$n_features)
  
  # possible observation df and book-keeping for likelihoods and posteriors for that
  # AC: this version is enumerating all possible observations? 
  possible_observations <- get_possible_observations(n_features = params$n_features)
  z_given_theta_upcoming <- initialize_z_given_theta(grid_theta, grid_epsilon,
                                                     nrow(possible_observations), 
                                                     params$n_features)
  post_upcoming <- initialize_posterior(grid_theta, grid_epsilon, 
                                        nrow(possible_observations), params$n_features)
  
  # dataframes of thetas and epsilons, and y given theta (these don't change)
  # AC: needs to double check if these are lp or p, currently lp
  
  theta_epsilon <- get_theta_epsilon(grid_theta, grid_epsilon, 
                                     params$alpha_prior,  params$beta_prior, 
                                     params$alpha_epsilon, params$beta_epsilon)
  y_given_theta = tibble(theta = grid_theta, 
                            y_ONE_given_theta = lp_yi_given_theta(yi = 1, 
                                                                     theta = grid_theta), 
                            y_ZERO_given_theta = lp_yi_given_theta(yi = 0, 
                                                                      theta = grid_theta))
  
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
    
    # steps in calculating EIG
    # 1. compute current posterior grid
    for (f in 1:params$n_features) {
      # update likelihood
      z_given_theta[[t]][[f]] <- get_z_given_theta(t = t, f = f,
                                                   y_given_theta = y_given_theta,
                                                   z_given_theta = z_given_theta,
                                                   model = model)
      
      # update posterior
      post[[t]][[f]] <- get_post(z_given_theta = z_given_theta[[t]][[f]], 
                                 theta_epsilon = theta_epsilon, 
                                 post = post[[t]][[f]])
    }
    
    # -compute new posterior grid over all possible outcomes
    
    # -compute KL between old and new posterior 
    for (o in 1:nrow(possible_observations)) {
      for (f in 1:params$n_features) {
        # pretend that the possible observation has truly been observed
        # note that's observed from the same stimulus as the previous one
        model[t+1, grepl("^f", names(model))] <- as.list(possible_observations[o,])
        model$stimulus_idx[t+1] <- stimulus_idx
        
        # get upcoming likelihood
        z_given_theta_upcoming[[o]][[f]] <- get_z_given_theta(t = t+1, f = f,
                                                              y_given_theta = y_given_theta,
                                                              z_given_theta = z_given_theta,
                                                              model = model)
        
        # upcoming posterior
        post_upcoming[[o]][[f]] <- get_post(z_given_theta = z_given_theta_upcoming[[o]][[f]], 
                                            theta_epsilon = theta_epsilon, 
                                            post = post[[t]][[f]])
        
        # kl between old and new posteriors
      }
    }
    
    # compute EIG
    model$EIG[[t]] <- get_eig_with_combos(unique_combination_df = current_unique_poss_combos,
                                          possible_combinations = poss_combos,
                                          n_feature = params$n_features)
      
    # luce choice probability whether to look away
    model$p_look_away[t] = params$eig_from_world / (model$EIG[t] + params$eig_from_world)
    
    # consider forced exposure case
    if (forced_exposure) {
      if(stimulus_idx == 1 && t >= forced_sample){
        model$look_away[t] = TRUE
      } else if (stimulus_idx == 1 && t < forced_sample) {
        model$look_away[t] = FALSE
      } else {
        model$look_away[t] = rbinom(1, 1, prob = model$p_look_away[t]) == 1
      }
    } else {
      # actual choice of whether to look away is sampled here
      model$look_away[t] = rbinom(1, 1, prob = model$p_look_away[t]) == 1
    }
    
    # if look away, increment
    if (model$look_away[t] == TRUE) {
      stimulus_idx <- stimulus_idx + 1
    }
    
    # update books
    t <- t+1
  } # FINISH HUGE WHILE LOOP
  
  return(model)  
}

