source(here('helper/grid_approximation.r'))
source(here("helper/d_update_post.r"))
source(here("helper/d_post_for_EIG.r"))
source(here("helper/initialization.r"))

# runs main simulation computing EIG 
# takes a df of parameters and some globals
main_simulation <- function(params_df = df,
                            grid_theta = seq(0.1, 1, 0.2),
                            grid_epsilon = seq(0.1, 1, 0.2),
                            forced_exposure = FALSE,
                            forced_sample = 5) {
  
  ### BOOK-KEEPING
  n_features <- ncol(params_df$stimuli_sequence$data[[1]]
                         [startsWith(names(params_df$stimuli_sequence$data[[1]]), 
                                     "V")])
  total_trial_number = max(params_df$stimuli_sequence$data[[1]]$trial_number)
  
  # df for keeping track of model behavior
  df_model <-  initialize_model(params_df$eig_from_world, params_df$max_observation)
  # matrix of the noisy observations
  m_observations <- initialize_m_observation(n_features, params_df$max_observation, 
                                            params_df$stimuli_sequence$data[[1]])
  # list of lists of df for the posteriors
  ll_df_post <- initialize_ll_df_posterior(grid_theta, grid_epsilon, 
                                           params_df$max_observation, n_features)
  # list of lists of df for the likelihoods
  ll_df_z_given_theta <- initialize_ll_df_z_given_theta(grid_theta, grid_epsilon, 
                                                        params_df$max_observation, 
                                                        n_features)
  
  # dataframe of thetas and epsilons
  df_lp_theta_epsilon <- get_df_lp_theta_epsilon(grid_theta, grid_epsilon, 
                                                 params_df$alpha_prior,  params_df$beta_prior, 
                                                 params_df$alpha_epsilon, params_df$beta_epsilon)
  
  df_lp_y_given_theta = tibble(theta = grid_theta, 
                               lp_y_ONE_given_theta = 
                                 lp_yi_given_theta(yi = 1, 
                                                   theta = grid_theta), 
                               lp_y_ZERO_given_theta = 
                                 lp_yi_given_theta(yi = 0, 
                                                   theta = grid_theta))
  
  ### MAIN MODEL LOOP
  stimulus_idx <- 1
  t <- 1
  
  # while we haven't run out of stimuli or observations, 
  # sample a new observation
  # compute expected information gain
  # make a choice what to do
  while(stimulus_idx <= total_trial_number && t <= params_df$max_observation){
    df_model$t[t] = t
    df_model$stimulus_idx[t] = stimulus_idx
    
    # get stimulus, observation, add to current observation
    current_stimulus <- params_df$stimuli_sequence$data[[1]][stimulus_idx,]
    current_observation <- noisy_observation_creature(
      creature = current_stimulus[,str_detect(names(current_stimulus), "V")], 
      n_sample = 1, 
      epsilon = params_df$noise_parameter
    )
    m_observations[t, ] <- current_observation
    
    # steps in calculating EIG
    # 1. compute current posterior grid
    for (f in 1:n_features) {
      # update likelihood
      # todo: streamline this function
      ll_df_z_given_theta[[t]][[f]] <- 
        get_df_lp_z_given_theta(t = t, f = f,
                                df_lp_y_given_theta = df_lp_y_given_theta,
                                ll_df_z_given_theta = ll_df_z_given_theta,
                                df_model = df_model, 
                                current_observation = current_observation)
      
      # update posterior
      ll_df_post[[t]][[f]] <- get_df_post(z_given_theta = ll_df_z_given_theta[[t]][[f]], 
                                          df_lp_theta_epsilon = df_lp_theta_epsilon, 
                                          df_post = ll_df_post[[t]][[f]])
    }
    
    # -compute new posterior grid over all possible outcomes
    # -compute KL between old and new posterior 
    # TODO: initialize ll_df_z_given_theta_upcoming and ll_df_post_upcoming
    for (o in 1:n_poss_outcomes) {
      for (f in 1:n_feature) {
        ll_df_z_given_theta[[t]][[f]] <- 
          get_df_lp_z_given_theta(t = t, f = f,
                                  df_lp_y_given_theta = df_lp_y_given_theta,
                                  ll_df_z_given_theta = ll_df_z_given_theta,
                                  df_model = df_model, 
                                  current_observation = current_observation)
        
        # update posterior
        ll_df_post[[t]][[f]] <- get_df_post(z_given_theta = ll_df_z_given_theta[[t]][[f]], 
                                            df_lp_theta_epsilon = df_lp_theta_epsilon, 
                                            df_post = ll_df_post[[t]][[f]])
      }
    }
    
    
    
    # compute EIG
    df_model$EIG[[t]] <- get_eig_with_combos(unique_combination_df = current_unique_poss_combos,
                                             all_possible_combinations = all_poss_combos,
                                             n_feature = n_features)
    
    # luce choice probability whether to look away
    df_model$p_look_away[t] = params_df$eig_from_world / (df_model$EIG[t] + params_df$eig_from_world)
    
    # consider forced exposure case
    if (forced_exposure) {
      if(stimulus_idx == 1 && t >= forced_sample){
        df_model$look_away[t] = TRUE
      } else if (stimulus_idx == 1 && t < forced_sample) {
        df_model$look_away[t] = FALSE
      } else {
        df_model$look_away[t] = rbinom(1, 1, prob = df_model$p_look_away[t]) == 1
      }
    } else {
      # actual choice of whether to look away is sampled here
      df_model$look_away[t] = rbinom(1, 1, prob = df_model$p_look_away[t]) == 1
    }
    
    # look away
    if (df_model$look_away[t] == TRUE) {
      stimulus_idx <- stimulus_idx + 1
    }
    
    # update books
    df_model$forced_sample <- forced_sample
    t <- t+1
  } # FINISH HUGE WHILE LOOP
  
  return(df_model)  
}
