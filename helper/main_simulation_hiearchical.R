main_simulation_hiearchical <- function(params = df,
                            grid_theta = seq(0.001, 1, 0.01),
                            grid_epsilon = seq(0.001, 1, 0.01)) {
  
  ### BOOK-KEEPING 
  
  total_trial_number = max(params$stimuli_sequence$data[[1]]$trial_number)
  # generate all possible combinations of concept1 vs concept 2 for all the trials
  total_possible_combinations <- initialize_two_concepts_combinations(total_trial_number)
  total_possible_y_combination <- initialize_y_value_combinations(total_trial_number)
  
  # df for keeping track of model behavior
  model <-  initialize_model(params$world_EIG, params$max_observation, 
                             params$n_features)
  

  
  # dataframes of thetas and epsilons and lambda, and y given theta (these don't change)
  lp_prior <- score_hierarchical_prior(grid_theta, grid_epsilon, grid_lambda, 
                                       params$alpha_prior_theta_zero, params$beta_prior_theta_zero,
                                       params$alpha_prior_theta_one, params$beta_prior_theta_one,
                                       params$alpha_prior_theta_two, params$beta_prior_theta_two,
                                       params$alpha_lambda, params$beta_lambda, 
                                       params$alpha_epsilon, params$beta_epsilon)
  
  lp_y_given_theta = tibble(theta = grid_theta, 
                            lp_y_ONE_given_theta = score_yi_given_theta(yi = 1, 
                                                                        theta = grid_theta), 
                            lp_y_ZERO_given_theta = score_yi_given_theta(yi = 0, 
                                                                         theta = grid_theta))
  
  lp_y_given_theta_one = tibble(theta = grid_theta, 
                            lp_y_ONE_given_theta_ONE = score_yi_given_theta(yi = 1, 
                                                                        theta = grid_theta), 
                            lp_y_ZERO_given_theta_ONE = score_yi_given_theta(yi = 0, 
                                                                         theta = grid_theta))
  
  lp_y_given_theta_two = tibble(theta = grid_theta, 
                                lp_y_ONE_given_theta_TWO = score_yi_given_theta(yi = 1, 
                                                                            theta = grid_theta), 
                                lp_y_ZERO_given_theta_TWO = score_yi_given_theta(yi = 0, 
                                                                             theta = grid_theta))
  
  
  #keep track of z given theta for one concept world case 
  lp_z_given_theta <- initialize_z_given_theta(grid_theta, grid_epsilon, 
                                               params$max_observation, 
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
    
    #figure out the different combinations on 
    current_trial_all_possible_combinations <- total_possible_combinations[[stimulus_idx]]
    current_trial_all_y_value_combinations <- total_possible_y_combination[[stimulus_idx]]
    
    # create grid to contain different combinations 
    
    # steps in calculating EIG
    
    # - compute current posterior grid
    for (f in 1:params$n_features) {
      
      
      ##### ONE CONCEPT CALCULATION #####
      
      # recycling the old function because nothing changes 
      lp_z_given_theta[[t]][[f]] <- score_z_given_theta(t = t, f = f,
                                               lp_y_given_theta = lp_y_given_theta,
                                               lp_z_given_theta = lp_z_given_theta,
                                               model = model)
      ONE_CONCEPT_WORLD <- lp_z_given_theta[[t]][[f]] %>% 
        select(theta, epsilon, lp_z_given_theta)
      
      ##### TWO CONCEPT CALUCLATION ##### 
      
      #two accumulators
      # this one takes care of all the lp(z|y = 1) lp(z|y=0)
      ldf_lp_z_given_y <- vector(mode = "list", 
                                 length = nrow(current_trial_all_y_value_combinations))
      # this one takes care of all the lp(y|theta)lp(gamma)
      ldf_lp_y_given_theta <- vector(mode = "list", 
                                     length = nrow(current_trial_all_y_value_combinations))
      
      # this one keeps track of all the combined dataframe from the above two lists 
      ldf_lp_z_y_theta_gamma <- vector(mode = "list", 
                                       length = nrow(current_trial_all_y_value_combinations))
      
      # first filling in all the lp(z|y)
      # each row is one possibilities, later needs to be combined with p(y|theta)p(gamma)
      # so temporarily just being put into the list
      for (i in 1:length(ldf_lp_z_given_y)){
          # the inner loop tries to figure out the observation part (p(z|y))
          # this is an accumulator 
          lp_z_given_y_acc <- rep(0, length(grid_epsilon)) 
          for (trial_n in 1:stimulus_idx){
                observations_on_this_trial <-  filter(model, stimulus_idx == trial_n) %>%
                  select(paste0("f", f)) %>%
                  pull()
                z_bar_given_y <- rowSums(sapply(observations_on_this_trial, 
                                                function(x){ score_z_ij_given_y(x, 
                                                                                yi = current_trial_all_y_value_combinations[i, trial_n], 
                                                                                grid_epsilon)}))
                lp_z_given_y_acc <- lp_z_given_y_acc + z_bar_given_y
                
              }
          df_lp_z_given_y <- tibble(
            epsilon = grid_epsilon, 
            lp_z_given_y = lp_z_given_y_acc 
          )
          ldf_lp_z_given_y[[i]] <- df_lp_z_given_y
      }
      
      # then filling in all the p(y|theta)p(gamma)
      # also needs to loop through all the possible combination of y values 
      for (j in 1:length(ldf_lp_y_given_theta)){
        # first figure out what's the current combination 
        current_y_value_combo <- current_trial_all_y_value_combinations[j, ]
        ldf_lp_y_given_theta[[j]] <- get_y_theta_combination(current_y_value_combo, 
                                                             all_theta_value_combo = current_trial_all_possible_combinations, 
                                                             log(params$p_gamma), 
                                                             log(1-params$p_gamma))
      }
      
      # Now we have two lists, we want to expand the dataframe to put element from 
      # list A and element from list B together 
      for (i in 1:length(ldf_lp_z_y_theta_gamma)){
        ldf_lp_z_y_theta_gamma[[i]] <- expand_grid(ldf_lp_z_given_y[[i]], 
                                                   ldf_lp_y_given_theta[[i]])
        
        ldf_lp_z_y_theta_gamma[[i]]$lp_z_given_theta <- ldf_lp_z_y_theta_gamma[[i]]$lp_z_given_y + ldf_lp_z_y_theta_gamma[[i]]$lp_y_theta_gamma
        ldf_lp_z_y_theta_gamma[[i]] <- ldf_lp_z_y_theta_gamma[[i]] %>% 
          select(theta, epsilon, lp_z_given_theta)
      }
      
      TWO_CONCEPT_WORLD <- 
        tibble(
          theta = ldf_lp_z_y_theta_gamma[[1]]$theta, 
          epsilon = ldf_lp_z_y_theta_gamma[[1]]$epsilon,
          lp_z_given_theta_gamma = logSumExp_for_list(ldf_lp_z_y_theta_gamma, 
                             3)
        )
        
       df_for_likelihood <- left_join(ONE_CONCEPT_WORLD, TWO_CONCEPT_WORLD, 
                 by = c("theta", "epsilon")) %>% 
       left_join(lp_prior, by = c("theta", "epsilon"))
      
       m_for_likelihood <- cbind(
         df_for_likelihood$lp_z_given_theta + log(df_for_likelihood$lambda),
         df_for_likelihood$lp_z_given_theta_gamma + log(1-df_for_likelihood$lambda)
       )
     
       df_for_likelihood$final_likelihood <- rowLogSumExps(lx = m_for_likelihood[,])
      
      # CALCULATE POSTERIOR 
       
      
       unnormalized_log_posterior <-  df_for_likelihood$final_likelihood + df_for_likelihood$lp_lambda + 
        df_for_likelihood$lp_theta_zero + df_for_likelihood$lp_theta_one + df_for_likelihood$lp_theta_two
      
       log_posterior <- unnormalized_log_posterior - matrixStats::logSumExp(unnormalized_log_posterior)
      
    }
    
   
    
    #EIG are diffenre stories ignore below for now 
    
    # compute EIG
    # for math behind this simplification: https://www.overleaf.com/project/618b40890437e356dc66539d
    model$EIG[t] <- sum(p_post_new * kl_new)
    
    # luce choice probability whether to look away
    model$p_look_away[t] = rectified_luce_choice(x = params$world_EIG, 
                                                 y = model$EIG[t])
    
    
    # actual choice of whether to look away is sampled here
    model$look_away[t] = rbinom(1, 1, prob = model$p_look_away[t]) == 1
    
    # if look away, increment
    if (model$look_away[t] == TRUE) {
      stimulus_idx <- stimulus_idx + 1
    }
    
    
    t <- t+1
    
  } # FINISH HUGE WHILE LOOP
  
  return(model)  
}
