main_simulation <- function(params = df,
                            grid_theta = seq(0.001, 1, 0.01),
                            grid_epsilon = seq(0.001, 1, 0.01)) {
  
  ### BOOK-KEEPING 
  
  total_trial_number = max(params$stimuli_sequence$data[[1]]$trial_number)
  # generate all possible combinations of concept1 vs concept 2 for all the trials
  total_possible_combinations <- FILL_ME_IN
  # SOMETHING THAT TRACKS EITHER Y IS EITHER ZERO OR ONE
  total_y_value_tracker <- FILL_ME_IN
  
  # df for keeping track of model behavior
  model <-  initialize_model(params$world_EIG, params$max_observation, 
                             params$n_features)
  

  
  # dataframes of thetas and epsilons, and y given theta (these don't change)
  lp_prior <- NEW_SCORE_PRIOR(....)
  lp_y_given_theta = tibble(theta = grid_theta, 
                            lp_y_ONE_given_theta = score_yi_given_theta(yi = 1, 
                                                                        theta = grid_theta), 
                            lp_y_ZERO_given_theta = score_yi_given_theta(yi = 0, 
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
    
    #TODO
    #figure out the different combinations on 
    CURRENT_COMBO <-ALL_COMBO[[stimulus_idx]]
    CURRENT_Y_COMBO <- total_y_value_tracker[[stimulus_idx]]
    
    # steps in calculating EIG
    # - compute current posterior grid
    for (f in 1:params$n_features) {
      
      # do some calulcation for the one concept world 
      ONE_CONCEPT_WORLD_RESULTS <- SOME_CALC
      
      # do some calculation for the two concept world 
      # for each feature loop through all possible combinations
      all_combo_y_ONE_likelihood <- #some length vector 
      all_combo_y_ZERO_likelihood <- # some length vector 
        
        for(i in nrow(current_two_concepts_combination)){
          
          # first figure out which combo we are on
          current_combo <- current_two_concepts_combination[i, ]
          
          
          # figure out how many lambda to attach to 
          # this is buggy, needs to take into acount of the 0 
          n_concept_one_occurence <- table(current_combo)[[1]]
          n_concept_two_occurence <- table(current_combo)[[2]]
          
          
          # figure out the p(y|theta_1) p(y|theta_2) components 
          lp_y_ONE_theta_1 <- lp_y_given_theta$lp_y_ONE_given_theta
          lp_y_ONE_theta_2 <- lp_y_given_theta$lp_y_ONE_given_theta
          lp_y_ZERO_theta_1 <- lp_y_given_theta$lp_y_ZERO_given_theta
          lp_y_ZERO_theta_2 <- lp_y_given_theta$lp_y_ZERO_given_theta
          
          # putting everything together 
          ACCUMULATOR <- STH 
          for (y in NUMBER_OF_Y_COMBINATION){
            #FIGURING OUT HOW TO PUT DIFFERENT COMPONENTS TOGETHER 
            
            
          }
          
          # get observation accumulation 
          lp_z_given_y$z_given_y_ONE = rowSums(sapply(observation_till_this_stimulus, 
                                                      function(x){ score_z_ij_given_y(x, 1, grid_epsilon)}))
          
          lp_z_given_y$z_given_y_ZERO = rowSums(sapply(observation_till_this_stimulus, 
                                                       function(x){ score_z_ij_given_y(x, 0, grid_epsilon)}))
          
          
          
          lp_combo_y_ONE_likelihood <- (lp_y_ONE_theta_1 * n_concept_one_occurence) + (lp_y_ONE_theta_2 * n_concept_one_occurence) + 
            (lp_gamma_1 * n_concept_one_occurence) + (lp_gamma_2 * n_concept_two_occurence)
          
          lp_combo_y_ZERO_likelihood <- (lp_y_ZERO_theta_1 * n_concept_one_occurence) + (lp_y_ZERO_theta_2 * n_concept_one_occurence) + 
            (lp_gamma_1 * n_concept_one_occurence) + (lp_gamma_2 * n_concept_two_occurence)
          
          # some sort of adding all combo together 
          all_combo_y_ONE_likelihood <- ACC #accumulating 
          all_combo_y_ZERO_likelihood <-ACC #accumulating 
          
        }
     
      
      
      # finally putting everything gether  
      # probalby needs to be rowwise or sth like that
      TWO_CONCEPT_WORLD <- logSumExp(lp_z_given_y$z_given_y_ONE +  all_combo_y_ONE_likelihood, 
                                     lp_z_given_y$z_given_y_ZERO +  all_combo_y_ZERO_likelihood)
      
      FINAL_RESULTS <- logSumExp((ONE_CONCEPT_WORLD + LOG_LAMBDA), 
                                 (TWO_CONCEPT_WORLD + LOG(1-LAMBDA)))
      
      
      
      
      
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
