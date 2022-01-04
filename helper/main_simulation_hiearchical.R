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
    
    #figure out the different combinations on 
    current_trial_all_possible_combinations <- total_possible_combinations[[stimulus_idx]]
    current_trial_all_y_value_combinations <- total_possible_y_combination[[stimulus_idx]]
    
    # create grid to contain different combinations 
    
    # steps in calculating EIG
    
    # - compute current posterior grid
    for (f in 1:params$n_features) {
      
      # do some calulcation for the one concept world 
      ONE_CONCEPT_WORLD_RESULTS <- SOME_CALC
      
      # TWO CONCEPT CALUCLATION, PROBABLY SHOULD GO TO A FUNCTION LATER 
      
      for(i in nrow(current_trial_all_possible_combinations)){
          
          # first figure out which combo we are on
          current_combo <- current_trial_all_possible_combinations[i, ]
          
          
          # figure out how many lambda to attach to 
          # this is buggy, needs to take into acount of the 0 
          n_concept_one_occurence <- length(which(current_combo == 1))
          n_concept_two_occurence <- length(which(current_combo == 2))
          
          
          # figure out the p(y|theta_1) p(y|theta_2) components 
          lp_y_ONE_theta_1 <- lp_y_given_theta_one$lp_y_ONE_given_theta_ONE
          lp_y_ONE_theta_2 <- lp_y_given_theta$lp_y_ONE_given_theta_TWO
          
          lp_y_ZERO_theta_1 <- lp_y_given_theta_one$lp_y_ZERO_given_theta_ONE
          lp_y_ZERO_theta_2 <- lp_y_given_theta$lp_y_ZERO_given_theta_TWO
          
          likelihood_with_two_concepts <- vector(mode = "list", 
                                                 length = nrow(current_trial_all_possible_combinations))
          
          select_y_val_theta <- function(lp_y_given_theta, concept, y_val){
            if(y_val == 0 & concept == 2){
              lp_y_ZERO_theta_2
            }else if(y_val == 1 & concept == 2){
              lp_y_ONE_theta_2
            }else if(y_val == 1 & concept == 1){
              lp_y_ONE_theta_1
            }else if(y_val == 0 & concept == 1){
              lp_y_ZERO_theta_1
            }
          }
          
          
        
            
            
            # first figure out all the observation related stuff, put it in a list to be used later  
            ldf_lp_z_given_y <- ACC_LIST 
            for (i in 1:nrow(current_trial_all_y_value_combinations)){
      
              # the inner loop tries to figure out the observation part (p(z|y))
              lp_z_given_y_acc <- ACC 
              for (trial_n in 1:length(current_trial_all_y_value_combinations[i, ])){
                observations_on_this_trial <-  filter(model, stimulus_idx == trial_n) %>%
                  select(paste0("f", f)) %>%
                  pull()
                z_bar_given_y <- rowSums(sapply(observations_on_this_trial, 
                                                function(x){ score_z_ij_given_y(x, 
                                                                                current_trial_all_y_value_combinations[i, trial_n], 
                                                                                grid_epsilon)}))
                lp_z_given_y_acc <- lp_z_given_y_acc + z_bar_given_y
              }
              
              ldf_lp_z_given_y[i] <- lp_z_given_y_acc
            }
            
            
            # then figure out the middle part 
            ldf_lp_y_given_theta <- ACC_LIST 
            
           
            
            for (i in 1:nrow(current_trial_all_y_value_combinations)){
              lp_y_theta <- ACC 
              lp_y_theta_list <- ACC_LIST
              
              
              for(theta_p in 1:nrow(current_trial_all_possible_combinations)){
        
              
                for (j in 1:length(current_trial_all_y_value_combinations[i,])){
                  current_lp_y_theta <- select_y_val_theta(lp_y_given_theta, 
                                                           concept = current_trial_all_possible_combinations[i ,j], 
                                                           y_val = current_trial_all_y_value_combinations[i ,j])
                  lp_y_theta <- lp_y_theta + current_lp_y_theta
                }
                n_concept_one_occurence <- length(which(current_trial_all_possible_combinations[i, ] == 1))
                n_concept_two_occurence <- length(which(current_trial_all_possible_combinations[i, ] == 2))
                lp_y_theta <- lp_y_theta + (lp_gamma_1 * n_concept_one_occurence) + (lp_gamma_2 * n_concept_two_occurence)
                
                lp_y_theta_list[i] <- lp_y_theta
              }
            }
            rowLogSum()
            
              

              # the second inner loop tries to figure out the different combinations of p(y|theta part)
              # this also needs to have rowlogSum to put together 
              
          
              
            
              lp_y_theta_total <- rowlogSum(lp_y_theta_total, lp_y_theta)
              
              lp_z_given_y_acc + lp_y_theta 
            
            
            
            
          
          # 
          for (i in 1:nrow(current_trial_all_y_value_combinations)){
            for (j in 1:length(current_trial_all_y_value_combinations[i,])){
              
              # figure out observation on the trial number 
              observations_on_this_trial <-  filter(model, stimulus_idx == j) %>%
                select(paste0("f", f)) %>%
                pull()
              
              z_bar_given_y_ZERO = rowSums(sapply(observations_on_this_trial, 
                                                  function(x){ score_z_ij_given_y(x, 0, grid_epsilon)}))
              
              if (current_trial_all_y_value_combinations[i,j] == 0){
                
                
            
                
                
               # select zero based stuff and do things 
               lp_y_ZERO_theta_1 * n_concept_one_occurence + lp_y_ZERO_theta_2 * n_concept_two_occurence + 
               (lp_gamma_1 * n_concept_one_occurence) + (lp_gamma_2 * n_concept_two_occurence)
             } else{
               # select ONE based stuff
               
               z_bar_given_y_ONE = rowSums(sapply(observations_on_this_trial, 
                                                   function(x){ score_z_ij_given_y(x, 0, grid_epsilon)}))
               
               lp_y_ONE_theta_1 * n_concept_one_occurence + lp_y_ONE_theta_2 * n_concept_two_occurence + 
                 (lp_gamma_1 * n_concept_one_occurence) + (lp_gamma_2 * n_concept_two_occurence)
             }
            }
          }
          
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
