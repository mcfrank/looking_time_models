main_simulation_uc <- function(subject = x,
                stimuli_sequence, 
                noise_parameter = noise_parameter, 
                eig_from_world = 0.001,
                max_observation = 5000, # should this be per trial or in total? currently per trial 
                grid_theta = grid_theta, 
                grid_epsilon = grid_epsilon, 
                alpha_prior = alpha_prior, 
                beta_prior = beta_prior,
                alpha_epsilon = alpha_epsilon, 
                beta_epsilon = beta_epsilon, 
                forced_exposure = TRUE,
                forced_sample = 5){
  
  
  feature_number <- ncol(stimuli_sequence[startsWith(names(stimuli_sequence), 
                                                     "V")])
  total_trial_number = max(stimuli_sequence$trial_number)
  
  # df for keeping track of model behavior, 
  df_model <-  tibble(subject_id = rep(subject, max_observation),
                      t = rep(NA,max_observation),
                      stimulus_idx = rep(NA,max_observation), 
                      EIG = rep(NA,max_observation), 
                      EIG_from_world = rep(eig_from_world,max_observation),
                      p_look_away = rep(NA,max_observation), 
                      look_away = rep(NA,max_observation)) 
 
  
  # matrix for keep track of actual observations 
  m_observation <- matrix(ncol = feature_number, 
                          nrow = max_observation)
  colnames(m_observation) <- grep("V",names(stimuli_sequence), value = TRUE)
   
  
  
  # create a list of dfs to keep track of ALL posterior distribution 
  # actually may consider keeping track of everything in matrix for multi-feature version 
  # this way we can always look up the posterior from previous observation 
  
  # material for calculating df_posterior 
  df_lp_theta_epsilon <- get_df_lp_theta_epsilon(grid_theta, grid_epsilon, 
                                                 alpha_theta, beta_theta, 
                                                 alpha_epsilon, beta_epsilon)
  # df for keep track of posterior distribution of each individual feature 
  df_posterior <- expand_grid(theta = grid_theta,
                              epsilon = grid_epsilon)
  # not sure when do we really need the non-log one, save some $$$  
  df_posterior$unnormalized_log_posterior <- NA_real_
  df_posterior$log_posterior <- NA_real_
  df_posterior$posterior <- NA_real_

  ll_df_posterior <- lapply(seq(1, max_observation, 1), 
                              function(x){
                                lapply(seq(1, feature_number, 1), 
                                       function(y){
                                         df_posterior
                                       })
                                })
  
  
  
  df_z_given_theta <- tibble(
    "theta" = df_posterior$theta, 
    "epsilon" = df_posterior$epsilon,
  )
  
  df_z_given_theta$lp_z_y_ONE <- NA_real_
  df_z_given_theta$lp_z_y_ZERO <-  NA_real_
  df_z_given_theta$lp_z_given_theta <- NA_real_
  
  
  ll_df_z_given_theta <- lapply(seq(1, total_trial_number, 1), 
                            function(x){
                              lapply(seq(1, feature_number, 1), 
                                     function(y){
                                       df_z_given_theta
                                     })
                            })
  
  
  
  
  stimulus_idx <- 1
  t <- 1

 # while(stimulus_idx <= total_trial_number && t <= max_observation){
    
  while(stimulus_idx <= total_trial_number && t <= max_observation){
    df_model$t[[t]] = t
    df_model$stimulus_idx[[t]] = stimulus_idx
    
    #get stimulus
    current_stimulus <- stimuli_sequence[stimulus_idx,]
    
    #get observation 
    current_observation <- noisy_observation_creature(
      creature = current_stimulus[,str_detect(names(current_stimulus), "V")], 
      n_sample = 1, 
      epsilon = noise_parameter
    )
  
    # add to current observation 
    m_observation[t, ] <- current_observation
  
    
   
     ## need to update posterior df here 
    
    current_unique_possible_combinations <- get_unique_combination(t, 
                                                                m_observation, 
                                                                feature_number)
  
    feature_pos <- current_unique_possible_combinations$feature_pos
    
    all_possible_combinations <- expand_grid(
      current_unique_possible_combinations, 
      hypothetical_observation = c(TRUE, FALSE)
      
    ) 
    
    n_possible_combination <- nrow(all_possible_combinations)
    all_possible_combinations$kl <- rep(NA_real_, n_possible_combination)
    all_possible_combinations$post_predictives <- rep(NA_real_, n_possible_combination)
    
    feature_occurence <- na.omit(as.vector(sapply(feature_pos, function(x){first(na.omit(x))})))
    for (index in feature_occurence){
      
      # this needs to be changed! we need to take into account of whether sth is taken from a new stimulus or not
      
      
      get_df_lp_z_given_theta <- function(t, 
                                        ll_df_z_given_theta, # needs to be about each observation, not each stimulus  
                                       index, 
                                       df_model, 
                                       m_observation,
                                       current_observation, 
                                       grid_theta, grid_epsilon, 
                                       alpha_theta, beta_theta, 
                                       alpha_epsilon, beta_epsilon){
        
        # see if this is a new stimulus or an old stimulus 
        # if starting a new stimulus 
        # relying on short circuiting to handle the t=1 part 
        if(t == 1 || df_model$stimulus_idx[[t]] != df_model$stimulus_idx[[t-1]]){
         
          df_lp_y_given_theta = tibble(
            "theta" = grid_theta, 
            "lp_y_ONE_given_theta" =  lp_yi_given_theta(yi = 1, theta = grid_theta ), 
            "lp_y_ZERO_given_theta" = lp_yi_given_theta(yi = 0, theta = grid_theta )
          )
          
          
          df_lp_z_given_y = tibble(
            "epsilon" = grid_epsilon, 
            "lp_z_given_y_ONE" = lp_z_ij_given_y(zij = current_observation[[index]], yi = 1, epsilon = grid_epsilon),
            "lp_z_given_y_ZERO" = lp_z_ij_given_y(zij = current_observation[[index]], yi = 0, epsilon = grid_epsilon)
          )
          
          df_lp_z_y_raw <- expand_grid(df_lp_y_given_theta, df_lp_z_given_y)
          
          
          # this needs to be keep track of separately 
          
          if (t == 1){
            df_lp_z_given_theta <- tibble(
              "theta" = df_lp_z_y_raw$theta, 
              "epsilon" = df_lp_z_y_raw$epsilon,
              "lp_z_y_ONE" = df_lp_z_y_raw$lp_y_ONE_given_theta + df_lp_z_y_raw$lp_z_given_y_ONE, 
              "lp_z_y_ZERO" = df_lp_z_y_raw$lp_y_ZERO_given_theta + df_lp_z_y_raw$lp_z_given_y_ZERO, 
              "lp_z_given_theta" = logSumExp(c(df_lp_z_y_raw$lp_y_ONE_given_theta + df_lp_z_y_raw$lp_z_given_y_ONE, 
                                               df_lp_z_y_raw$lp_y_ZERO_given_theta + df_lp_z_y_raw$lp_z_given_y_ZERO))
              )
            
            
          }else{
            df_lp_z_given_theta <- tibble(
              "theta" = df_lp_z_y_raw$theta, 
              "epsilon" = df_lp_z_y_raw$epsilon,
              "lp_z_y_ONE" = df_lp_z_y_raw$lp_y_ONE_given_theta + df_lp_z_y_raw$lp_z_given_y_ONE, 
              "lp_z_y_ZERO" = df_lp_z_y_raw$lp_y_ZERO_given_theta + df_lp_z_y_raw$lp_z_given_y_ZERO, 
              "lp_z_given_theta" = logSumExp(c(df_lp_z_y_raw$lp_y_ONE_given_theta + df_lp_z_y_raw$lp_z_given_y_ONE, 
                                               df_lp_z_y_raw$lp_y_ZERO_given_theta + df_lp_z_y_raw$lp_z_given_y_ZERO)) + 
                (ll_df_z_given_theta[[t-1]][[index]])$lp_z_given_theta
          )
            
          }
          
          
          
          
          
        }else{
          
          # needs to use the t to figure out what the last trial number's last t is 
          df_lp_y_given_theta = tibble(
            "theta" = grid_theta, 
            "lp_y_ONE_given_theta" =  lp_yi_given_theta(yi = 1, theta = grid_theta ), 
            "lp_y_ZERO_given_theta" = lp_yi_given_theta(yi = 0, theta = grid_theta )
          )
          
          
          # current only looking at single feature creature 
          # OK THIS ACTUALLY NEEDS TO BE ALL OBSERVATIONS ON THIS STIMULUS 
          current_stimulus_idx = df_model$stimulus_idx[[t]]
          last_t_for_last_stimulus = max((df_model[df_model$stimulus_idx == current_stimulus_idx-1,])$t, na.rm = TRUE)
          
          observations_on_this_stimulus_till_this_t = m_observation[last_t_for_last_stimulus+1:t, index]
    
          
          df_lp_z_given_y = tibble(
            "epsilon" = grid_epsilon, 
            "lp_z_given_y_ONE" = rowSums(sapply(observations_on_this_stimulus_till_this_t, 
                                                function(x){lp_z_ij_given_y(x, 1, grid_epsilon)})),
            "lp_z_given_y_ZERO" = rowSums(sapply(observations_on_this_stimulus_till_this_t, 
                                                 function(x){lp_z_ij_given_y(x, 0, grid_epsilon)}))
          )
          
          df_lp_z_y_raw <- expand_grid(df_lp_y_given_theta, df_lp_z_given_y)
          
          
          df_lp_z_given_theta <- tibble(
            "theta" = df_lp_z_y_raw$theta, 
            "epsilon" = df_lp_z_y_raw$epsilon,
            "lp_z_y_ONE" = df_lp_z_y_raw$lp_z_given_y_ONE + df_lp_y_given_theta$lp_y_ONE_given_theta, 
            "lp_z_y_ZERO" = df_lp_z_y_raw$lp_z_given_y_ZERO + df_lp_y_given_theta$lp_y_ZERO_given_theta,
            "lp_z_given_theta" = matrixStats::logSumExp(c(df_lp_z_y_raw$lp_z_given_y_ONE + 
                                                            df_lp_z_y_raw$lp_y_ONE_given_theta,
                                                          df_lp_z_y_raw$lp_z_given_y_ZERO + 
                                                            df_lp_z_y_raw$lp_y_ZERO_given_theta,
                                                          )) + 
              (ll_df_z_given_theta[[last_t_for_last_stimulus]][[index]])$lp_z_given_theta
          )
          
          
          
          
        }
        
        
        
      return (df_lp_z_given_theta)
        
      }
      
      
      ll_df_z_given_theta[[t]][[index]] <- get_df_lp_z_given_theta(t, 
                                                                   ll_df_z_given_theta, # needs to be about each observation, not each stimulus  
                                                                   index, 
                                                                   df_model, 
                                                                   m_observation,
                                                                   current_observation, 
                                                                   grid_theta, grid_epsilon, 
                                                                   alpha_theta, beta_theta, 
                                                                   alpha_epsilon, beta_epsilon)
      
      
      
      
      
      unnormalized_log_posterior <- ll_df_z_given_theta[[t]][[index]]$lp_z_given_theta + 
                                    df_lp_theta_epsilon$lp_theta + 
                                    df_lp_theta_epsilon$lp_epsilon
      
      
      
      ll_df_posterior[[t]][[index]]$unnormalized_log_posterior <- unnormalized_log_posterior
      ll_df_posterior[[t]][[index]]$log_posterior <- unnormalized_log_posterior - matrixStats::logSumExp(unnormalized_log_posterior)
      ll_df_posterior[[t]][[index]]$posterior <- exp(ll_df_posterior[[t]][[index]]$log_posterior)
      
      
     
    }
    
    for (i in 1:feature_number){
      
      # find corresponding calculated value 
      calculated_value_index <- match(TRUE, sapply(feature_pos, function(x){i %in% x}))
      calculated_value_index_in_ll <- feature_occurence[[calculated_value_index]]
      ll_df_posterior[[t]][[i]] <- ll_df_posterior[[t]][[calculated_value_index_in_ll]]
      ll_df_z_given_theta[[t]][[i]] <- ll_df_z_given_theta[[t]][[calculated_value_index_in_ll]]
    }
    
    
    prev_posterior_list <- ll_df_posterior[[t]][feature_occurence]
    
    post_posterior_list <- lapply(seq(1, n_possible_combination),
                                  function(x){
                                    expand_grid(theta = grid_theta, 
                                                epsilon = grid_epsilon)
                                  })
    
    for (i in 1:n_possible_combination){
      post_posterior_df = post_posterior_list[[i]]
      prev_observation_posterior = prev_posterior_list[[ceiling(i/2)]]
      post_posterior_list[[i]] <- update_posterior(previous_posterior_df =  prev_observation_posterior,
                                                   current_posterior_df = post_posterior_list[[i]], 
                                                   (i%%2 == 1), 
                                                   grid_theta, grid_epsilon)
    }
    
    for (s in 1:n_possible_combination){
      
      all_possible_combinations$kl[s] <- get_kl(post_posterior_list[[s]]$posterior, 
                                                prev_posterior_list[[ceiling(s/2)]]$posterior)
      all_possible_combinations$post_predictives[s] <- noisy_post_pred(prev_posterior_list[[ceiling(s/2)]]$theta, 
                                                                       prev_posterior_list[[ceiling(s/2)]]$epsilon, 
                                                                       prev_posterior_list[[ceiling(s/2)]]$posterior, 
                                                                       all_possible_combinations$hypothetical_observation[s]) 
      
    }
    
    
     df_model$EIG[[t]] <- get_eig_with_combinationa(unique_combination_df = current_unique_possible_combinations,
                                                    all_possible_combinations = all_possible_combinations,
                                                    n_feature = feature_number)
     
     df_model$p_look_away[t] = eig_from_world / (df_model$EIG[t] + eig_from_world)
     
     # try to force short exposure at the first trial 
     if(forced_exposure){
       if(stimulus_idx == 1 && t >= forced_sample){
        
         df_model$look_away[t] = TRUE
       }else{
         df_model$look_away[t] = rbinom(1, 1, prob = df_model$p_look_away[t]) == 1
       }
     }else{
       
       df_model$look_away[t] = rbinom(1, 1, prob = df_model$p_look_away[t]) == 1
     }
     
     
     if (df_model$look_away[t]==TRUE) {
       stimulus_idx <- stimulus_idx + 1
     }
     
     df_model$forced_sample = forced_sample
     
     
    t = t+1
    #stimulus_idx = stimulus_idx + 1
  }
  return (df_model)  

}
  
  
  
  
  
