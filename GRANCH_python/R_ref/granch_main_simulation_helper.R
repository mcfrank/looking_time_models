# THIS SCRIPT CONTAINS THREE MAIN SIMULATION FUNCTION
# 1. main_simulation (RANCH with EIG)
# 2. backward_IM_main_simulation (RANCH with KL and Surprisal)
# 3. main_simulation_random_looking (Baseline No Learnign Model)


granch_main_comparison <- function(params = df, testing = FALSE) {
  
  
  # grid info 
  grid_mu_theta <- (params$grid_mu_theta)[[1]]
  grid_sig_sq <- (params$grid_sig_sq)[[1]]
  grid_y <- (params$grid_y)[[1]]
  grid_epsilon <- (params$grid_epsilon)[[1]]
  hypothetical_obs_grid_n <- params$hypothetical_obs_grid_n
  
  ## constant dataframes 
  df_y_given_mu_sig_sq <- (params$df_y_given_mu_sig_sq)[[1]]
  prior_df <- (params$prior_df)[[1]]
  
  
  ### BOOK-KEEPING 
  total_trial_number = max(params$stimuli_sequence[[1]]$trial_number)
  
  # df for keeping track of model behavior
  model <-  initialize_model( params$world_EIG, params$max_observation,  params$n_features)
  
  # list of lists of df for the posteriors and likelihoods
  # require new function to stored the existing calculations 
  ll_z_given_mu_sig_sq <- initialize_z_given_mu_sig_sq(prior_df, 
                                                       params$max_observation, 
                                                       params$n_features)
  
  # could be further optimized
  ll_post <- initialize_post_df(prior_df,
                                params$max_observation, 
                                params$n_features)
  
  
  
  p_post_new <- matrix(data = NA, nrow = hypothetical_obs_grid_n ^ params$n_features, 
                       ncol =  params$n_features)
  kl_new <- matrix(data = NA, nrow = hypothetical_obs_grid_n ^ params$n_features, 
                   ncol =  params$n_features)
  
  
  basic_kl_pp_df <- tibble(
    kl = rep(NA_real_, params$max_observation), 
    pp = rep(NA_real_, params$max_observation)
  )
  
  
  ## make a list of list that keeps track of: 
  # at each time point t, (only looking at single fature case because it doesn't make too much sense to look at multiple )
  # the real posterior 
  # the kl 
  # the posterior predictives 
  ll_model_testing <- initialize_model_testing_infrastructrue(params$max_observation)
  
  
  
  
  
  ### MAIN MODEL LOOP
  stimulus_idx <- 1
  t <- 1
  
  # while we haven't run out of stimuli or observations, 
  # sample a new observation
  # compute expected information gain
  # make a choice what to do
  while(t < 11) {
    print(glue::glue("time: {t}"))
    model$t[t] = t
    model$stimulus_idx[t] = stimulus_idx
    
    # get stimulus, observation, add to model
    current_stimulus <-  params$stimuli_sequence[[1]][stimulus_idx, grepl("V", names(params$stimuli_sequence[[1]]))]
    
    current_observation <- noisy_observation(current_stimulus, epsilon = params$epsilon)
    
    all_posible_observations_on_current_stimulus <- get_all_possible_observations_for_stimulus(current_stimulus, 
                                                                                               epsilon = params$epsilon, 
                                                                                               grid_n = hypothetical_obs_grid_n)
    
    model[t, grepl("^f", names(model))] <- as.list(current_observation)
    
    # steps in calculating EIG
    # - compute current posterior grid
    for (f in 1:params$n_features) {
      # update likelihood
      ll_z_given_mu_sig_sq[[t]][[f]] <- score_z_given_mu_sig_sq(t, f, 
                                                                df_y_given_mu_sig_sq, # cached likelihoods
                                                                ll_z_given_mu_sig_sq, # this is going to be a list of list storing all the relevant info
                                                                model) 
      
      # update posterior
      ll_post[[t]][[f]] <- score_post(ll_z_given_mu_sig_sq[[t]][[f]],
                                      prior_df) 
      
      
      ll_model_testing[[t]][[1]] <- ll_post[[t]][[f]]
      
      
      if (t > 1){
        basic_kl_pp_df$kl[t] = kl_div(ll_post[[t]][[f]]$posterior, 
                                      ll_post[[t-1]][[f]]$posterior)
        basic_kl_pp_df$pp[t] = get_post_pred(obs = 0.11,
                                             lp_post = ll_post[[t]][[f]] ,
                                             df_y_given_mu_sig_sq)
      }
      
    }
    
    
    t <- t+1
    
  } # FINISH HUGE WHILE LOOP
  
  if(testing == TRUE){
    testing_output <- list(ll_model_testing, 
                           model)
    return(testing_output)
  }else{
    return(basic_kl_pp_df)
    #return(model)  
  }
  
  
  
}

## ----------------- main_simulation -------------------
# runs main simulation computing EIG 
# takes a df of parameters and some globals
granch_main_simulation <- function(params = df, testing = TRUE) {
  
  print("using very small substitue value now:")
  print("sequence_scheme:")
  print(params$sequence_scheme)  

    # grid info 
  grid_mu_theta <- (params$grid_mu_theta)[[1]]
  grid_sig_sq <- (params$grid_sig_sq)[[1]]
  grid_y <- (params$grid_y)[[1]]
  grid_epsilon <- (params$grid_epsilon)[[1]]
  hypothetical_obs_grid_n <- params$hypothetical_obs_grid_n
  
  ## constant dataframes 
   df_y_given_mu_sig_sq <- (params$df_y_given_mu_sig_sq)[[1]]
   prior_df <- (params$prior_df)[[1]]
 
   
  ### BOOK-KEEPING 
  total_trial_number = max(params$stimuli_sequence[[1]]$trial_number)
  
  # df for keeping track of model behavior
  model <-  initialize_model( params$world_EIG, params$max_observation,  params$n_features)

  # list of lists of df for the posteriors and likelihoods
  # require new function to stored the existing calculations 
  ll_z_given_mu_sig_sq <- initialize_z_given_mu_sig_sq(prior_df, 
                                                       params$max_observation, 
                                                       params$n_features)
  
  # could be further optimized
  ll_post <- initialize_post_df(prior_df,
                                params$max_observation, 
                                params$n_features)
 

  
  p_post_new <- matrix(data = NA, nrow = hypothetical_obs_grid_n ^ params$n_features, 
                       ncol =  params$n_features)
  kl_new <- matrix(data = NA, nrow = hypothetical_obs_grid_n ^ params$n_features, 
                   ncol =  params$n_features)
  
  
  
  ## make a list of list that keeps track of: 
  # at each time point t, (only looking at single fature case because it doesn't make too much sense to look at multiple )
  # the real posterior 
  # the kl 
  # the posterior predictives 
  ll_model_testing <- initialize_model_testing_infrastructrue(params$max_observation)
  
  
  
  

  ### MAIN MODEL LOOP
  stimulus_idx <- 1
  t <- 1
  
  # while we haven't run out of stimuli or observations, 
  # sample a new observation
  # compute expected information gain
  # make a choice what to do
  while(stimulus_idx <= total_trial_number && t <= params$max_observation) {
# while(t < 5){   
    print(glue::glue("time: {t}"))
    model$t[t] = t
    model$stimulus_idx[t] = stimulus_idx
    
    # get stimulus, observation, add to model
    current_stimulus <-  params$stimuli_sequence[[1]][stimulus_idx, grepl("V", names(params$stimuli_sequence[[1]]))]
  
    current_observation <- noisy_observation(current_stimulus, epsilon = params$epsilon)
    
    all_posible_observations_on_current_stimulus <- get_all_possible_observations_for_stimulus(current_stimulus, 
                                                                                               epsilon = params$epsilon, 
                                                                                               grid_n = hypothetical_obs_grid_n)
    
    model[t, grepl("^f", names(model))] <- as.list(current_observation)
    
    # steps in calculating EIG
    # - compute current posterior grid
    for (f in 1:params$n_features) {
      # update likelihood
      ll_z_given_mu_sig_sq[[t]][[f]] <- score_z_given_mu_sig_sq(t, f, 
                                                                df_y_given_mu_sig_sq, # cached likelihoods
                                                                ll_z_given_mu_sig_sq, # this is going to be a list of list storing all the relevant info
                                                                model) 
      
      # update posterior
      ll_post[[t]][[f]] <- score_post(ll_z_given_mu_sig_sq[[t]][[f]],
                                      prior_df) 
      
      
      ll_model_testing[[t]][[1]] <- ll_post[[t]][[f]]
    }
    
    
    # ---------- BELOW FOR EIG --------- #
    
    #get all possible observations

    model$stimulus_idx[t+1] <- stimulus_idx # pretend you're on the next stimulus


    l_possible_obs_post <- lapply(seq(1, nrow(all_posible_observations_on_current_stimulus), 1), function(y){NULL})

    for (o in 1:nrow(all_posible_observations_on_current_stimulus)) {
      for (f in 1:params$n_features) {

        model[t+1, paste0("f", f)] <- all_posible_observations_on_current_stimulus[o,f]

        hypothetical_obs_likelihood <- score_z_given_mu_sig_sq(t + 1, f,
                                                                df_y_given_mu_sig_sq, # cached likelihoods
                                                                ll_z_given_mu_sig_sq, # this is going to be a list of list storing all the relevant info
                                                                model)

        hypothetical_obs_posterior <- score_post(hypothetical_obs_likelihood,
                                        prior_df)


        l_possible_obs_post[[o]] <- hypothetical_obs_posterior

        #approximate 0 with small value
        hypothetical_obs_posterior$posterior[hypothetical_obs_posterior$posterior < exp(-700)] <- 1/(10^300)
        ll_post[[t]][[f]]$posterior[ll_post[[t]][[f]]$posterior < exp(-700)] <- 1/(10^300)

        kl_new[o,f] <- kl_div(hypothetical_obs_posterior$posterior,
                              ll_post[[t]][[f]]$posterior)

        p_post_new[o, f] <- get_post_pred(obs = all_posible_observations_on_current_stimulus[o,f],
                                              lp_post = ll_post[[t]][[f]] ,
                                              df_y_given_mu_sig_sq)
        model[t+1, paste0("f", f)] <- NA_real_

      }
    }

    ll_model_testing[[t]][[2]] <- kl_new
    ll_model_testing[[t]][[3]] <- p_post_new
    ll_model_testing[[t]][[4]] <- all_posible_observations_on_current_stimulus
    ll_model_testing[[t]][[5]] <- l_possible_obs_post

    model$stimulus_idx[t+1] <- NA_real_
    model[t+1, paste0("f", f)] <- NA_real_

    model$EIG[t] <- sum(p_post_new * kl_new)


      # currently taking away the probabilistic decision-making process

    model$look_away[t] = model$EIG[t] < model$EIG_from_world[t]
    #model$look_away[t] = t > 5
    
    #model$p_look_away[t] = rectified_luce_choice(x = model$EIG_from_world[t], 
    #                                               y = model$EIG[t])
  
    
   
    
    # actual choice of whether to look away is sampled here
    #model$look_away[t] = rbinom(1, 1, prob = model$p_look_away[t]) == 1
    
    # if look away, increment
    if (model$look_away[t] == TRUE) {
      stimulus_idx <- stimulus_idx + 1
    }
  
  
    t <- t+1
 
    } # FINISH HUGE WHILE LOOP
  
  if(testing == TRUE){
    testing_output <- list(ll_model_testing, 
                           model)
    return(testing_output)
  }else{
    return(model)  
  }
  
  
 
}









## ----------------- backward im metrics -------------------
# runs main simulation computing KL and surprisal  
# takes a df of parameters and some globals
granch_backward_im_simulation <- function(params = df, testing = FALSE) {
  
  print("using backward im now:")
  print(params$im)
  print("sequence_scheme:")
  print(params$sequence_scheme)  
  
  # grid info 
  grid_mu_theta <- (params$grid_mu_theta)[[1]]
  grid_sig_sq <- (params$grid_sig_sq)[[1]]
  grid_y <- (params$grid_y)[[1]]
  grid_epsilon <- (params$grid_epsilon)[[1]]
  hypothetical_obs_grid_n <- params$hypothetical_obs_grid_n
  
  ## constant dataframes 
  df_y_given_mu_sig_sq <- (params$df_y_given_mu_sig_sq)[[1]]
  prior_df <- (params$prior_df)[[1]]
  
  
  ### BOOK-KEEPING 
  total_trial_number = max(params$stimuli_sequence[[1]]$trial_number)
  

  
  model <-  initialize_model(params$world_EIG, params$max_observation,  params$n_features, params$im)
  
  # list of lists of df for the posteriors and likelihoods
  # require new function to stored the existing calculations 
  ll_z_given_mu_sig_sq <- initialize_z_given_mu_sig_sq(prior_df, 
                                                       params$max_observation, 
                                                       params$n_features)
  
  # could be further optimized
  ll_post <- initialize_post_df(prior_df,
                                params$max_observation, 
                                params$n_features)
  
  
  
  # Keep track of backward-looking information metrics   
  
    im_all <- matrix(data = NA, nrow = params$max_observation + 1, 
                     ncol = params$n_features)

  
  
  ## make a list of list that keeps track of: 
  # at each time point t, (only looking at single feature case because it doesn't make too much sense to look at multiple )
  # the real posterior 
  # the kl 
  # the posterior predictives 
  # ll_model_testing <- initialize_model_testing_infrastructrue(params$max_observation)
  
  
  
  
  
  ### MAIN MODEL LOOP
  stimulus_idx <- 1
  t <- 1
  
  # while we haven't run out of stimuli or observations, 
  # sample a new observation
  # compute expected information gain
  # make a choice what to do
  while(stimulus_idx <= total_trial_number && t < params$max_observation) {
  #while(t < 5){   
    print(glue::glue("time: {t}"))
    model$t[t] = t
    model$stimulus_idx[t] = stimulus_idx
    
    # get stimulus, observation, add to model
    current_stimulus <-  params$stimuli_sequence[[1]][stimulus_idx, grepl("V", names(params$stimuli_sequence[[1]]))]
    
    current_observation <- noisy_observation(current_stimulus, epsilon = params$epsilon)
    
   
    model[t, grepl("^f", names(model))] <- as.list(current_observation)
    
    # steps in calculating EIG
    # - compute current posterior grid
    for (f in 1:params$n_features) {
      # update likelihood
      ll_z_given_mu_sig_sq[[t]][[f]] <- score_z_given_mu_sig_sq(t, f, 
                                                                df_y_given_mu_sig_sq, # cached likelihoods
                                                                ll_z_given_mu_sig_sq, # this is going to be a list of list storing all the relevant info
                                                                model) 
      
      # update posterior
      ll_post[[t]][[f]] <- score_post(ll_z_given_mu_sig_sq[[t]][[f]],
                                      prior_df) 
      
      
      #ll_model_testing[[t]][[1]] <- ll_post[[t]][[f]]
    }
    
    
    # ---------- BELOW FOR IM --------- #
    


    for (f in 1:params$n_features){
      if (t == 1){
        if(params$im == "KL"){
          
          # we need to calculate a normalized prior 
          # unormalized prior: 
          unormalized_log_prior <- params$prior_df[[1]]$lp_mu_sig_sq + params$prior_df[[1]]$lp_epsilon
          normalized_prior <- exp(unormalized_log_prior - matrixStats::logSumExp(unormalized_log_prior))
          
          im_all[t, f] <- kl_div(corrected_vector(ll_post[[t]][[f]]$posterior), 
                                 normalized_prior)
          
        }else if (params$im == "surprisal"){
          
          # using the formula from calculating get post pred
          
          temp_df <- merge(params$prior_df[[1]], df_y_given_mu_sig_sq)
          temp_df$lp_z_given_mu_sig_sq_for_y = score_z_ij_given_y(z_val = current_observation, y_val = temp_df$y, epsilon = temp_df$grid_epsilon) +  
            temp_df$lp_y_given_mu_sig_sq
          
          # the order to list out aggregate group matters to get the order of the groupings right
          temp_df <- aggregate(lp_z_given_mu_sig_sq_for_y ~ grid_epsilon + grid_sig_sq
                               + grid_mu_theta, 
                               data = temp_df, FUN = matrixStats::logSumExp)
          
          unormalized_log_prior <- params$prior_df[[1]]$lp_mu_sig_sq + params$prior_df[[1]]$lp_epsilon
          normalized_log_prior <- unormalized_log_prior - matrixStats::logSumExp(unormalized_log_prior)
          
          p <- exp(logSumExp(temp_df$lp_z_given_mu_sig_sq + normalized_log_prior))
          
          im_all[t, f] <- -log(p)
         
        }
        
        
      }else{
        if(params$im == "KL"){
          im_all[t, f] <- kl_div(corrected_vector(ll_post[[t]][[f]]$posterior),
                                 corrected_vector(ll_post[[t-1]][[f]]$posterior))
          
        }else if (params$im == "surprisal"){
          

          im_all[t, f] <- -log(get_post_pred(obs = current_observation[f], 
                                             lp_post =  ll_post[[t-1]][[f]],
                                             df_y_given_mu_sig_sq))
          
        }
        
      }
      
    }
    
  
    
    
    
    #ll_model_testing[[t]][[2]] <- kl_new
    #ll_model_testing[[t]][[3]] <- p_post_new
    #ll_model_testing[[t]][[4]] <- all_posible_observations_on_current_stimulus
    #ll_model_testing[[t]][[5]] <- l_possible_obs_post
    
    model$stimulus_idx[t+1] <- NA_real_
    model[t+1, paste0("f", f)] <- NA_real_
    
    model$im_val[t] <- sum(im_all[t, ])
    
    
    # currently taking away the probabilistic decision-making process
    
    model$look_away[t] = model$im_val[t] < model$EIG_from_world[t]
    #model$look_away[t] = t > 5
    
    #model$p_look_away[t] = rectified_luce_choice(x = model$EIG_from_world[t], 
    #                                               y = model$EIG[t])
    
    
    
    
    # actual choice of whether to look away is sampled here
    #model$look_away[t] = rbinom(1, 1, prob = model$p_look_away[t]) == 1
    
    # if look away, increment
    if (model$look_away[t] == TRUE) {
      stimulus_idx <- stimulus_idx + 1
    }
    
    
    t <- t+1
    
  } # FINISH HUGE WHILE LOOP
  
  if(testing == TRUE){
    testing_output <- list(ll_model_testing, 
                           model)
    return(testing_output)
  }else{
    return(model)  
  }
  
  
  
}










## ----------------- main_simulation_random_looking -------------------


main_simulation_random_looking <- function(params = df,
                                           grid_theta = seq(0.001, 1, 0.01),
                                           grid_epsilon = c(0.000001)
) {
  
  ### BOOK-KEEPING 
  total_trial_number = max(params$stimuli_sequence$trial_number)
  
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

## ----------------- plot_posterior -------------------
plot_posterior <- function(p) {
  df <- map_df(1:3, 
         function(x) {
           p[[x]]$feature <- x
           return(p[[x]])
         })
  
  ggplot(df, aes(x = theta, y = epsilon, fill = posterior)) + 
    geom_tile() + 
    facet_wrap(~feature) + 
    viridis:::scale_fill_viridis()
}
