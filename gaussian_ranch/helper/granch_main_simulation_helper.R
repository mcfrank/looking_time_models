# THIS SCRIPT CONTAINS THREE MAIN SIMULATION FUNCTION
# 1. main_simulation (RANCH with EIG)
# 2. backward_IM_main_simulation (RANCH with KL and Surprisal)
# 3. main_simulation_random_looking (Baseline No Learnign Model)


source(here("helper/init.r"))
source(here("helper/compute_prob.r"))


## ----------------- main_simulation -------------------
# runs main simulation computing EIG 
# takes a df of parameters and some globals
granch_main_simulation <- function(params = df, testing = TRUE) {
  
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
  ll_post <- initialize_post_df(
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
        hypothetical_obs_posterior$posterior[hypothetical_obs_posterior$posterior < exp(-700)] <- .00001
        ll_post[[t]][[f]]$posterior[ll_post[[t]][[f]]$posterior < exp(-700)] <- .00001

        kl_new[o,f] <- kl_div(hypothetical_obs_posterior$posterior,
                              ll_post[[t]][[f]]$posterior)

        p_post_new[o, f] <- get_post_pred(obs = all_posible_observations_on_current_stimulus[o,f],
                                              lp_post = ll_post[[t]][[f]] ,
                                              df_y_given_mu_sig_sq)
        model[t+1, paste0("f", f)] <- NA_real_

      }
    }

    ll_model_testing[[t]][[2]]<- kl_new
    ll_model_testing[[t]][[3]]<- p_post_new
    ll_model_testing[[t]][[4]] <- l_possible_obs_post

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









## ----------------- backward_IM_main_simulation -------------------

backward_IM_main_simulation <- function(params = df,
                                        grid_theta = seq(0.001, 1, 0.01),
                                        grid_epsilon = seq(0.001, 1, 0.01) 
) {
  
  ### BOOK-KEEPING 
  total_trial_number = max(params$stimuli_sequence$trial_number)
  
  # df for keeping track of model behavior
  model <-  initialize_model(params$world_EIG, params$max_observation, 
                             params$n_features, 
                             params$measurement)
  
  # list of lists of df for the posteriors and likelihoods
  lp_post <- initialize_posterior(grid_theta, grid_epsilon, 
                                  params$max_observation, params$n_features)
  
  
  lp_z_given_theta <- initialize_z_given_theta(grid_theta, grid_epsilon, 
                                               params$max_observation, 
                                               params$n_features)
  
  #  book-keeping for likelihoods and posteriors for new observations
  possible_observations <- get_possible_observations(params$n_features)
  lp_z_given_theta_new <- initialize_z_given_theta(grid_theta, grid_epsilon,
                                                   nrow(possible_observations), 
                                                   params$n_features)
  lp_post_new <- initialize_posterior(grid_theta, grid_epsilon, 
                                      nrow(possible_observations), 
                                      params$n_features)
  p_post_new <- matrix(data = NA, nrow = nrow(possible_observations), 
                       ncol = params$n_features)
  
  
  # dataframes of thetas and epsilons, and y given theta (these don't change)
  lp_prior <- score_prior(grid_theta, grid_epsilon, 
                          params$alpha_prior,  params$beta_prior, 
                          params$alpha_epsilon, params$beta_epsilon)
  # needs to get the distribution form so that we can calulate backward looking IM at t=1 
  lp_prior$prior_dist <- exp((lp_prior$lp_theta + lp_prior$lp_epsilon) - logSumExp((lp_prior$lp_theta + lp_prior$lp_epsilon)))
  
  
  lp_y_given_theta = tibble(theta = grid_theta, 
                            lp_y_ONE_given_theta = score_yi_given_theta(yi = 1, 
                                                                        theta = grid_theta), 
                            lp_y_ZERO_given_theta = score_yi_given_theta(yi = 0, 
                                                                         theta = grid_theta))
  
  # Keep track of backward-looking information metrics   
  if(params$measurement == "KL" | params$measurement == "surprisal"){
    im_all <- matrix(data = NA, nrow = max_observation + 1, 
                     ncol = params$n_features)
  } 
  
  
  
  
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
    
    # - compute current posterior grid
    for (f in 1:params$n_features) {
      # update likelihood
      
      lp_z_given_theta[[t]][[f]] <- 
        score_z_given_theta(t = t, f = f,
                            lp_y_given_theta = lp_y_given_theta,
                            lp_z_given_theta = lp_z_given_theta,
                            model = model)
      
      # update posterior
      lp_post[[t]][[f]] <- score_post(lp_z_given_theta = lp_z_given_theta[[t]][[f]], 
                                      lp_prior = lp_prior, 
                                      lp_post = lp_post[[t]][[f]])
      
      
      
    }
    
    # Calculate a KL between last two time points and decide if we want to move on 
    for (f in 1:params$n_features) {
      # if it is a KL, then the first KL should be between t = 1 and the prior   
      if (t == 1){
        if(params$measurement == "KL"){
          im_all[t, f] <- kl_div(lp_post[[t]][[f]]$posterior,
                                 lp_prior$prior_dist)
        }else if (params$measurement == "surprisal"){
          p_1 = exp(matrixStats::logSumExp( log(1 - lp_prior$epsilon) + log(lp_prior$theta) + log(lp_prior$prior_dist))) + 
            +     exp(matrixStats::logSumExp((log(lp_prior$epsilon) + log(1-lp_prior$theta) + log(lp_prior$prior_dist))))
          im_all[t, f] <- -log(p_1)
        }
        
        
      }else{
        if(params$measurement == "KL"){
          im_all[t, f] <- kl_div(lp_post[[t]][[f]]$posterior,
                                 lp_post[[t-1]][[f]]$posterior)
          
        }else if (params$measurement == "surprisal"){
          im_all[t, f] <- -log(get_post_pred(lp_post[[t-1]][[f]], 
                                             heads = current_observation[f]))
        }
        
      }
    }
    
    
    
    
    
    
    # compute KL across features 
    # for math behind this simplification: https://www.overleaf.com/project/618b40890437e356dc66539d
    
    
    
    # forced choice options
    if (t < params$forced_exposure_n){
      # less than the forced exposure n OR at the forced exposure n, should not to look away
      model$p_look_away[t] = 0
    }else if (t == params$forced_exposure_n){
      # when finally at the last one, definitely needs to look away
      model$p_look_away[t] = 1
    }else{
      # anything afterwards are all normal; also when forced_exposure_n = 0 it's all normal 
      if(params$measurement == "KL"){
        model$KL[t] <- sum(im_all[t, ]) 
        # luce choice probability whether to look away
        model$p_look_away[t] = rectified_luce_choice(x = params$world_EIG, 
                                                     y = model$KL[t])
        
      }else if(params$measurement == "surprisal"){
        model$surprisal[t] <- sum(im_all[t, ])  
        # luce choice probability whether to look away
        model$p_look_away[t] = rectified_luce_choice(x = params$world_EIG, 
                                                     y = model$surprisal[t])
      }
      
    }
    
    
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
