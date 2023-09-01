# This file contains a main function and two helper functions to calculate im (surprisal and kl)

run_basic_model <- function(params){
  # initialize the model that keeps track of all the IM 
  total_trial_number <- max((params$stimuli_sequence$data[[1]])$trial_number)
  
  model <- tibble(
    # params_info = rep(params$params_info, total_trial_number),
    trial_number = seq(1, total_trial_number),  
    kl = rep(NA_real_, total_trial_number), 
    surprisal = rep(NA_real_, total_trial_number)
  )
  
  # initialize two matrices that keep track of the alpha count and beta count 
  alphas <- matrix(data = NA, nrow = total_trial_number + 1, # plus one because first row recording priors  
                   ncol = params$n_features)
  
  betas <- matrix(data = NA, nrow = total_trial_number + 1,  # plus one because first row recording priors  
                  ncol = params$n_features)
  
  # set up the priors 
  alphas[1, ] <- params$alpha_prior
  betas[1, ] <- params$beta_prior
  
  # initialize two vectors that keeps track of the feature-based suprirsal and KL 
  feature_surprisal <- matrix(data = NA, nrow = 1, 
                              ncol = params$n_features)
  
  feature_kl <- matrix(data = NA, nrow = 1, 
                       ncol = params$n_features)
  
  # iterate through the sequence 
  for (trial_number in 1:total_trial_number){
    # iterate through all features after observing the trial  
    for (f in 1:params$n_features){
      # update the two matrices 
      if (pull(params$stimuli_sequence$data[[1]][trial_number, f]) == TRUE){
        # when true, accumulating alpha count, retaining beta count 
        alphas[trial_number+1, f] <- alphas[trial_number, f] + 1
        betas[trial_number+1, f] <- betas[trial_number, f]
      }else{
        # when false, accumulating beta count 
        alphas[trial_number+1, f] <- alphas[trial_number, f]
        betas[trial_number+1, f] <- betas[trial_number, f] + 1
      }
      
      # update the S and KL matrices 
      #
      feature_surprisal[1,f] <- get_surprise(alphas[trial_number+1, f], 
                                             betas[trial_number+1, f], 
                                             pull(params$stimuli_sequence$data[[1]][trial_number, f]))
      
      feature_kl[1, f] <- get_kl(alphas[trial_number, f], betas[trial_number, f], 
                                 alphas[trial_number+1, f], betas[trial_number+1, f])
      
      
    }
    # sum across the s and kl 
    trial_surprisal <- sum(feature_surprisal)
    trial_kl <- sum(feature_kl)
    
    # update the model for values 
    model$kl[trial_number] <- trial_kl 
    model$surprisal[trial_number] <- trial_surprisal
    
  } #for loop for updating 
  
  return (model)
  
}




get_surprise <- function(alpha_count, beta_count, observation){
  p <- alpha_count / (alpha_count + beta_count)
  return (if_else(observation, -log2(p), -log2(1-p)))
}


get_kl <- function(old_alpha_count, old_beta_count, 
                   curr_alpha_count, curr_beta_count){
  
  dbeta_grid <- seq(0.0001, 1, 0.0001)
  
  prior <- dbeta(dbeta_grid, old_alpha_count, old_beta_count) / sum(dbeta(dbeta_grid, 
                                                                          old_alpha_count, 
                                                                          old_beta_count))
  
  post <- dbeta(dbeta_grid, curr_alpha_count, curr_beta_count) / sum(dbeta(dbeta_grid, 
                                                                           curr_alpha_count, 
                                                                           curr_beta_count))
  
  return(philentropy::KL(rbind( post, prior), unit = "log"))
  
  
}