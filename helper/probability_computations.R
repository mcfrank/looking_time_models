# FILE INCLUDES ALL PROBABILITY COMPUTATIONS AND INFORMATION THEORY
#
# Naming conventions
# - `lp_` means "log probability" and is in the name of all variables holding this info
# - all functions have `score_` in their name and (for length) not `lp_`
# - score functions return log probabilities
# - no datatypes are in the names of the variables. 
# - TODO: distinguish between a single instance and the full dataset? 
#
# This script contains all testable computations for the models.


# ---------------- score_z_given_theta ---------------------
# main function updating likelihood
# returns a dataframe of cumulative log probabilities, p(z | theta)
score_z_given_theta <- function(t, # timestep
                                f, # feature
                                lp_y_given_theta, # cached likelihoods
                                lp_z_given_theta, # likelihoods
                                model) {
  
  # set up current variables
  this_lp_z_given_theta <- lp_z_given_theta[[t]][[f]]
  grid_epsilon <- unique(this_lp_z_given_theta$epsilon)
  grid_theta <- unique(this_lp_z_given_theta$theta)
  stimulus_idx <- model$stimulus_idx[t]
  
  # need to compute over all noisy observations of this stimulus
  observations_this_stimulus <- filter(model, stimulus_idx == stimulus_idx) %>%
    select(paste0("f", f)) %>%
    pull()
  
  # initialize log p(z|y)
  lp_z_given_y = tibble(epsilon = grid_epsilon)
  
  # compute probabilities over all observations of this stimulus
  lp_z_given_y$z_given_y_ONE = rowSums(sapply(observations_this_stimulus, 
                                              function(x){ lp_z_ij_given_y(x, 1, grid_epsilon)}))
  lp_z_given_y$z_given_y_ZERO = rowSums(sapply(observations_this_stimulus, 
                                               function(x){ lp_z_ij_given_y(x, 0, grid_epsilon)}))
  
  # clever expansion with cached likelihoods
  lp_z_y_theta <- expand_grid(lp_y_given_theta, lp_z_given_y)
  
  # update current observation
  this_lp_z_given_theta$lp_z_y_ZERO <- lp_z_y_theta$lp_y_ZERO_given_theta + lp_z_y_theta$z_given_y_ZERO
  this_lp_z_given_theta$lp_z_y_ONE <- lp_z_y_theta$lp_y_ONE_given_theta + lp_z_y_theta$z_given_y_ONE
  
  # likelihood of all samples for current stimulus
  this_lp_z_given_theta$lp_z_given_theta <- 
    rowLogSumExps(lx = as.matrix(this_lp_z_given_theta[,c("lp_z_y_ONE", "lp_z_y_ZERO")]))
  
  # add in likelihood for last sample from last stimulus, which includes all prior obs
  if (stimulus_idx > 1) {
    last_stim_last_t <- max(model$t[model$stimulus_idx == stimulus_idx - 1], na.rm=TRUE)
    this_lp_z_given_theta$lp_z_given_theta <- this_lp_z_given_theta$lp_z_given_theta + 
      lp_z_given_theta[[last_stim_last_t]][[f]]$lp_z_given_theta
  }
  
  return(this_lp_z_given_theta)
}

# ---------------- score_post ---------------------
# update posterior
# rolls in likelihood and prior, does logsumexp
score_post <- function(lp_z_given_theta, lp_prior, lp_post) {
  
  lp_post$unnormalized_log_posterior <- lp_z_given_theta$lp_z_given_theta + 
    lp_theta_epsilon$lp_theta + 
    lp_theta_epsilon$lp_epsilon
  lp_post$log_posterior <- lp_post$unnormalized_log_posterior - matrixStats::logSumExp(lp_post$unnormalized_log_posterior)
  lp_post$posterior <- exp(lp_post$log_posterior)
  
  return(lp_post)
}

# ---------------- score_z_ij_given_y ---------------------
# p(z_ij | y)
score_z_ij_given_y <- function(zij, yi, epsilon){
  
  if (zij == yi){
    log(1 - epsilon)
  }else{
    log(epsilon)
  }
}


# ---------------- score_prior ---------------------
# p(z_ij | y)
score_prior <- function(grid_theta, grid_epsilon, 
                        alpha_prior, beta_prior, 
                        alpha_epsilon, beta_epsilon) {

  thetas = tibble(theta = grid_theta, 
                  lp_theta = lp_theta(grid_theta, alpha_prior, beta_prior))
  epsilons = tibble(epsilon = grid_epsilon, 
                    lp_epsilon = lp_epsilon(grid_epsilon, alpha_epsilon, beta_epsilon))
  
  lp_theta_epsilon = expand_grid(thetas, epsilons)
  return(lp_theta_epsilon) 
}


# ---------------- rectified_luce_choice ----------------
# this crazy function is necessary because if the values get too close to 0, 
# this can be > 1 or < 0
rectified_luce_choice <- function(x, y) {
  max(min(x / (x + y), 1), 0)
}

# # ---------------- score_z_given_theta ---------------------
# score_z_given_theta <- function(observation, 
#                                 grid_theta, 
#                                 grid_epsilon) {
#   
#   thetas = tibble(theta = grid_theta, 
#                   yi_given_theta_y_TRUE = lp_yi_given_theta(yi = 1, theta = grid_theta), 
#                   yi_given_theta_y_FALSE = lp_yi_given_theta(yi = 0, theta = grid_theta))
#   
#   epsilons = tibble(epsilon = grid_epsilon, 
#                     zij_given_y_y_TRUE = lp_z_ij_given_y(zij = observation, yi = 1, epsilon = grid_epsilon), 
#                     zij_given_y_y_FALSE = lp_z_ij_given_y(zij = observation, yi = 0, epsilon = grid_epsilon))
#   
#   all = expand_grid(thetas, epsilons)
#   
#   m_lp_z_ij_given_thetas <- cbind(all$yi_given_theta_y_FALSE + all$zij_given_y_y_FALSE, 
#                                   all$yi_given_theta_y_TRUE +all$zij_given_y_y_TRUE)
#   
#   return(rowLogSumExps(m_lp_z_ij_given_thetas))
#   
# }
# 
# # ---------------- score_theta_given_z ---------------------
# score_theta_given_z <- function(z_bar, 
#                                 theta, epsilon, 
#                                 alpha_theta, beta_theta, 
#                                 alpha_epsilon, beta_epsilon ) {
#   
#   
#   
#   lp_z_given_theta(z_bar, theta, epsilon) + 
#     lp_theta(theta, alpha_theta, beta_theta) + 
#     lp_epsilon(epsilon, alpha_epsilon, beta_epsilon)
# }
# 
# 
# score_z_given_theta <- function(z_bar, 
#                                 theta, 
#                                 epsilon) {
#   sum(sapply(z_bar[[1]], 
#              function(x){lp_z_ij_given_theta(zij = x, 
#                                              theta = theta, 
#                                              epsilon = epsilon)}))
# }

# ---------------- score_z_ij_given_theta ---------------------
score_z_ij_given_theta <- function(zij, theta, epsilon) {
  logSumExp(
    c(lp_z_ij_given_y(zij = zij, yi = 1, epsilon = epsilon) + lp_yi_given_theta(yi = 1, theta = theta ), 
      lp_z_ij_given_y(zij = zij, yi = 0, epsilon = epsilon) + lp_yi_given_theta(yi = 0, theta = theta))
  )
}

# ---------------- score_z_ij_given_y ---------------------
score_z_ij_given_y <- function(zij, yi, epsilon){
  if (zij == yi) {
    log(1 - epsilon)
  } else {
    log(epsilon)
  }
}

# ---------------- score_yi_given_theta ---------------------
score_yi_given_theta <- function(yi, theta){
  # a cooler way to say that if yi = 1 then theta if yi = 0 then yi = 1-theta? 
  dbinom(yi, size = 1, prob = theta, log = TRUE)
}

# ---------------- score_theta ---------------------
score_theta <- function(theta, alpha_theta, beta_theta){
  dbeta(x = theta, shape1 = alpha_theta, shape2 = beta_theta, log = TRUE)
}

# ---------------- score_epsilon ---------------------
score_epsilon <- function(epsilon, alpha_epsilon, beta_epsilon){
  dbeta(x = epsilon, shape1 = alpha_epsilon, shape2 = beta_epsilon, log = TRUE)
}






# ---------------- compute KL divergence ---------------------
# TODO: this could be replaced with something native? 
# this is untested and has no input checking etc. 
kl_div <- function (x, y) {
  sum(x * log(x / y)) 
}

# ---------------- get_post_pred ---------------------
# get posterior predictive
# TODO: this is not done in log space and could have underflow errors!
get_post_pred <- function(lp_post, heads = TRUE) {
  p_1 <- sum(((1 - lp_post$epsilon) * lp_post$theta * lp_post$posterior) + 
               (lp_post$epsilon * (1-lp_post$theta) * lp_post$posterior))
  
  ifelse(heads, p_1, 1 - p_1)
}

 
# #########noisy_post_pred for entire creature#########
# 
# 
# creature_noisy_post_pred <- function(
#   outcome_index, 
#   all_possible_outcomes, 
#   posterior_at_t){
#   
#   # pre-optimization: 1260
#   # post-optimization: 
#   
#   # creature_noisy_post_pred(1, all_possible_creatures, posterior)
#   
#   
#   # calculate post predctive for each feature
#   feature_predictive <- lapply(seq(1, 
#                                    ncol(all_possible_outcomes[startsWith(names(all_possible_outcomes), 
#                                                                          "V")]), 
#                                    1), 
#                                function(x,
#                                         observation = all_possible_outcomes[all_possible_outcomes$index == outcome_index,],
#                                         posterior = posterior_at_t){
#                                  
#                                  f_posterior <- posterior[posterior$feature_index == x, ]
#                                  
#                                  f_observation <- observation[x] 
#                                  
#                                  noisy_post_pred(f_posterior$theta, 
#                                                  f_posterior$epsilon, 
#                                                  f_posterior$posterior, 
#                                                  f_observation)
#                                  
#                                }) 
#   
#   return(feature_predictive %>% unlist() %>% prod())
#   
#   
# }
# calculate_repetition_of_combination <- function(df, n_unique_combination){
#   
#   repetitions_for_all_combination <- vector(length = n_unique_combination)
#   for (i in 1:n_unique_combination){
#     true_occurence = df$n[[2*i-1]]
#     total_occurence = df$occurence[[2*i-1]]
#     repetitions_for_all_combination[i] <- choose(total_occurence, true_occurence)
#   }
#   
#   return (prod(repetitions_for_all_combination))
#   
#   
# }
# 



# 
# calculate_kls_faster <- function(
#   t, 
#   current_observation,
#   n_feature,
#   ll_df_posterior,
#   feature_pos,
#   all_possible_combinations, 
#   grid_theta, 
#   grid_epsilon, 
#   df_lp_theta_epsilon,
#   alpha_prior, 
#   beta_prior, 
#   alpha_epsilon, 
#   beta_epsilon
# ){
#   
#   n_possible_combination <- nrow(all_possible_combinations)
#   
#   all_possible_combinations$kl <- rep(NA_real_, n_possible_combination)
#   all_possible_combinations$post_predictives <- rep(NA_real_, n_possible_combination)
#   
#   feature_occurence <- na.omit(as.vector(sapply(feature_pos, function(x){first(na.omit(x))})))
#   print(t)
#   for (index in feature_occurence){
#     
#     if(t == 1){
#       ll_df_posterior[[t]][[index]] <-  init_update(ll_df_posterior[[t]][[index]], 
#                                                     df_lp_theta_epsilon, 
#                                                     current_observation[[index]],
#                                                     grid_theta, grid_epsilon,
#                                                     alpha_theta, beta_theta, 
#                                                     alpha_epsilon, beta_epsilon)
#     }else{
#       ll_df_posterior[[t]][[index]] <- update_posterior(previous_posterior_df = ll_df_posterior[[t-1]][[index]],
#                                                         current_posterior_df = ll_df_posterior[[t]][[index]], 
#                                                         current_observation[[index]], 
#                                                         grid_theta, grid_epsilon)
#       
#       
#       
#     }
#     
#     
#   }
#   # then fill in the rest of index 
#   for (i in 1:n_feature){
#     # find corresponding calculated value 
#     calculated_value_index <- match(TRUE, sapply(feature_pos, function(x){i %in% x}))
#     calculated_value_index_in_ll <- feature_occurence[[calculated_value_index]]
#     ll_df_posterior[[t]][[i]] <- ll_df_posterior[[t]][[calculated_value_index_in_ll]]
#   }
#   
#   
#   prev_posterior_list <- ll_df_posterior[[t]][feature_occurence]
#   
#   post_posterior_list <- lapply(seq(1, n_possible_combination),
#                                 function(x){
#                                   expand_grid(theta = grid_theta, 
#                                               epsilon = grid_epsilon)
#                                 })
#   
#   for (i in 1:n_possible_combination){
#     post_posterior_df = post_posterior_list[[i]]
#     prev_observation_posterior = prev_posterior_list[[ceiling(i/2)]]
#     post_posterior_list[[i]] <- update_posterior(previous_posterior_df =  prev_observation_posterior,
#                                                  current_posterior_df = post_posterior_list[[i]], 
#                                                  (i%%2 == 1), 
#                                                  grid_theta, grid_epsilon)
#   }
#   
#   for (s in 1:n_possible_combination){
#     
#     all_possible_combinations$kl[s] <- get_kl(post_posterior_list[[s]]$posterior, 
#                                               prev_posterior_list[[ceiling(s/2)]]$posterior)
#     all_possible_combinations$post_predictives[s] <- noisy_post_pred(prev_posterior_list[[ceiling(s/2)]]$theta, 
#                                                                      prev_posterior_list[[ceiling(s/2)]]$epsilon, 
#                                                                      prev_posterior_list[[ceiling(s/2)]]$posterior, 
#                                                                      all_possible_combinations$hypothetical_observation[s]) 
#     
#   }
#   return (all_possible_combinations)
# }
# 
# get_unique_combination <- function(t, 
#                                    m_observation, 
#                                    n_feature){
#   colnames(m_observation) <- NULL
#   # drop the NA rows 
#   curr_observations <- m_observation[rowSums(is.na(m_observation)) != n_feature, ]
#   
#   if(n_feature == 1){
#     unique_combination = list(curr_observations)
#     n_unique_combination <- 1
#   }else{
#     if(t == 1){
#       # single row 
#       unique_combination <- unique(curr_observations)
#       n_unique_combination <- length(unique_combination)
#     }else{
#       unique_combination <- data.frame(unique.matrix(curr_observations, 
#                                                      MARGIN = 2))
#       n_unique_combination = ncol(unique_combination)
#     }
#   }
#   
#   if(n_feature == 1){
#     unique_combination_df <- tibble(unique_combination = list(unique_combination))
#   }else{
#     unique_combination_df <- tibble(unique_combination =  as.list(unique_combination))
#   }
#   
#   unique_combination_df$occurence <-  rep(0, n_unique_combination)
#   unique_combination_df$feature_pos <- as.list(data.frame(matrix(nrow = n_feature,ncol = n_unique_combination)))
#   unique_combination_df$n <-  rep(NA, n_unique_combination)
#   
#   
#   
#   for (i in 1:n_unique_combination){
#     current_combination = unique_combination_df[[i, 1]][[1]]
#     for (f in 1:n_feature){
#       if (t == 1){
#         if (curr_observations[f] == current_combination){
#           unique_combination_df[i, 2] <- unique_combination_df[i, 2] + 1
#           unique_combination_df[[i, 3]][[1]][[f]] <- f
#         }
#       }else{
#         if(n_feature == 1){
#           unique_combination_df[i, 2] <- unique_combination_df[i, 2] + 1
#           unique_combination_df[[i, 3]][[1]][[f]] <- f
#         }else{
#           if (all(curr_observations[,f] == current_combination)){
#             unique_combination_df[i, 2] <- unique_combination_df[i, 2] + 1
#             unique_combination_df[[i, 3]][[1]][[f]] <- f
#           }
#         }
#         
#       }
#     }
#   }
#   
#   
#   
#   return(unique_combination_df)
#   
# }
# 
# # very confusing, may want to modularize this
# get_eig_with_combos <- function(unique_combination_df = current_all_possible_combinations,
#                                 all_possible_combinations = all_possible_combinations,
#                                 n_feature = feature_number){
#   l_comb <- lapply(unique_combination_df$occurence, 
#                    function(x){partitions::compositions(x, 2)}) 
#   n_unique_combination <- nrow(unique_combination_df)
#   
#   list_combination <- lapply(seq(1, length(l_comb)), 
#                              function(x){as.list(data.frame(as.matrix(l_comb[[x]])))}) %>% 
#     cross()
#   
#   matrix_combination <- sapply(list_combination, function(x){unlist(x)})
#   
#   
#   
#   number_of_unique_combinations = prod(sapply( unique_combination_df$occurence, 
#                                                function(x){choose(x+1, 1)}))
#   
#   assertthat::are_equal(length(list_combination), number_of_unique_combinations)
#   
#   
#   # figure out all the possible combinations 
#   list_all_possible_combination <- lapply(seq(1,  number_of_unique_combinations , 1), 
#                                           function(x){all_possible_combinations})
#   
#   for (i in 1:number_of_unique_combinations){
#     list_all_possible_combination[[i]]$n <- matrix_combination[, i]
#     
#   }
#   # calculate how many times the unique combination appears in the all possible scenarios
#   repetition_list <- sapply(list_all_possible_combination, 
#                             function(df){
#                               calculate_repetition_of_combination(df, n_unique_combination)
#                             })
#   assertthat::are_equal(sum((repetition_list)), 2^n_feature)
#   #unique(old_kls$kl)
#   eig_list <- sapply(list_all_possible_combination, 
#                      function(df){
#                        creature_kl <- sum(df$n * df$kl) 
#                        creature_post_pred <- prod(df$post_predictives ^ df$n)
#                        (creature_kl %*% creature_post_pred)[[1]]
#                      })
#   
#   total_eig <- sum(repetition_list * eig_list)
#   
#   
#   return (total_eig)
#   
#   
#   
# }
# 
# 
