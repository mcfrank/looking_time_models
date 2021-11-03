# SCRIPT INCLUDES ALL PROBABILITY COMPUTATIONS
#
# Naming conventions
# - `lp_` means "log probability" and is in the name of all variables holding this info
# - all functions have `score_` in their name and (for length) not `lp_`
# - score functions return log probabilities
# - no datatypes are in the names of the variables. 
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
    last_stim_last_t <- max(model$t[stimulus_idx == stimulus_idx - 1])
    this_lp_z_given_theta$lp_z_given_theta <- this_lp_z_given_theta$lp_z_given_theta + 
      lp_z_given_theta[[last_stim_last_t]][[f]]$lp_z_given_theta
  }
  
  return(this_lp_z_given_theta)
}

# ---------------- score_post ---------------------
# update posterior
# rolls in likelihood and prior, does logsumexp
score_post <- function(lp_z_given_theta, lp_prior, lp_post) {
  
  post$unnormalized_log_posterior <- lp_z_given_theta$lp_z_given_theta + 
    lp_theta_epsilon$lp_theta + 
    lp_theta_epsilon$lp_epsilon
  post$log_posterior <- post$unnormalized_log_posterior - matrixStats::logSumExp(post$unnormalized_log_posterior)
  post$posterior <- exp(post$log_posterior)
  
  return(post)
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
