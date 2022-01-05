# FILE INCLUDES ALL PROBABILITY COMPUTATIONS AND INFORMATION THEORY
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
  this_stimulus_idx <- model$stimulus_idx[t]
  
  # need to compute over all noisy observations of this stimulus
  observations_this_stimulus <- filter(model, stimulus_idx == this_stimulus_idx) %>%
    select(paste0("f", f)) %>%
    pull()
  
  # initialize log p(z|y)
  lp_z_given_y = tibble(epsilon = grid_epsilon)
  
  # compute probabilities over all observations of this stimulus
  lp_z_given_y$z_given_y_ONE = rowSums(sapply(observations_this_stimulus, 
                                              function(x){ score_z_ij_given_y(x, 1, grid_epsilon)}))
  lp_z_given_y$z_given_y_ZERO = rowSums(sapply(observations_this_stimulus, 
                                               function(x){ score_z_ij_given_y(x, 0, grid_epsilon)}))
  
  # clever expansion with cached likelihoods
  lp_z_y_theta <- expand_grid(lp_y_given_theta, lp_z_given_y)
  
  # update current observation
  this_lp_z_given_theta$lp_z_y_ZERO <- lp_z_y_theta$lp_y_ZERO_given_theta + lp_z_y_theta$z_given_y_ZERO
  this_lp_z_given_theta$lp_z_y_ONE <- lp_z_y_theta$lp_y_ONE_given_theta + lp_z_y_theta$z_given_y_ONE
  
  # likelihood of all samples for current stimulus
  this_lp_z_given_theta$lp_z_given_theta <- 
    rowLogSumExps(lx = as.matrix(this_lp_z_given_theta[,c("lp_z_y_ONE", "lp_z_y_ZERO")]))
  
  # add in likelihood for last sample from last stimulus, which includes all prior obs
  if (this_stimulus_idx > 1) {
    last_stim_last_t <- max(model$t[model$stimulus_idx == this_stimulus_idx - 1], na.rm=TRUE)
    this_lp_z_given_theta$lp_z_given_theta <- this_lp_z_given_theta$lp_z_given_theta + 
      lp_z_given_theta[[last_stim_last_t]][[f]]$lp_z_given_theta
  }
  
  return(this_lp_z_given_theta)
}

# ---------------- score_post ---------------------
# update posterior
# rolls in likelihood and prior, does logsumexp
score_post <- function(lp_z_given_theta, lp_prior, lp_post) {
  
  # likelihood * prior
  lp_post$unnormalized_log_posterior <- lp_z_given_theta$lp_z_given_theta + 
    lp_prior$lp_theta + 
    lp_prior$lp_epsilon
  
  # normalize
  lp_post$log_posterior <- lp_post$unnormalized_log_posterior - matrixStats::logSumExp(lp_post$unnormalized_log_posterior)
  lp_post$posterior <- exp(lp_post$log_posterior)
  
  
  
  return(lp_post)
}

# ---------------- score_z_ij_given_y ---------------------
# p(z_ij | y)
score_z_ij_given_y <- function(zij, yi, epsilon) {
  
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
                  lp_theta = score_theta(grid_theta, alpha_prior, beta_prior))
  epsilons = tibble(epsilon = grid_epsilon, 
                    lp_epsilon = score_epsilon(grid_epsilon, alpha_epsilon, beta_epsilon))
  
  lp_theta_epsilon = expand_grid(thetas, epsilons)
  return(lp_theta_epsilon) 
}

# ---------------- score_hierarchical_prior ---------------------

score_hierarchical_prior <- function(grid_theta, grid_epsilon, grid_lambda, 
         alpha_prior_theta_zero, beta_prior_theta_zero,
         alpha_prior_theta_one, beta_prior_theta_one,
         alpha_prior_theta_two, beta_prior_theta_two,
         alpha_lambda, beta_lambda, 
         alpha_epsilon, beta_epsilon) {
  
  thetas = tibble(theta = grid_theta, 
                  lp_theta_zero = score_theta(grid_theta, alpha_prior_theta_zero, beta_prior_theta_zero), 
                  lp_theta_one = score_theta(grid_theta, alpha_prior_theta_one, beta_prior_theta_one), 
                  lp_theta_two = score_theta(grid_theta, alpha_prior_theta_two, beta_prior_theta_two)
                  )
  epsilons = tibble(epsilon = grid_epsilon, 
                    lp_epsilon = score_epsilon(grid_epsilon, alpha_epsilon, beta_epsilon))
  
  lambdas = tibble(lambda = grid_labmda, 
                   lp_lambda = score_lambda(grid_lambda, alpha_lambda, beta_lambda))
  
  lp_thetas_epsilon_lambda = expand_grid(thetas, epsilons, lambdas)
  return(lp_thetas_epsilon_lambda) 
}

# ---------------- rectified_luce_choice ----------------
# this crazy function is necessary because if the values get too close to 0, 
# this can be > 1 or < 0
rectified_luce_choice <- function(x, y) {
  max(min(x / (x + y), 1), 0)
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

# ---------------- score_lambda ---------------------
score_lambda <- function(lambda, alpha_lambda, beta_lambda){
  dbeta(x = lambda, shape1 = alpha_lambda, shape2 = beta_lambda, log = TRUE)
}

# ---------------- compute KL divergence ---------------------
# throws error if one but not both distributions contain 0's at a certain spot
kl_div <- function (x, y) {
      sum(x * log(x/y))
}

kl_div_log <- function(log_x, log_y){
  
  sum(exp(log_x + as.brob(log(log_x - log_y))))
  
}

# ---------------- get_post_pred ---------------------
# get posterior predictive
get_post_pred <- function(lp_post, heads = TRUE) {
  
  p_1 = exp(matrixStats::logSumExp( log(1 - lp_post$epsilon) + log(lp_post$theta) + log(lp_post$posterior))) + 
    +     exp(matrixStats::logSumExp((log(lp_post$epsilon) + log(1-lp_post$theta) + log(lp_post$posterior))))

  
  # Currently we believe that the above version is equivalent to the non-log version below (since we're not running into actual underflow risk here):
# p_1 = sum(((1 - lp_post$epsilon) * lp_post$theta * lp_post$posterior) + (lp_post$epsilon * (1-lp_post$theta) * lp_post$posterior)) 
  

  ifelse(heads, p_1, 1 - p_1)
}

