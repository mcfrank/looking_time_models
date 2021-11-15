library(assertthat)
library(here)

source(here("helper/probability_computations.R"))
source(here("helper/initialization.R"))
source(here("helper/make_scheme_and_params.R"))


### SET UP TESTING ENVIRONMENT

# initialize variables

# what about when theta = 0


### TEST score_theta
# score_theta <- function(theta, alpha_theta, beta_theta){}
# this function should return the log probability of theta for a given beta distribution
# test output value returned using: https://docs.scipy.org/doc/scipy/reference/generated/scipy.stats.beta.html

assert_that(score_theta(1, 1, 1) == 0)
assert_that(round(score_theta(.3, 3, 2), 3) == -.280)
assert_that(score_theta(10, 1, 1) == -Inf)
assert_that(score_theta(-1, 1, 1) == -Inf)


# TEST score_epsilon 
# score_epsilon <- function(epsilon, alpha_epsilon, beta_epsilon){}
# this function should return the log probability of epsilon for a given beta distribution 
# test output value returned using: https://docs.scipy.org/doc/scipy/reference/generated/scipy.stats.beta.html

assert_that(score_epsilon(1, 1, 1) == 0)
assert_that(round(score_epsilon(.5, 5, 10), 3) == .200)
assert_that(score_epsilon(10, 1, 1) == -Inf)
assert_that(score_epsilon(-1, 1, 1) == -Inf)


### TEST score_z_ij_given_y
# score_z_ij_given_y <- function(zij, yi, epsilon){}
# this function return the log probability of perceiving or misperceiving the feature; 
# the probability of misperceiving it is epsilon; the probability of perceiving it correctly is (1-epsilon)

assert_that(round(score_z_ij_given_y(1, 1, .6), 3) == -.916)
assert_that(round(score_z_ij_given_y(1, 0, .7), 3) == -.357)
#assert_that(round(score_z_ij_given_y(2, 2, .8))) # but this shouldn't give us anything back?? 
assert_that((score_z_ij_given_y(1, 1, 1) == -Inf))
assert_that((score_z_ij_given_y(1, 0, 1) == 0))

### TEST score_yi_given_theta
# score_yi_given_theta <- function(yi, theta){}
# this function should return the log probability of seeing yi = 1 if the probability of seeing yi = 1 is theta, and yi = 0 if the probability is (1-theta). 
# test output value returned using: function: spstats.bernoulli.logpmf

assert_that(round(score_yi_given_theta(1, .8), 3) == -.223)
assert_that(round(score_yi_given_theta(0, .8), 3) == -1.609)
assert_that(score_yi_given_theta(.5, .8) == -Inf)
assert_that(is.nan(score_yi_given_theta(1, 10)))


### TEST score_z_given_theta

# CASE 1: SINGLE OBSERVATION ON ONE STIMULUS 
t = 1
f = 1 
stimulus_idx <- 1
theta =  c(0.1, 0.4,.75, .9)
epsilon = c(0.1, 0.4,.75, .9)
lp_y_given_theta = tibble(
  theta = theta, 
  lp_y_ONE_given_theta = score_yi_given_theta(yi = 1, theta = theta), 
  lp_y_ZERO_given_theta = score_yi_given_theta(yi = 0, theta = theta))

lp_z_given_theta = initialize_z_given_theta(grid_theta = theta, 
                                            grid_epsilon = epsilon, 
                                            max_observation = 3, 
                                            feature_number = 3)

model <- initialize_model(eig_from_world = 0.001, 
                          max_observation = 3, 
                          n_features = 3)
model$t[t] = t
model$stimulus_idx[t] = stimulus_idx
model$f1[t] = TRUE

# hand calculate example: 
# theta = .4, epsilon = .4, observation is TRUE 

test_single_z_given_theta <- function(observation, theta_value, epsilon_value){
  

  ##calculating a single value with the tested function 
  lp_z_given_y_ONE = score_z_ij_given_y(observation, 1, epsilon = epsilon_value) # already tested function 
  lp_z_given_y_ZERO = score_z_ij_given_y(observation, 0, epsilon = epsilon_value)# already tested function 
  lp_y_ZERO_given_theta = score_yi_given_theta(yi = 0, theta = theta_value) #already tested function
  lp_y_ONE_given_theta = score_yi_given_theta(yi = 1, theta = theta_value)#already tested function
  
  value <-log(exp(lp_z_given_y_ONE + lp_y_ONE_given_theta) + exp(lp_z_given_y_ZERO + lp_y_ZERO_given_theta)) 
  return(value)
  
}

assert_that(round(score_z_given_theta(t, f, lp_y_given_theta, lp_z_given_theta, model) %>% 
  filter(theta == .4, epsilon == .4) %>% 
  pull(lp_z_given_theta), 3) == round(test_single_z_given_theta(TRUE, .4, .4), 3))

assert_that(round(score_z_given_theta(t, f, lp_y_given_theta, lp_z_given_theta, model) %>% 
                    filter(theta == .75, epsilon == .75) %>% 
                    pull(lp_z_given_theta), 3) == round(test_single_z_given_theta(TRUE, .75, .75), 3))




#CASE 2: MULTIPLE OBSERVATION ON ONE STIMULUS 
# two observation, TRUE TRUE 
t = 2
f = 1 
stimulus_idx <- 1
theta =  c(0.1, 0.4,.75, .9)
epsilon = c(0.1, 0.4,.75, .9)
lp_y_given_theta = tibble(
  theta = theta, 
  lp_y_ONE_given_theta = score_yi_given_theta(yi = 1, theta = theta), 
  lp_y_ZERO_given_theta = score_yi_given_theta(yi = 0, theta = theta))

lp_z_given_theta = initialize_z_given_theta(grid_theta = theta, 
                                            grid_epsilon = epsilon, 
                                            max_observation = 3, 
                                            feature_number = 3)

model <- initialize_model(eig_from_world = 0.001, 
                          max_observation = 3, 
                          n_features = 3)


test_multiple_obs_z_given_theta <- function(observations, theta_value, epsilon_value){
  
  
  lp_z_given_y_ONE = 0
  lp_z_given_y_ZERO = 0
  for(i in 1:length(observations)){
    lp_z_given_y_ONE = lp_z_given_y_ONE + score_z_ij_given_y(observations[[i]], 1, epsilon = epsilon_value)  
    lp_z_given_y_ZERO = lp_z_given_y_ZERO + score_z_ij_given_y(observations[[i]], 0, epsilon = epsilon_value)
  }
  lp_y_ZERO_given_theta = score_yi_given_theta(yi = 0, theta = theta_value) #already tested function
  lp_y_ONE_given_theta = score_yi_given_theta(yi = 1, theta = theta_value)#already tested function
  
  value <-log(exp(lp_z_given_y_ONE + lp_y_ONE_given_theta) + exp(lp_z_given_y_ZERO + lp_y_ZERO_given_theta)) 
  return(value)
}

model$t[1:t] = seq(1, t)
model$stimulus_idx[1:t] = stimulus_idx
model$f1[1:t] = c(TRUE, TRUE) 

assert_that(round(score_z_given_theta(2, 1, lp_y_given_theta, # cached likelihoods
                    lp_z_given_theta, # likelihoods
                    model) %>% 
              filter(theta == .4, epsilon == .4) %>% 
              pull(lp_z_given_theta), 3) == round(test_multiple_obs_z_given_theta(c(TRUE, TRUE), .75, .4), 3))


model$f1[1:t] = c(TRUE, FALSE) 
assert_that(round(score_z_given_theta(2, 1, lp_y_given_theta, # cached likelihoods
                                      lp_z_given_theta, # likelihoods
                                      model) %>% 
                    filter(theta == .75, epsilon == .4) %>% 
                    pull(lp_z_given_theta), 3) == round(test_multiple_obs_z_given_theta(c(TRUE, FALSE), .75, .4), 3))



# CASE 3: Observing new stimulus
test_obs_on_new_stimuli_z_given_theta <- function(observations, theta_value, epsilon_value, previous_val){
  
  
  lp_z_given_y_ONE = 0
  lp_z_given_y_ZERO = 0
  for(i in 1:length(observations)){
    lp_z_given_y_ONE = lp_z_given_y_ONE + score_z_ij_given_y(observations[[i]], 1, epsilon = epsilon_value)  
    lp_z_given_y_ZERO = lp_z_given_y_ZERO + score_z_ij_given_y(observations[[i]], 0, epsilon = epsilon_value)
  }
  lp_y_ZERO_given_theta = score_yi_given_theta(yi = 0, theta = theta_value) #already tested function
  lp_y_ONE_given_theta = score_yi_given_theta(yi = 1, theta = theta_value)#already tested function
  
  current_stim_value <- log(exp(lp_z_given_y_ONE + lp_y_ONE_given_theta) + exp(lp_z_given_y_ZERO + lp_y_ZERO_given_theta)) 
  prev_stim_value <- previous_val
  
  value <- prev_stim_value + current_stim_value
  
  return(value)
}


t = 2
f = 1 
stimulus_idx <- 1
theta =  c(0.1, 0.4,.75, .9)
epsilon = c(0.1, 0.4,.75, .9)
lp_y_given_theta = tibble(
  theta = theta, 
  lp_y_ONE_given_theta = score_yi_given_theta(yi = 1, theta = theta), 
  lp_y_ZERO_given_theta = score_yi_given_theta(yi = 0, theta = theta))

lp_z_given_theta = initialize_z_given_theta(grid_theta = theta, 
                                            grid_epsilon = epsilon, 
                                            max_observation = 3, 
                                            feature_number = 3)

model <- initialize_model(eig_from_world = 0.001, 
                          max_observation = 3, 
                          n_features = 3)


model$t[1] = 1
model$stimulus_idx[1] = 1
model$f1[1] = TRUE

lp_z_given_theta[[1]][[1]] <- score_z_given_theta(1, 1, lp_y_given_theta, 
                    lp_z_given_theta, 
                    model)

model$t[2] = 2
model$stimulus_idx[2] = 2
model$f1[2] = TRUE

score_z_given_theta(2, 1, lp_y_given_theta, lp_z_given_theta, model)
test_obs_on_new_stimuli_z_given_theta(c(TRUE), 0.9, 0.1, 
                                      previous_val = test_multiple_obs_z_given_theta(c(TRUE), 0.9, 0.1))

assert_that(
  round(score_z_given_theta(2, 1, lp_y_given_theta, lp_z_given_theta, model) %>% 
    filter(theta == .9, epsilon == .1) %>% pull(lp_z_given_theta), 3) == 
  round(test_obs_on_new_stimuli_z_given_theta(c(TRUE), 0.9, 0.1, 
                                              previous_val = test_multiple_obs_z_given_theta(c(TRUE), 0.9, 0.1)), 3)
)


model$t[3] = 3
model$stimulus_idx[3] = 2
model$f1[3] = FALSE

lp_z_given_theta[[2]][[1]] <- score_z_given_theta(2, 1, lp_y_given_theta, 
                                                  lp_z_given_theta, 
                                                  model)

assert_that(
  round(score_z_given_theta(3, 1, lp_y_given_theta, lp_z_given_theta, model) %>% 
          filter(theta == .75, epsilon == .4) %>% pull(lp_z_given_theta), 3) == 
    round(test_obs_on_new_stimuli_z_given_theta(c(TRUE, FALSE), .75, .4, 
                                                previous_val = test_multiple_obs_z_given_theta(c(TRUE), .75, .4)), 3)
)


## TEST kl_div
# this function should return the KL divergence between two distributions.
# current version does not check for the validit of the input; the testing version causes weird output 
# checking against a built in function of KL from package philentropy: https://cran.r-project.org/web/packages/philentropy/vignettes/Information_Theory.html

p <- seq(1, 10, 1) / sum(seq(1, 10, 1)); q <- seq(2, 11, 1) / sum(seq(2, 11, 1))
assert_that(kl_div(p, q) == philentropy::KL(rbind(p, q), unit = "log")) 


set.seed(123); p <- rnorm(100, 0, 1); set.seed(123); q <- rnorm(100, 0, 1)
assert_that(kl_div(p,q) == 0) # should be 0 for two vectors that are the same


## TEST get_post_pred
## calculates posterior predictive from posterior
## uses dataframe with 3 inputs: lp_post$epsilon; lp_post$theta; lp_post$posterior
## We are assuming that the input to this function will be:
# - the posterior sums to 1
# - theta and epsilon are never exactly 0 (due to log(0) = -Inf issues) <- might need to revisit to a


# Test 1: if P(theta = 0.5) = 1, posterior predictive should give a 50% chance of success
lp_post = tibble(epsilon = c(0),
                 theta = c(0.5),
                 posterior = c(1))

assert_that(get_post_pred(lp_post, heads = TRUE) == 0.5)


# Test 2: grid for two vals of theta and epsilon
lp_post = tibble(epsilon = rep(c(0.33, 0.66), 2),
                 theta = rep(c(0.33, 0.66), each=2),
                 posterior = c(0.3, 0.2, 0.1, 0.4))

# by hand: (1-0.33) * 0.33 * 0.3 + 0.33 * (1-0.33) * 0.3 + (1-0.66) * 0.33 * 0.2 + 0.66 * (1-0.33) * 0.2 + (1-0.66) * 0.33 * 0.1 + 0.66 * (1-0.33) * 0.1 + (1-0.66) * 0.66 * 0.4 + 0.66 * (1-0.66) * 0.4
# = 0.4785

# round to 4 decimal places
assert_that(round(get_post_pred(lp_post, heads = TRUE),4) == 0.4785)


## TEST score_post
# this function gets three inputs:
# 1) the likelihood p(z|theta) [lp_z_given_theta], 
# 2) the priors p(theta) and p(epsilon) [lp_prior]
# 3) a preallocated df to put in posterior [lp_post]
lp_z_given_theta = tibble(lp_z_given_theta = c(1))
lp_prior = tibble(lp_theta = c(1), lp_epsilon = c(1))

lp_post = initialize_posterior(grid_theta = c(0.5), grid_epsilon = c(0), 
                               max_observation = 1, feature_number = 1)

# Test 1: single length prior & likelihood & single observation/feature for each parameter for each parameter
test_df <- score_post(lp_z_given_theta, lp_prior, lp_post[[1]][[1]])

expected_df <- lp_post[[1]][[1]] %>% mutate(unnormalized_log_posterior = 3, # 1+1+1 (see lp_z_given_theta & lp_prior)
                                  log_posterior = 0,
                                  posterior = 1)

assert_that(all.equal(test_df,expected_df))

# Test 2: test bigger df
lp_z_given_theta = tibble(theta = c(0.33, 0.33, 0.66, 0.66), 
                          epsilon = c(0, 0.1, 0, 0.1),
                          lp_z_y_ONE = NA,
                          lp_z_y_ZERO = NA,
                          lp_z_given_theta = c(1, 1, 1, 1))
lp_prior = tibble(theta = c(0.33, 0.33, 0.66, 0.66), 
                  epsilon = c(0, 0.1, 0, 0.1), 
                  lp_theta = c(3,2,1,0), 
                  lp_epsilon = c(0,1,2,3))
lp_post = tibble(theta = c(0.33, 0.33, 0.66, 0.66), 
                 epsilon = c(0, 0.1, 0, 0.1), 
                 unnormalized_log_posterior = NA, 
                 log_posterior = NA, 
                 posterior = NA)

test_df <- score_post(lp_z_given_theta, lp_prior, lp_post)


expected_df <- tibble(theta = c(0.33, 0.33, 0.66, 0.66), 
                      epsilon = c(0, 0.1, 0, 0.1), 
                      unnormalized_log_posterior = c(4,4,4,4), # this is lp_z_given_theta + lp_theta + lp_epsilon
                      log_posterior = rep(-1.39, 4), # log(0.25) because 0.25 is the posterior density for a uniform prior
                      posterior = rep(0.25,4)) #uniform posterior


assert_that(all.equal(test_df,expected_df))
