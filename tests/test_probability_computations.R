library(assertthat)
### SET UP TESTING ENVIRONMENT

# initialize variables





### TEST score_lp_z_given_theta

## friendly case: just a numerical value 
# create fake observations, one feature, T T F where we know that stimulus_idx = [1 2 2]

for o in observations
  for f in features
    score_z_given_theta(
    
      
assert_that(lp_z_given_theta[[3]][[1]]$lp_z_given_theta[5] == .799)

## unfriendly cases
# what about when epsilon = 0


# what about when theta = 0

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
# this function should return the log probability of epsilon for a given betat distribution 
# test output value returned using: https://docs.scipy.org/doc/scipy/reference/generated/scipy.stats.beta.html
    
    assert_that(score_epsilon(1, 1, 1) == 0)
    assert_that(round(score_epsilon(.5, 5, 10), 3) == .200)
    assert_that(score_epsilon(10, 1, 1) == -Inf)
    assert_that(score_epsilon(-1, 1, 1) == -Inf)
    
    
