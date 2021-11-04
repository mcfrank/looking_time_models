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




### TEST score_lp_z_given_theta

