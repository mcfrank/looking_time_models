
noisy_observation_feature <- function(
  feature, 
  n_sample, 
  epsilon 
){
  real_features <- rep(feature, n_sample)
  noisy <- rbernoulli(p = epsilon, n = n_sample)
  return(ifelse(noisy, as.logical(1-real_features), real_features))
  
}


noisy_observation_creature <- function(
  creature, 
  n_sample = 1, 
  epsilon
){
  sapply(creature, function(y){noisy_observation_feature(
    feature = y, 
    n_sample = n_sample, 
    epsilon = epsilon
  )})
  
}
