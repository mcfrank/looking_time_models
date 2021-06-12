
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
  stimuli_df,
  trial_index, 
  n_sample = 1, 
  epsilon
){
  
  creature <- stimuli_df %>% 
    filter(trial_number == trial_index) %>% 
    select_at(vars(starts_with("V"))) 
    
    
  sapply(creature, function(y){noisy_observation_feature(
    feature = y, 
    n_sample = n_sample, 
    epsilon = epsilon
  )}) %>% 
    as_tibble_row()
  
  
}
