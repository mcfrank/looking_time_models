
noisy_observation_feature <- function(
  feature, 
  n_sample, 
  epsilon 
){
  real_features <- rep(feature, n_sample)
  noisy <- rbernoulli(p = epsilon, n = n_sample)
  return(ifelse(noisy, as.logical(1-real_features), real_features))
  
}

# noisy_observation_creature takes a stimulus dataframe
# take stimulus dataframe and change the particular columns of the 
# selected row according to epsilon
# returns tibble with Vn columns 

noisy_observation_creature <- function(
  stimuli_df,
  trial_index, 
  n_sample = 1, 
  epsilon
) {
  
  creature <- simple_stimuli[simple_stimuli$trial_number == trial_index, 
                             str_detect(names(simple_stimuli), "V")]  
  # creature <- stimuli_df %>% 
  #   filter(trial_number == trial_index) %>% 
  #   select_at(vars(starts_with("V"))) 
    
  observations <- sapply(creature, function(y) {noisy_observation_feature(
    feature = y, 
    n_sample = n_sample, 
    epsilon = epsilon
  ) %>%  
    as_tibble_row(.name_repair =  make.names)
    
    }) 
  
  rename_column <- function(x){paste0("V", x)}
  tidy_column_names <- lapply(1:ncol(creature), rename_column)
  names(observations) <- tidy_column_names
  
  observations <- observations %>% as_tibble()  %>% unnest(cols = starts_with("V"))
  
  return(observations)
}
