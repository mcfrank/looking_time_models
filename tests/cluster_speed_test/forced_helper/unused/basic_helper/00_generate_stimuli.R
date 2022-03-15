
# generate stimuli
generate_stimuli_vector <- function(total_feature_n, stimuli_feature_n){
  
  vec <- c(rep(0, total_feature_n - stimuli_feature_n), rep(1, stimuli_feature_n))
  return(sample(vec))
  
}

# generate similar / dissimilar stimuli
# similar ratio is the proportion of different features for similar stimuli
# dissimilar ratio is the proportion of different features for dissimilar stimuli
generate_stimuli_similarity <- function(original_stimuli, similarity, 
                                        similar_ratio, 
                                        dissimilar_ratio){
  
  non_overlapping_feature <- ifelse(similarity == "similar", similar_ratio, dissimilar_ratio)
  
  # first figure out where the 1s are at 
  feature_pos <- which(original_stimuli %in% c(1))
  non_feature_pos <- which(original_stimuli %in% c(0))
  
  # change 1 to 0
  feature_change_pos <- sample(feature_pos, 
                               non_overlapping_feature * length(feature_pos), 
                               replace = FALSE)
  
  new_stim <- replace(original_stimuli, feature_change_pos, 0)
  
  # change 0 to 1 
  non_feature_change_pos <- sample(non_feature_pos, 
                                   non_overlapping_feature * length(feature_pos), 
                                   replace = FALSE)
  
  new_stim <- replace(new_stim, non_feature_change_pos, 1)
  
  
  return (new_stim)  
  
}

## create a stimuli sequence 
generate_block_sequence <- function(total_feature_n, 
                                    simple_feature_n, 
                                    complex_feature_n,
                                    similar_ratio, 
                                    dissimilar_ratio, 
                                    block_length, 
                                    deviant_pos, 
                                    complexity, 
                                    similarity){
  
  TOTAL_FEATURE_N = total_feature_n  
  # the number of 1 in the feature vector 
  SIMPLE_FEATURE_N = simple_feature_n
  COMPLEX_FEATURE_N = complex_feature_n  
  
  feature_n <- ifelse(complexity == "complex", COMPLEX_FEATURE_N, SIMPLE_FEATURE_N)
  background_stim <- generate_stimuli_vector(TOTAL_FEATURE_N, feature_n)
  deviant_stim <- generate_stimuli_similarity(background_stim, 
                                              similarity, 
                                              similar_ratio, 
                                              dissimilar_ratio)
  
  block_list <- replicate(block_length, background_stim, simplify = FALSE)
  
  block_list[deviant_pos] <- replicate(length(deviant_pos), 
                                       deviant_stim, 
                                       simplify = FALSE)
  
  return(block_list)
  
}


get_block_sequence <- function(
  complexity, 
  similarity, 
  total_feature_n, 
  simple_feature_n, 
  complex_feature_n, 
  similar_ratio, 
  dissimilar_ratio, 
  block_length, 
  deviant_pos = c(3, 5)){
  
  TOTAL_FEATURE_N = total_feature_n  
  # the number of 1 in the feature vector 
  SIMPLE_FEATURE_N = simple_feature_n
  COMPLEX_FEATURE_N = complex_feature_n  
  
  
  feature_n <- ifelse(complexity == "complex", COMPLEX_FEATURE_N, SIMPLE_FEATURE_N)
  background_stim <- generate_stimuli_vector(TOTAL_FEATURE_N, feature_n)
  deviant_stim <- generate_stimuli_similarity(background_stim, 
                                              similarity, 
                                              similar_ratio, 
                                              dissimilar_ratio)
  
  block_list <- replicate(block_length, background_stim, simplify = FALSE)
  
  if (length(deviant_pos) > 0){
    block_list[deviant_pos] <- replicate(length(deviant_pos),
                                         deviant_stim,
                                         simplify = FALSE)
  }
  return(block_list)
  
}

make_observation <- function(stimuli, feature_sample){
  
  #figure out the feature position  
  feature_pos <- which(stimuli %in% c(1))
  sampled_feature <- sample(feature_pos, 
                            feature_sample, 
                            replace = FALSE)
  observation <- rep(0, length(stimuli))
  observation[sampled_feature] <- 1
  
  return(observation)
  
}


generate_sequence_with_parameter <- function(d,
                                             total_feature_n, 
                                             complex_feature_n, 
                                             simple_feature_n, 
                                             similar_ratio, 
                                             dissimilar_ratio, 
                                             prior){
  
  d_exp_sim <- d %>% 
    # the block length doesn't match 
    mutate(
      
      sequence = pmap(d_experiment_parameter %>% select(-c(subject, block_number)), .f = ~with(list(...), 
                                                                                               get_block_sequence(complexity, similarity, 
                                                                                                                  total_feature_n, 
                                                                                                                  simple_feature_n, 
                                                                                                                  complex_feature_n, 
                                                                                                                  similar_ratio, 
                                                                                                                  dissimilar_ratio, 
                                                                                                                  block_length, 
                                                                                                                  dev_positions)))
    ) #%>% 
  # unnest(sequence) %>% 
  # group_by(subject, block_number) %>% 
  #mutate(trial_number = row_number())
  
  # this should be added later so less computation load 
  
  # d_rt <- d_rt %>% 
  # select(subject, block_number, trial_number, rt, item_type, trial_type, trial_complexity) %>% 
  # mutate(rt = rt + 500) %>%  # add the baseline back 
  # mutate(temp_id = paste(subject, block_number, trial_number)) %>% 
  # #rename(real_trial_number = trial_number) %>% 
  # select(temp_id, rt, item_type, trial_type, trial_complexity)
  # 
  # d_exp_sim <- d_exp_sim %>% 
  #  mutate(temp_id = paste(subject, block_number, trial_number)) %>% 
  #   left_join(d_rt, by = "temp_id") 
  
  
  
  return(d_exp_sim)
  
  
  
}

