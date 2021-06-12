

generate_creature_sequence <- function(
  block_length, 
  deviant_positions, # takes a vector, 
  total_feature, 
  feature_theta, 
  feature_number, 
  dissimilar_ratio
){
  
  
  background_theta <- make_creature(total_feature = total_feature, 
                                    feature_theta = feature_theta, 
                                    feature_number = feature_number)
  
  
  deviant_theta <- make_dissimilar_creature(creature = background_theta, 
                                            dissimilar_ratio = dissimilar_ratio)
  
  
  background <- make_individual_creature(background_theta)
  deviant <- make_individual_creature(deviant_theta)
  
  block_list <- replicate(block_length, background, simplify = FALSE)
  
  if (length(deviant_positions) > 0){
    block_list[deviant_positions] <- replicate(length(deviant_positions),
                                               deviant,
                                               simplify = FALSE)
  }
  
  # putting everything in a tibble dataframe  
  tidy_creature_sequence <- bind_rows(lapply(block_list,
                                             function(x) x %>% as_tibble_row(.name_repair = make.names))) 
  
  # renaming columns 
  rename_column <- function(x){paste0("V", x)}
  tidy_column_names <- lapply(1:total_feature, rename_column)
  colnames(tidy_creature_sequence) <- tidy_column_names
  
  tidy_creature_sequence <- tidy_creature_sequence %>% 
    mutate(trial_number = row_number(), 
           trial_type = case_when(
             trial_number %in% deviant_positions ~ "deviant", 
             TRUE ~ "background"
           ))
  
  
  return(tidy_creature_sequence)
  
}


make_creature <- function(
  total_feature,
  # currently assuming all situations where there are features the theta are the same 
  feature_theta, 
  # complexity controls for the proportion of the features 
  feature_number
){
  
  sample(c(rep(feature_theta, feature_number), 
           rep(1-feature_theta, total_feature -feature_number)))
  
  
}

make_dissimilar_creature <- function(
  creature, 
  dissimilar_ratio # proportion of features flipped 
){
  
  
  # first figure out where the feature theta are at 
  feature_pos <- which(creature > 0.5)
  non_feature_pos <- which(creature  < 0.5 | creature == 0.5)
  total_feature_n <- length(feature_pos)
  
  feature_flip <- floor(total_feature_n * dissimilar_ratio)
  
  # change feature to non-feature
  feature_change_pos <- sample(feature_pos, 
                               feature_flip, 
                               replace = FALSE)
  
  creature[feature_change_pos] <- 1 - creature[feature_change_pos]
  
  # change non-feature to feature
  non_feature_change_pos <- sample(non_feature_pos, 
                                   feature_flip, 
                                   replace = FALSE)
  
  creature[non_feature_change_pos] <- 1 - creature[non_feature_change_pos]  
  
  dissimilar_stimuli = creature
  
  return(dissimilar_stimuli)
}

make_individual_creature <- function(creature_theta){
  sapply(creature_theta, function(x){rbernoulli(p = x, n = 1)})
}