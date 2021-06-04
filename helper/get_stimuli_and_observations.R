




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
  
  
  return(block_list)
  
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


generate_noisy_observations <- function(block, 
                                        exposure_type, 
                                        short_exposure_samps, 
                                        long_exposure_samps, 
                                        normal_exposure_samps, 
                                        epsilon
){
  
  if(exposure_type == "forced_short"){
    first_trial_samps = short_exposure_samps
  }else if (exposure_type == "forced_long"){
    first_trial_samps = long_exposure_samps
  }else{
    first_trial_samps = normal_exposure_samps
  }
  
  first_trial_observation <- noisy_observation_creature(block[[1]], 
                                                        first_trial_samps, 
                                                        epsilon)
  
  remaining_trials <- lapply(block[2:length(block)], 
                             function(creature){
                               noisy_observation_creature(
                                 creature, 
                                 n_sample = normal_exposure_samps, 
                                 epsilon = epsilon 
                               )
                             })
  
  all_observations <- vector("list", length(block))
  all_observations[[1]] <- first_trial_observation
  all_observations[2:length(all_observations)] <- remaining_trials
  
  # this is to make sure we can keep track of the trial number and the number of observations for each trial 
  obs <-  as.data.frame(do.call(rbind, all_observations))
  
  
  obs_length = as.vector(lapply(all_observations, function(x){
    length(x)
  }))
  creature_feature_n <- ncol(obs)
  obs_num <- unlist(obs_length) / creature_feature_n
  trial_observation_num <- lapply(obs_num, 
                                  function(x){
                                    rep(x, x)
                                  }) %>% unlist()
  
  trial_num <- rep(seq(obs_num), obs_num)
  
  obs$trial_num <- rep(seq(obs_num), obs_num)
  # obs$trial_observation_num <- trial_observation_num
  
  obs %<>% 
    group_by(trial_num) %>%
    mutate(observation_num = 1:n())
  
  return(obs)
  
}

noisy_observation_feature <- function(
  feature, 
  n_sample, 
  epsilon 
){
  real_features <- rep(feature, n_sample)
  noisy <- rbernoulli(p = epsilon, n = n_sample)
  return(ifelse(noisy, 1-real_features, real_features))
  
}


noisy_observation_creature <- function(
  creature, 
  n_sample, 
  epsilon
){
  sapply(creature, function(y){noisy_observation_feature(
    feature = y, 
    n_sample = n_sample, 
    epsilon = epsilon
  )})
  
}