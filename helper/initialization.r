# ALL FUNCTIONS DEALING WITH INITIALIZING MODEL COMPUTATION

initialize_posterior <- function(grid_theta, grid_epsilon, max_observation, feature_number){
  posterior <- expand_grid(theta = grid_theta,
                              epsilon = grid_epsilon)
  # not sure when do we really need the non-log one, save some $$$  
  posterior$unnormalized_log_posterior <- NA_real_
  posterior$log_posterior <- NA_real_
  posterior$posterior <- NA_real_
  
  posterior <- lapply(seq(1, max_observation, 1), 
                            function(x){
                              lapply(seq(1, feature_number, 1), 
                                     function(y){
                                       posterior
                                     })
                            })
  return(posterior)
}

initialize_z_given_theta <- function(grid_theta, grid_epsilon, max_observation, feature_number){
  z_given_theta <- expand_grid(theta = grid_theta, 
                                  epsilon = grid_epsilon)
  z_given_theta$lp_z_y_ONE <- NA_real_
  z_given_theta$lp_z_y_ZERO <-  NA_real_
  z_given_theta$lp_z_given_theta <- NA_real_

  z_given_theta <- lapply(seq(1, max_observation, 1), 
                                function(x){
                                  lapply(seq(1, feature_number, 1), 
                                         function(y){
                                           z_given_theta
                                         })
                                })
  return(z_given_theta)
  
}

# model data frame, includes observations
initialize_model <- function(eig_from_world, max_observation, n_features) {
  
    model <- tibble(t = rep(NA,max_observation),
                    stimulus_idx = rep(NA,max_observation), 
                    EIG = rep(NA,max_observation), 
                    EIG_from_world = rep(eig_from_world,max_observation),
                    p_look_away = rep(NA,max_observation), 
                    look_away = rep(NA,max_observation)) 
  
  # initialize columns for observations
  model[,paste("f", 1:n_features, sep="")] <- NA
  
  return(model)
}

# data f for keep track of actual observations 
initialize_m_observation <- function(feature_number, max_observation, stimuli_sequence){
  
  m_observation <- matrix(ncol = feature_number, 
                          nrow = max_observation)
  colnames(m_observation) <- grep("V",names(stimuli_sequence), value = TRUE)
  return(m_observation)
}


# helper function that gets you a df with features as columns and all possible
# combinations
get_possible_observations <- function(n_features) {
  feature_list <- list()

  for (i in 1:n_features) {
    feature_list[[i]] <- c(FALSE, TRUE)
  }

  expand.grid(feature_list) %>%
    tibble() %>%
    rename_with(~gsub("Var", "V", .x)) # fix random naming thing
}



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

