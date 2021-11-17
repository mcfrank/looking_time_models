make_simulation_params <- function(n,
                                    sequence_scheme, 
                                   complexity,
                                  alpha_priors, 
                                   beta_priors, 
                                   alpha_epsilon, 
                                   beta_epsilon, 
                                   noise_parameter, 
                                   world_EIG, 
                                   max_observation, 
                                   fixed_length_complexity, 
                                   feature_space_n, 
                                   simple_feature_n, 
                                   complex_feature_n, 
                                   dissimilar_ratio,
                                   forced_exposure_params
                                  ){
  
  params_id_df <- crossing(
    alpha_prior = alpha_priors,
    beta_prior = beta_priors, 
    alpha_epsilon = alpha_epsilon, 
    beta_epsilon = beta_epsilon,
    noise_parameter = noise_parameters,
    world_EIG = world_EIG, 
    max_observation = max_observation
  ) %>% 
    mutate(params_id = row_number(), 
    )
  
  
  stimulus_sequence_df <- tibble(sequence_scheme = sequence_scheme) %>% 
                          crossing(tibble(complexity = complexity)) %>% 
                          mutate(n = 1) %>% 
                          rowwise() %>% 
                          mutate(stimuli_sequence = 
                                   nest(
                                     scheme_to_stimuli(sequence_scheme, 
                                                       fixed_length_complexity = fixed_length_complexity, 
                                                        complexity = complexity, 
                                                        feature_space_n = feature_space_n, 
                                                        simple_feature_n = simple_feature_n, 
                                                        complex_feature_n = complex_feature_n, 
                                                        dissimilar_ratio = dissimilar_ratio), 
                                                        data = everything()
                                        )
                                  ) %>% 
                        ungroup() %>% 
                        mutate(sim_id = row_number()) 
  
max_observations_per_block <- get_max_observations(sequence_scheme, 
                                              forced_exposure_params,
                                              max_observations)
  

  stimulus_sequence_df %>% mutate(max_observations_per_block = max_observations_per_block) %>%
    crossing(params_id_df)
  
  
  
}



scheme_to_stimuli <- function(sequence_scheme, 
                              fixed_length_complexity = TRUE,
                              complexity, 
                              #similarity?, 
                              feature_space_n, 
                              simple_feature_n, complex_feature_n, 
                              dissimilar_ratio
){
  
  if (fixed_length_complexity){
    background_creature <- sample(c(rep(FALSE, 
                                        feature_space_n - ifelse(complexity == "complex", complex_feature_n, simple_feature_n)),    
                                    rep(TRUE, ifelse(complexity == "complex", complex_feature_n, simple_feature_n))))
    
    # make deviant creature
    # first figure out which location has TRUE
    deviant_creature <- background_creature
    
    feature_pos <- which(background_creature == TRUE)
    non_feature_pos <- which(background_creature == FALSE)
    total_feature_n <- length(feature_pos)
    feature_flip <- ceiling(total_feature_n * dissimilar_ratio)
    feature_change_pos <- sample(feature_pos, 
                                 feature_flip, 
                                 replace = FALSE)
    deviant_creature[feature_change_pos] <- !deviant_creature[feature_change_pos]
    
    # change non-feature to feature
    non_feature_change_pos <- sample(non_feature_pos, 
                                     ifelse(feature_flip > length(non_feature_pos), length(non_feature_pos), feature_flip), 
                                     replace = FALSE)
    
    deviant_creature[non_feature_change_pos] <-!deviant_creature[non_feature_change_pos]
    
    dissimilar_stimuli = deviant_creature
    
    
  }else{
    
    background_creature <- sample(c(rep(TRUE, ifelse(complexity == "complex", 
                                                     complex_feature_n, 
                                                     simple_feature_n))))
    # how to represent dissimilarity with ratio? currently just flipping 
    # FIXME: 
    deviant_creature <- as.logical(1-background_creature)
    dissimilar_stimuli = deviant_creature
    
  }
  
  
  block_list <- strsplit(sequence_scheme, "")[[1]]
  
  background_sequence <- replicate(length(block_list), background_creature, simplify = FALSE)
  background_sequence[which(block_list == "D")] <- replicate(length(which(block_list == "D")), 
                                                             dissimilar_stimuli, simplify = FALSE)
  
  tidy_creature_sequence <- bind_rows(lapply(background_sequence,
                                             function(x) x %>% as_tibble_row(.name_repair = make.names))) 
  
  # renaming columns 
  rename_column <- function(x){paste0("V", x)}
  tidy_column_names <- lapply(1:length(background_creature), rename_column)
  colnames(tidy_creature_sequence) <- tidy_column_names
  tidy_creature_sequence <- tidy_creature_sequence %>% 
    mutate(trial_number = row_number())
  
  return(tidy_creature_sequence)
  
  
}

get_max_observations <- function(sequence_scheme, forced_exposure_params, max_observations) {

  # the length of each block
  block_lengths <- nchar(sequence_scheme)
  
    max_observations <- block_lengths %>% map(function(x) rep(max_observation, x))
  
    # modify this to put cosntraints for forced_exposure
    if (forced_exposure_params$do_forced_exposure) {
      
      if(forced_exposure_params$forced_exposure_trials == 'first') {
        max_observations <- max_observations %>% lapply(function(x) {x[1] <- forced_exposure_params$max_sample_num 
                                                        + x})
      }
      
      else if (forced_exposure_params$forced_exposure_trials == 'allbutlast') {
        max_observations <- max_observations %>% lapply(function(x) {x[1:length(x)-1] = forced_exposure_params$max_sample_num 
                                                        + x})

      }
      else {
        error('invalid forced exposure trials specified. check whether forced_exposure_params$forced_exposure_trials was correctly specified')
        
      }
    
    }
   
  return(max_observations)   
  }

