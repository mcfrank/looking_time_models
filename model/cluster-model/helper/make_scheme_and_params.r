

set_model_params <- function(alpha_priors, 
                             beta_priors, 
                             alpha_epsilons, 
                             beta_epsilosn, 
                             noise_parameters, 
                             world_EIGs, 
                             max_observation){
  
  model_params_df <- crossing(
    alpha_prior = alpha_priors,
    beta_prior = beta_priors, 
    alpha_epsilon = alpha_epsilons, 
    beta_epsilon = beta_epsilosn,
    noise_parameter = noise_parameters,
    world_EIG = world_EIGs, 
    max_observation = max_observation
  ) %>% 
    mutate(params_id = row_number(), 
    )
  
  return(model_params_df)
  
}

set_stim_params <- function(sequence_scheme, features_df){
  
  
  
  stimulus_sequence_df <- expand_grid(sequence_scheme, features_df)%>% 
    rowwise() %>% 
    mutate(
    #  total_features_n = features_df$total_features_n, 
   #   on_features_n = features_df$on_features_n, 
      stimuli_sequence = nest(scheme_to_stimuli(sequence_scheme, 
                                                n_features = n_features, 
                                                on_features_n = on_features_n), data = everything())) %>% 
    ungroup() 
  
  return(stimulus_sequence_df)
}






make_simulation_params <- function(n_sim,
                                   model_params, 
                                   stim_params
){
  
  expand_grid(model_params, stim_params) %>% 
    left_join(expand_grid(sub_id = 1:n_sim, params_id = model_params$params_id))
  
}




scheme_to_stimuli <- function(sequence_scheme, n_features, on_features_n){
  
  
  background_creature <- sample(c(rep(FALSE, n_features - on_features_n), 
                                  rep(TRUE, on_features_n)))
  
  # construct deviant creature from background creature 
  deviant_creature <- background_creature
  
  # if not all features can be flipped, then select a random subset of the features to flip as many as possible 
  if (on_features_n > n_features * .5){
    # randomly select a subset of features to be flipped 
    # R has annoying behaviors for sample when the vector length is 1
    # can't use if_else because it has an annoying behavior that forces both clause return the same thing 
    
    if (n_features - on_features_n == 1 | n_features == on_features_n){

      on_location <-  sample(which(background_creature == TRUE), 1)
      off_location <-  which(background_creature == FALSE)
    }else{
      on_location <- sample(which(background_creature == TRUE), size = n_features - on_features_n)
      off_location <- sample(which(background_creature == FALSE), size = n_features - on_features_n)
    }
  
  # if all features can be flipped, flip all features   
  }else{
    on_location <- which(background_creature == TRUE)
    off_location <- sample(x = which(background_creature == FALSE), size = on_features_n)
  }
  
  deviant_creature <- background_creature
  deviant_creature[on_location] <- FALSE 
  deviant_creature[off_location] <- TRUE 
  
  block_list <- strsplit(sequence_scheme, "")[[1]]
  
  background_sequence <- replicate(length(block_list), background_creature, simplify = FALSE)
  background_sequence[which(block_list == "D")] <- replicate(length(which(block_list == "D")), 
                                                             deviant_creature, simplify = FALSE)
  
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


