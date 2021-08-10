get_sim_res <- function(raw_sim_res){
  raw_sim_res %>% 
    group_by(subject_id, stimulus_idx) %>% 
    summarise(sample_n = n())
  
}

get_sim_eva <- function(sim_res){
  # if subjec t= 1 sd will be na 
  sim_res %>% 
    group_by(stimulus_idx) %>%
    filter(!is.na(stimulus_idx)) %>% 
    summarise(mean_lt = mean(sample_n, na.rm = TRUE), 
              sd_lt = sd(sample_n, na.rm = TRUE), 
              n = n())
  
}


simulation_wrappter <- function(subject_n, stimuli_sequence, noise_parameter, 
                                eig_from_world, max_observation, alpha_prior, beta_prior, forced_sample, sim_id, 
                                type){
  
  sim_df <- main_simulations(subject_n, 
                             stimuli_sequence, 
                             noise_parameter, 
                             eig_from_world,
                             max_observation, # should this be per trial or in total? currently per trial 
                             grid_theta = grid_theta, 
                             grid_epsilon = grid_epsilon, 
                             alpha_prior, 
                             beta_prior,
                             alpha_epsilon = alpha_epsilon, 
                             beta_epsilon = beta_epsilon, 
                             forced_exposure = TRUE,
                             forced_sample) %>% 
    get_sim_res() %>% 
    get_sim_eva()
  
  sim_df$sim_id <- sim_id
  sim_df$type <- type
  
  print(sim_id)
  return(sim_df)  
}