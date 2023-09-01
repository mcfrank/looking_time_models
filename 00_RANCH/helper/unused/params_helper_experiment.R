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


simulation_wrapper <- function(subject_n, stimuli_sequence, noise_parameter, 
                                eig_from_world, max_observation, alpha_prior, beta_prior, sim_id, 
                                type = "regular"){
  
  sim_df <- main_simulations(subject_n, 
                             stimuli_sequence[[sim_id]], 
                             noise_parameter, 
                             eig_from_world,
                             max_observation, # should this be per trial or in total? currently per trial 
                             grid_theta = grid_theta, 
                             grid_epsilon = grid_epsilon, 
                             alpha_prior, 
                             beta_prior,
                             alpha_epsilon = alpha_epsilon, 
                             beta_epsilon = beta_epsilon, 
                             forced_exposure = FALSE,
                             forced_sample = NULL) #%>% 
    #get_sim_res() %>% 
    #get_sim_eva()
  
  sim_df$sim_id <- sim_id
  sim_df$type <- type
  
  print(sim_id)
  return(sim_df)  
}

simulation_wrapper_for_linking_data <- function(sim_id, n, stimuli_sequence, noise_parameter, 
                               eig_from_world, max_observation, alpha_prior, beta_prior, alpha_epsilon, beta_epsilon){
  
  sim_df <- main_simulations(n, 
                             stimuli_sequence[[sim_id]], 
                             noise_parameter, 
                             eig_from_world,
                             max_observation, # should this be per trial or in total? currently per trial 
                             grid_theta = grid_theta, 
                             grid_epsilon = grid_epsilon, 
                             alpha_prior, 
                             beta_prior,
                             alpha_epsilon, 
                             beta_epsilon, 
                             forced_exposure = FALSE,
                             forced_sample = NULL) #%>% 
  #get_sim_res() %>% 
  #get_sim_eva()
  
  sim_df$sim_id <- sim_id
  
  print(sim_id)
  return(sim_df)  
}


get_fam_pref_df <- function(params_df){
  bckgrd_df <- params_df %>% 
    filter(type == "background") %>% 
    pivot_wider(names_from = type, 
                values_from = mean_lt) %>% 
    select(sim_id, background, forced_exposure_n, 
           noise_parameter, eig_from_world, alpha_prior, beta_prior, 
           sd_lt, ci.upper, ci.lower, n) %>% 
    rename(bckgd_mean_lt = background, 
           bckgd_sd_lt = sd_lt, 
           bckgd_ci_upper = ci.upper, 
           bckgrd_ci_lower = ci.lower)
  
  deviant_df <- params_df %>% 
    filter(type == "deviant") %>% 
    pivot_wider(names_from = type, 
                values_from = mean_lt) %>% 
    select(sim_id, deviant, 
           sd_lt, ci.upper, ci.lower) %>% 
    rename(deviant_mean_lt = deviant, 
           deviant_sd_lt = sd_lt, 
           deviant_ci_upper = ci.upper, 
           deviant_ci_lower = ci.lower)
  
  
  sim_res_df <- bckgrd_df %>% 
    left_join(deviant_df, by = "sim_id") 
  
  return(sim_res_df)
  
}


t_test_from_mean <- function(m1,m2,s1,s2,n1,n2, m0=0,equal.variance=FALSE)
{
  if( equal.variance==FALSE ) 
  {
    se <- sqrt( (s1^2/n1) + (s2^2/n2) )
    # welch-satterthwaite df
    df <- ( (s1^2/n1 + s2^2/n2)^2 )/( (s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1) )
  } else
  {
    # pooled standard deviation, scaled by the sample sizes
    se <- sqrt( (1/n1 + 1/n2) * ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2) ) 
    df <- n1+n2-2
  }      
  t <- (m1-m2-m0)/se 
  dat <- c(m1-m2, se, t, 2*pt(-abs(t),df))    
  names(dat) <- c("Difference of means", "Std Error", "t", "p-value")
  return(dat) 
}

t_test_wrapper <- function(sim_id, bckgd_mean_lt, deviant_mean_lt, bckgd_sd_lt, deviant_sd_lt, n){
  data <- t_test_from_mean(bckgd_mean_lt, deviant_mean_lt, bckgd_sd_lt, deviant_sd_lt, n, n)
  
}

