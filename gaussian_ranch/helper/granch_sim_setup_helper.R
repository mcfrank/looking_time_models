
set_granch_params <- function(mu_priors, 
                             V_priors, 
                             alpha_priors, 
                             beta_priors, 
                             epsilons, 
                             mu_epsilons, 
                             sd_epsilons,
                             world_EIGs, 
                             max_observation){
  
  # what's the shape of the epsilon? normal?
  
  model_params_df <- crossing(
    mu_prior = mu_priors,
    V_prior = V_priors, 
    alpha_prior = alpha_priors, 
    beta_prior = beta_priors,
    epsilon = epsilons,
    mu_epsilon = mu_epsilons, 
    sd_epsilon = sd_epsilons,
    world_EIG = world_EIGs, 
    max_observation = max_observation
  ) %>% 
    mutate(params_info = paste("mp", mu_prior, 
                               "vp", V_prior, 
                               "ap", alpha_prior, 
                               "bp", beta_prior, 
                               "e", epsilon, 
                               "mu_e", mu_epsilons,
                               "sd_e", sd_epsilons,
                               "wEIG", world_EIG, 
                               sep = "_"),
           params_id = row_number())
  
  return(model_params_df)
  
}

# stimuli creation is going to look different 
# some sort of reading in vectors and make it into dataframe 


make_simulation_params <- function(n_sim,
                                   model_params, 
                                   stim_params
){
  
  expand_grid(model_params, stim_params) %>% 
    left_join(expand_grid(sub_id = 1:n_sim, params_id = model_params$params_id))
  
}

