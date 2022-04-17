
set_granch_params <- function(
                            grid_mu_theta, grid_sig_sq, grid_y,grid_epsilon, hypothetical_obs_grid_n,
                            mu_priors, 
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
    mutate(
      hypothetical_obs_grid_n = hypothetical_obs_grid_n, 
      params_info = paste("mp", mu_prior, 
                               "vp", V_prior, 
                               "ap", alpha_prior, 
                               "bp", beta_prior, 
                               "e", epsilon, 
                               "mu_e", mu_epsilons,
                               "sd_e", sd_epsilons,
                               "wEIG", world_EIG, 
                               sep = "_"),
           params_id = row_number())
  
  
  model_params_df <- model_params_df %>% 
    mutate(grid_mu_theta = list(grid_mu_theta), 
           grid_sig_sq = list(grid_sig_sq), 
           grid_y = list(grid_y), 
           grid_epsilon = list(grid_epsilon)) %>% 
    select(params_id, grid_mu_theta, grid_sig_sq, grid_y, grid_epsilon, mu_prior, V_prior, 
           alpha_prior, beta_prior, epsilon, mu_epsilon, sd_epsilon, world_EIG, max_observation, hypothetical_obs_grid_n)
  
  
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



# get the prior df so that we don't need to calculate multiple times throughout
make_lp_mu_sig_sq <- function(grid_mu_theta, grid_sig_sq,
                          mu_prior, lambda_prior, alpha_prior, beta_prior){
  
  lp_mu_sig_sq <- expand.grid(grid_mu_theta = grid_mu_theta,
                              grid_sig_sq = grid_sig_sq)
  lp_mu_sig_sq$lp_mu_sig_sq = mapply(score_mu_sig_sq, 
                                     lp_mu_sig_sq$grid_mu_theta, lp_mu_sig_sq$grid_sig_sq, 
                                     mu = mu_prior, 
                                     lambda = lambda_prior, 
                                     alpha = alpha_prior, 
                                     beta = beta_prior, log = TRUE)
  
  return(lp_mu_sig_sq)
}

make_df_y_given_mu_sig_sq <- function(lp_mu_sig_sq, grid_y){
  get_df_y_given_mu_sig_sq(lp_mu_sig_sq, grid_y)
}


 

make_prior_df <- function(lp_mu_sig_sq, grid_epsilon, prior_mu_epsilon, prior_sd_epsilon){
  # needs to add lp_epsilon here 
  lp_epsilon = tibble(grid_epsilon = grid_epsilon)
  lp_epsilon$lp_epsilon = mapply(score_epsilon, 
                                 lp_epsilon$grid_epsilon, mu = prior_mu_epsilon, sd = prior_sd_epsilon)
  
  prior_df <- merge(lp_mu_sig_sq, lp_epsilon)
  return(prior_df)
  
}
  









