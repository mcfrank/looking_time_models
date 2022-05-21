time_granch_simulation <- function(embedding_path, grid_mu_n, grid_sig_sq_n, grid_y_n, grid_epsilon_n, 
                                   hypothetical_obs_grid_n, 
                                   n_feature){
  
  
  embedding_path <- embedding_path
  stimuli_pool_size <- 10
  stimuli_pool <-  get_stimuli_pool(stimuli_pool_size, n_feature, embedding_path)
  
  
  mu_priors = c(0)
  mu_priors = c(0)
  V_priors = c(0.001)
  alpha_priors = c(1) 
  beta_priors = c(1) 
  epsilons = c(0.000001) 
  mu_epsilon = c(0.001)
  sd_epsilon = c(2)
  world_EIGs = c(0.0001) 
  max_observation = 500
  
  grid_mu_theta = seq(-2, 2, 4/grid_mu_n)
  grid_sig_sq = seq(-2, 2, 4/grid_sig_sq_n)
  grid_y = seq(-2, 2, 4/grid_y_n)
  grid_epsilon = seq(-2, 2, 4/grid_epsilon_n)
  hypothetical_obs_grid_n = hypothetical_obs_grid_n
  
  model_params <- set_granch_params(
    grid_mu_theta, grid_sig_sq, grid_y, grid_epsilon, hypothetical_obs_grid_n,
    mu_priors, V_priors, 
    alpha_priors, beta_priors, 
    epsilons, mu_epsilon, sd_epsilon,
    world_EIGs, max_observation)
  
  startTime <- Sys.time()
  model_params <- add_precalculated_prior_dfs(model_params)
  endTime <-  Sys.time()
  
  info_df <- tibble(
    time_for_prior = c(endTime - startTime), 
    len_grid_mu_theta = length(grid_mu_theta), 
    len_grid_sig_sq = length(grid_sig_sq), 
    len_grid_y = length(grid_y), 
    len_grid_epsilon = length(grid_epsilon), 
    hypothetical_obs_grid_number = hypothetical_obs_grid_n,
    prior_grid_row = nrow(model_params$prior_df[[1]]),
    df_y_given_mu_sig_sq_row = nrow(model_params$df_y_given_mu_sig_sq[[1]]),
    obj_size = object.size(model_params)
  )
  
  stims_df <- tibble(sequence_scheme = c("BBBBBB"),
                     n_features = n_feature
  ) %>% 
    mutate(
      stimuli_sequence = map(sequence_scheme, function(ss){make_real_stimuli_df(ss, 
                                                                                background = get_bd_pair(stimuli_pool)[1, ], 
                                                                                deviant = get_bd_pair(stimuli_pool)[2, ])}))
  
  
  
  full_params_df <- make_simulation_params(n_sim = 1,model_params, stims_df)
  
  startTime <- Sys.time()
  all_sims_res <- full_params_df %>%
    mutate(row_number = row_number()) %>% 
    group_by(row_number) %>% 
    nest() %>%
    mutate(results = map(data,
                         function(df) granch_main_simulation(params = df))) %>%
    unnest(cols = c(data, results))
  endTime <-  Sys.time()
  
  info_df <- info_df %>% 
    mutate(ranch_sim_time = endTime - startTime, 
           n_f = n_feature, 
           n_hypo_obs = hypothetical_obs_grid_n)
  
  return (info_df)
}