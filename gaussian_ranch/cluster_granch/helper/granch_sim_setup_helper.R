
## Set up stimulus ---------------------------------
get_stimuli_pool <- function(n_stim,n_dim, embedding_path){
  em <- read_csv(embedding_path, col_names = FALSE)
  em[sample(nrow(em), n_stim), 1:n_dim]
}

get_bd_pair <- function(stimuli_pool){
  # might consider adding distance? 
  bd_pair <- as.matrix(stimuli_pool)[sample(2),]
  return(bd_pair)
}

make_real_stimuli_df <- function(sequence_scheme, background, deviant){
  
  background <- as.vector(background)
  deviant <- as.vector(deviant)
  
  block_list <- strsplit(sequence_scheme, "")[[1]]
  
  background_sequence <- replicate(length(block_list), background, simplify = FALSE)
  background_sequence[which(block_list == "D")] <- replicate(length(which(block_list == "D")), 
                                                             deviant, simplify = FALSE)
  
  tidy_creature_sequence <- bind_rows(lapply(background_sequence,
                                             function(x) x %>% as_tibble_row(.name_repair = make.names))) 
  
  rename_column <- function(x){paste0("V", x)}
  tidy_column_names <- lapply(1:length(background), rename_column)
  colnames(tidy_creature_sequence) <- tidy_column_names
  tidy_creature_sequence <- tidy_creature_sequence %>% 
    mutate(trial_number = row_number())
  return(tidy_creature_sequence)
}

## Set up grid values ---------------------------------

get_grid_mu_theta <- function(stimuli_pool, sd_n, sd_step){
  seq(mean(as.matrix(stimuli_pool)) - sd_n * sd(as.matrix(stimuli_pool)), 
      mean(as.matrix(stimuli_pool)) + sd_n * sd(as.matrix(stimuli_pool)), 
      sd(as.matrix(stimuli_pool)) / sd_step)
}


find_min_diff <- function(stimuli_pool){
  p.sorted <- sort(as.vector(as.matrix(stimuli_pool)), decreasing = TRUE)
  min_val <- max(p.sorted) - min(p.sorted)
  for (i in 1:(length(p.sorted)-1)){
    diff = p.sorted[i] - p.sorted[i + 1]
    if (diff < min_val){min_val = diff}
  }
  return(min_val)
}

## Set up parameters ---------------------------------

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
  





add_precalculated_prior_dfs <- function(model_params){
  
  # sketchy but works
  # first one lp_mu_sig_sq
  # second one df_y_given_mu_sig_sq
  # third one prior_df 
  ll_pre_calculated_df <- lapply(seq(1, nrow(model_params), 1), 
                                 function(x){
                                   lapply(seq(1, 3, 1), 
                                          function(x){
                                            NULL
                                          })
                                 })
  
  for(i in 1: nrow(model_params)){
    # 1st element lp_mu_sig_sq
    ll_pre_calculated_df[[i]][[1]] <- make_lp_mu_sig_sq(model_params$grid_mu_theta[[i]], 
                                                        model_params$grid_sig_sq[[i]], 
                                                        model_params$mu_prior[[i]], model_params$V_prior[[i]], 
                                                        model_params$alpha_prior[[i]], model_params$beta_prior[[i]])
    
    # 2nd element df_y_given_mu_sig_sq 
    
    ll_pre_calculated_df[[i]][[2]] <- make_df_y_given_mu_sig_sq(ll_pre_calculated_df[[i]][[1]], 
                                                                model_params$grid_y[[i]])
    
    
    ll_pre_calculated_df[[i]][[3]] <- make_prior_df(ll_pre_calculated_df[[i]][[1]], 
                                                    model_params$grid_epsilon[[i]], 
                                                    model_params$mu_epsilon[[i]], 
                                                    model_params$sd_epsilon[[i]])
  }
  
  model_params$df_y_given_mu_sig_sq <- lapply(ll_pre_calculated_df, function(x){x[[2]]})
  model_params$prior_df <- lapply(ll_pre_calculated_df, function(x){x[[3]]})
  
  
  return(model_params)
}





