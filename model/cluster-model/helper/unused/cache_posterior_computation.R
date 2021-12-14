









### CACHED CODE
cache_observations <- function(t, m_observation, feature_number) {
  
  current_unique_poss_combos <- get_unique_combination(t, 
                                                       m_observation, 
                                                       feature_number)
  
  feature_pos <- current_unique_poss_combos$feature_pos
  
  all_poss_combos <- expand_grid(current_unique_poss_combos, 
                                 hypothetical_observation = c(TRUE, FALSE)) 
  
  n_poss_combination <- nrow(all_poss_combos)
  all_poss_combos$kl <- rep(NA_real_, n_poss_combination)
  all_poss_combos$upcoming_preds <- rep(NA_real_, n_poss_combination)
  
  feature_occurence <- na.omit(as.vector(sapply(feature_pos, function(x){first(na.omit(x))})))
}

compute_posterior_cached <- function (cache_stuff, 
                                      

# ---- get joint grid post over theta and epsilon ---
# caching scheme: now for each feature that could occur, score that feature
for (index in feature_occurence) {
  ll_df_z_given_theta[[t]][[index]] <- get_df_lp_z_given_theta(t, 
                                                               df_lp_y_given_theta,
                                                               ll_df_z_given_theta, 
                                                               stimulus_idx,   # needs to be about each observation, not each stimulus  
                                                               index, 
                                                               df_model, 
                                                               m_observation,
                                                               current_observation, 
                                                               grid_theta, grid_epsilon, 
                                                               params_df$alpha_prior,  
                                                               params_df$beta_prior)
  
  unnormalized_log_post <- ll_df_z_given_theta[[t]][[index]]$lp_z_given_theta + 
    df_lp_theta_epsilon$lp_theta + 
    df_lp_theta_epsilon$lp_epsilon
  
  ll_df_post[[t]][[index]]$unnormalized_log_post <- unnormalized_log_post
  ll_df_post[[t]][[index]]$log_post <-  ll_df_post[[t]][[index]]$unnormalized_log_post - matrixStats::logSumExp( ll_df_post[[t]][[index]]$unnormalized_log_post)
  ll_df_post[[t]][[index]]$post <- exp(ll_df_post[[t]][[index]]$log_post)
}

# find corresponding calculated value above for each feature and propagate into full post
for (i in 1:feature_number) {
  calculated_value_index <- match(TRUE, sapply(feature_pos, function(x){i %in% x}))
  calculated_value_index_in_ll <- feature_occurence[[calculated_value_index]]
  ll_df_post[[t]][[i]] <- ll_df_post[[t]][[calculated_value_index_in_ll]]
  ll_df_z_given_theta[[t]][[i]] <- ll_df_z_given_theta[[t]][[calculated_value_index_in_ll]]
}

# ---- EIG computation ---
# use previous post (at this observation) and new potential posts
# to compute EIG
prev_post_list <- ll_df_post[[t]][feature_occurence]
prev_z_given_theta_list <- ll_df_z_given_theta[[t]][feature_occurence]

upcoming_post_list <- lapply(seq(1, n_poss_combination),
                             function(x){
                               expand_grid(theta = grid_theta, 
                                           epsilon = grid_epsilon)
                             })

last_t_for_last_stimulus = ifelse(stimulus_idx == 1, 1,
                                  max((df_model[df_model$stimulus_idx == stimulus_idx-1,])$t, na.rm = TRUE)
)
prev_z_given_theta_last_stimulus <- ll_df_z_given_theta[[last_t_for_last_stimulus]][feature_occurence]

# enumerate again over possible future observations
for (i in 1:n_poss_combination) {
  all_hypothetical_observations_on_this_stimulus = c((unlist(all_poss_combos$unique_combination[[i]]))[last_t_for_last_stimulus:t], 
                                                     all_poss_combos$hypothetical_observation[[i]])
  
  upcoming_post_df = upcoming_post_list[[i]]
  
  prev_observation_post = prev_post_list[[ceiling(i/2)]]
  prev_observation_z_given_theta = prev_z_given_theta_list[[ceiling(i/2)]]
  prev_last_stimulus_observation_z_given_theta = prev_z_given_theta_last_stimulus[[ceiling(i/2)]]
  
  upcoming_df_z_given_theta = eig_get_df_lp_z_given_theta(t,
                                                          df_model,
                                                          prev_last_stimulus_observation_z_given_theta,
                                                          all_hypothetical_observations_on_this_stimulus, # contains unique combination + hypothetical scenarios 
                                                          grid_theta, grid_epsilon, 
                                                          df_lp_y_given_theta)
  
  upcoming_post_list[[i]]$unnormalized_log_post <- upcoming_df_z_given_theta$lp_z_given_theta +  df_lp_theta_epsilon$lp_theta + 
    df_lp_theta_epsilon$lp_epsilon
  
  upcoming_post_list[[i]]$log_post <- upcoming_post_list[[i]]$unnormalized_log_post - matrixStats::logSumExp(upcoming_post_list[[i]]$unnormalized_log_post)
  upcoming_post_list[[i]]$post <- exp(upcoming_post_list[[i]]$log_post)
  
  all_poss_combos$kl[i] <- get_kl(upcoming_post_list[[i]]$post, 
                                  prev_post_list[[ceiling(i/2)]]$post)
  all_poss_combos$upcoming_preds[i] <- noisy_post_pred(prev_post_list[[ceiling(i/2)]]$theta, 
                                                       prev_post_list[[ceiling(i/2)]]$epsilon, 
                                                       prev_post_list[[ceiling(i/2)]]$post, 
                                                       all_poss_combos$hypothetical_observation[i]) 
} w