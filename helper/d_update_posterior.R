
get_df_lp_z_y_raw <- function(t, stimulus_idx, last_t_for_last_stimulus, index, df_model, m_observation, current_observation, grid_epsilon, df_lp_y_given_theta){
  

  observations_on_this_stimulus_till_this_t = na.omit(m_observation[last_t_for_last_stimulus:t, index])
                                                     
  df_lp_z_given_y = tibble(
    "epsilon" = grid_epsilon
  )

  if (t == 1 || stimulus_idx != df_model$stimulus_idx[[t-1]]){
    df_lp_z_given_y$lp_z_given_y_ONE =  lp_z_ij_given_y(zij = current_observation[[index]], yi = 1, epsilon = grid_epsilon)
    df_lp_z_given_y$lp_z_given_y_ZERO = lp_z_ij_given_y(zij = current_observation[[index]], yi = 0, epsilon = grid_epsilon)
  }else{
    df_lp_z_given_y$lp_z_given_y_ONE = rowSums(sapply(observations_on_this_stimulus_till_this_t, 
                                                     function(x){lp_z_ij_given_y(x, 1, grid_epsilon)}))
    df_lp_z_given_y$lp_z_given_y_ZERO = rowSums(sapply(observations_on_this_stimulus_till_this_t, 
                                                       function(x){lp_z_ij_given_y(x, 0, grid_epsilon)}))
  }
  
  df_lp_z_y_raw <- expand_grid(df_lp_y_given_theta, df_lp_z_given_y)
  return(df_lp_z_y_raw)
}


get_lp_z_given_theta_for_raw <- function(t, last_t_for_last_stimulus, index, df_model, 
                                         current_df_lp_z_given_theta, ll_df_z_given_theta){

   if(t == 1 | df_model$stimulus_idx[[t]] == 1){
     lp_z_given_theta <-rowLogSumExps(lx = as.matrix(current_df_lp_z_given_theta[,c("lp_z_y_ONE", "lp_z_y_ZERO")]))
  }else{
      if(df_model$stimulus_idx[[t]] != df_model$stimulus_idx[[t-1]]){
     lp_z_given_theta <- rowLogSumExps(lx = as.matrix(current_df_lp_z_given_theta[,c("lp_z_y_ONE", "lp_z_y_ZERO")])) + 
                               (ll_df_z_given_theta[[t-1]][[index]])$lp_z_given_theta
                            }else{
      lp_z_given_theta<- rowLogSumExps(lx = as.matrix(current_df_lp_z_given_theta[,c("lp_z_y_ONE", "lp_z_y_ZERO")])) + 
                                (ll_df_z_given_theta[[last_t_for_last_stimulus]][[index]])$lp_z_given_theta
                            }
        }
                             
  return(lp_z_given_theta)
  
}

get_df_lp_z_given_theta <- function(t, 
                                    df_lp_y_given_theta, 
                                    ll_df_z_given_theta,# needs to be about each observation, not each stimulus  
                                    stimulus_idx, 
                                    index, 
                                    df_model, 
                                    m_observation,
                                    current_observation, 
                                    grid_theta, grid_epsilon, 
                                    alpha_theta, beta_theta){
  
  current_df_lp_z_given_theta = ll_df_z_given_theta[[t]][[index]]
  last_t_for_last_stimulus = ifelse(stimulus_idx == 1, 1,
                                    max((df_model[df_model$stimulus_idx == stimulus_idx-1,])$t, na.rm = TRUE)
  )

  df_lp_z_y_raw <-  get_df_lp_z_y_raw(t, stimulus_idx, last_t_for_last_stimulus, index, df_model, m_observation, current_observation, grid_epsilon, df_lp_y_given_theta)
  
  current_df_lp_z_given_theta$lp_z_y_ONE <-  df_lp_z_y_raw$lp_y_ONE_given_theta + df_lp_z_y_raw$lp_z_given_y_ONE
  current_df_lp_z_given_theta$lp_z_y_ZERO <- df_lp_z_y_raw$lp_y_ZERO_given_theta + df_lp_z_y_raw$lp_z_given_y_ZERO
  current_df_lp_z_given_theta$lp_z_given_theta <- get_lp_z_given_theta_for_raw(t, last_t_for_last_stimulus, index, 
                                                                               df_model, current_df_lp_z_given_theta, ll_df_z_given_theta)
  
 
  return (current_df_lp_z_given_theta)
}
