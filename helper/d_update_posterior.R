
get_df_lp_z_given_theta <- function(t, 
                                    ll_df_z_given_theta,# needs to be about each observation, not each stimulus  
                                    stimulus_idx, 
                                    index, 
                                    df_model, 
                                    m_observation,
                                    current_observation, 
                                    grid_theta, grid_epsilon, 
                                    alpha_theta, beta_theta, 
                                    alpha_epsilon, beta_epsilon){
  
  # see if this is a new stimulus or an old stimulus 
  # if starting a new stimulus 
  # relying on short circuiting to handle the t=1 part
  if(t == 1 || df_model$stimulus_idx[[t]] != df_model$stimulus_idx[[t-1]]){
    
    df_lp_y_given_theta = tibble(
      "theta" = grid_theta, 
      "lp_y_ONE_given_theta" =  lp_yi_given_theta(yi = 1, theta = grid_theta ), 
      "lp_y_ZERO_given_theta" = lp_yi_given_theta(yi = 0, theta = grid_theta )
    )
    
    
    df_lp_z_given_y = tibble(
      "epsilon" = grid_epsilon, 
      "lp_z_given_y_ONE" = lp_z_ij_given_y(zij = current_observation[[index]], yi = 1, epsilon = grid_epsilon),
      "lp_z_given_y_ZERO" = lp_z_ij_given_y(zij = current_observation[[index]], yi = 0, epsilon = grid_epsilon)
    )
    
    df_lp_z_y_raw <- expand_grid(df_lp_y_given_theta, df_lp_z_given_y)
    
    
    # this needs to be keep track of separately 
    
    if (t == 1){
      df_lp_z_given_theta <- tibble(
        "theta" = df_lp_z_y_raw$theta, 
        "epsilon" = df_lp_z_y_raw$epsilon,
        "lp_z_y_ONE" = df_lp_z_y_raw$lp_y_ONE_given_theta + df_lp_z_y_raw$lp_z_given_y_ONE, 
        "lp_z_y_ZERO" = df_lp_z_y_raw$lp_y_ZERO_given_theta + df_lp_z_y_raw$lp_z_given_y_ZERO, 
        "lp_z_given_theta" = logSumExp(c(df_lp_z_y_raw$lp_y_ONE_given_theta + df_lp_z_y_raw$lp_z_given_y_ONE, 
                                         df_lp_z_y_raw$lp_y_ZERO_given_theta + df_lp_z_y_raw$lp_z_given_y_ZERO))
      )
      
      df_lp_z_given_theta$lp_z_given_theta <- rowLogSumExps(lx = as.matrix(df_lp_z_given_theta[,c("lp_z_y_ONE", "lp_z_y_ZERO")]))
      
      
    }else{
      df_lp_z_given_theta <- tibble(
        "theta" = df_lp_z_y_raw$theta, 
        "epsilon" = df_lp_z_y_raw$epsilon,
        "lp_z_y_ONE" = df_lp_z_y_raw$lp_y_ONE_given_theta + df_lp_z_y_raw$lp_z_given_y_ONE, 
        "lp_z_y_ZERO" = df_lp_z_y_raw$lp_y_ZERO_given_theta + df_lp_z_y_raw$lp_z_given_y_ZERO, 
        "lp_z_given_theta" = logSumExp(c(df_lp_z_y_raw$lp_y_ONE_given_theta + df_lp_z_y_raw$lp_z_given_y_ONE, 
                                         df_lp_z_y_raw$lp_y_ZERO_given_theta + df_lp_z_y_raw$lp_z_given_y_ZERO)) + 
          (ll_df_z_given_theta[[t-1]][[index]])$lp_z_given_theta
      )
      
      df_lp_z_given_theta$lp_z_given_theta <- rowLogSumExps(lx = as.matrix(df_lp_z_given_theta[,c("lp_z_y_ONE", "lp_z_y_ZERO")])) + 
        + (ll_df_z_given_theta[[t-1]][[index]])$lp_z_given_theta
      
      
    }
    
    
    
    
    
  }else{
    
    # needs to use the t to figure out what the last trial number's last t is 
    df_lp_y_given_theta = tibble(
      "theta" = grid_theta, 
      "lp_y_ONE_given_theta" =  lp_yi_given_theta(yi = 1, theta = grid_theta ), 
      "lp_y_ZERO_given_theta" = lp_yi_given_theta(yi = 0, theta = grid_theta )
    )
    
    
    # current only looking at single feature creature 
    # OK THIS ACTUALLY NEEDS TO BE ALL OBSERVATIONS ON THIS STIMULUS 
    current_stimulus_idx = df_model$stimulus_idx[[t]]
    if (current_stimulus_idx == 1){
      
      #print(index)
      observations_on_this_stimulus_till_this_t = na.omit(m_observation[1:t, index])
      
      df_lp_z_given_y = tibble(
        "epsilon" = grid_epsilon, 
        "lp_z_given_y_ONE" = rowSums(sapply(observations_on_this_stimulus_till_this_t, 
                                            function(x){lp_z_ij_given_y(x, 1, grid_epsilon)})),
        "lp_z_given_y_ZERO" = rowSums(sapply(observations_on_this_stimulus_till_this_t, 
                                             function(x){lp_z_ij_given_y(x, 0, grid_epsilon)}))
      )
      
      df_lp_z_y_raw <- expand_grid(df_lp_y_given_theta, df_lp_z_given_y)
      
      
      df_lp_z_given_theta <- tibble(
        "theta" = df_lp_z_y_raw$theta, 
        "epsilon" = df_lp_z_y_raw$epsilon,
        "lp_z_y_ONE" = df_lp_z_y_raw$lp_z_given_y_ONE + df_lp_y_given_theta$lp_y_ONE_given_theta, 
        "lp_z_y_ZERO" = df_lp_z_y_raw$lp_z_given_y_ZERO + df_lp_y_given_theta$lp_y_ZERO_given_theta,
        "lp_z_given_theta" = matrixStats::logSumExp(c(df_lp_z_y_raw$lp_z_given_y_ONE + 
                                                        df_lp_z_y_raw$lp_y_ONE_given_theta,
                                                      df_lp_z_y_raw$lp_z_given_y_ZERO + 
                                                        df_lp_z_y_raw$lp_y_ZERO_given_theta
        )) 
      )
      
      df_lp_z_given_theta$lp_z_given_theta <- rowLogSumExps(lx = as.matrix(df_lp_z_given_theta[,c("lp_z_y_ONE", "lp_z_y_ZERO")]))
      
      
    }else{
      
      last_t_for_last_stimulus = max((df_model[df_model$stimulus_idx == current_stimulus_idx-1,])$t, na.rm = TRUE)
      
      observations_on_this_stimulus_till_this_t = na.omit(m_observation[(last_t_for_last_stimulus+1):t, index])
      
      
      df_lp_z_given_y = tibble(
        "epsilon" = grid_epsilon, 
        "lp_z_given_y_ONE" = rowSums(sapply(observations_on_this_stimulus_till_this_t, 
                                            function(x){lp_z_ij_given_y(x, 1, grid_epsilon)})),
        "lp_z_given_y_ZERO" = rowSums(sapply(observations_on_this_stimulus_till_this_t, 
                                             function(x){lp_z_ij_given_y(x, 0, grid_epsilon)}))
      )
      
      df_lp_z_y_raw <- expand_grid(df_lp_y_given_theta, df_lp_z_given_y)
      
      
      df_lp_z_given_theta <- tibble(
        "theta" = df_lp_z_y_raw$theta, 
        "epsilon" = df_lp_z_y_raw$epsilon,
        "lp_z_y_ONE" = df_lp_z_y_raw$lp_z_given_y_ONE + df_lp_y_given_theta$lp_y_ONE_given_theta, 
        "lp_z_y_ZERO" = df_lp_z_y_raw$lp_z_given_y_ZERO + df_lp_y_given_theta$lp_y_ZERO_given_theta,
        "lp_z_given_theta" = matrixStats::logSumExp(c(df_lp_z_y_raw$lp_z_given_y_ONE + 
                                                        df_lp_z_y_raw$lp_y_ONE_given_theta,
                                                      df_lp_z_y_raw$lp_z_given_y_ZERO + 
                                                        df_lp_z_y_raw$lp_y_ZERO_given_theta
        )) + 
          (ll_df_z_given_theta[[last_t_for_last_stimulus]][[index]])$lp_z_given_theta
      )
      
      df_lp_z_given_theta$lp_z_given_theta <- rowLogSumExps(lx = as.matrix(df_lp_z_given_theta[,c("lp_z_y_ONE", "lp_z_y_ZERO")])) + 
        (ll_df_z_given_theta[[last_t_for_last_stimulus]][[index]])$lp_z_given_theta
      
      
    }
    
    
    
    
    
  }
  
  
  
  return (df_lp_z_given_theta)
  
}
