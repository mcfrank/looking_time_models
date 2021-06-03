get_surprise_for_feature_updates <- function(feature_i, observation, updates_df){
  
  feature_df <- updates_df %>% 
    filter(feature_index == feature_i)
  
  thetas <- feature_df %>% 
    distinct(theta) %>% 
    pull()
  
  all_updates <- feature_df %>% 
    distinct(update_number) %>% 
    pull()
  
  observation_for_feature <- observation %>% 
    pull(paste0("V", feature_i))
    
  
  # FIXME: currently not including the prior's surprise(?)
  all_surprise <- c()
  
  for (i in seq(1, length(all_updates)-1, 1)){
    
    current_observation <- observation_for_feature[[i+1]]
    current_ps <- feature_df %>% 
         filter(update_number == i) %>% 
         pull(log_posterior) %>% 
         exp()
    

    if (current_observation == 1){
      current_s <- -log(thetas %*% current_ps)[[1]] # get the element  
    }else if(current_observation == 0){
      current_s <- -log((1-thetas) %*% current_ps)[[1]]
    }
    
    all_surprise <- c(all_surprise, current_s)
  }
    # current_lps <- feature_df %>% 
    #   filter(update_number == i) %>% 
    #   pull(log_posterior) 
    # 
    # 
    # current_s <-weighted.mean(x = -current_lps, w = exp(current_lps)) 
    # all_surprise <- c(all_surprise, current_s)
    
  
  
  surprise_df <- tibble(surprise = all_surprise, 
                        update_number = seq(1, length(all_surprise), 1)) %>% 
    mutate(feature_index = feature_i)
  
  return(surprise_df)
  
}


get_surprise_for_creature_updates <- function(observation, updates_df){
  
  all_features <- updates_df %>% 
    distinct(feature_index) %>% 
    pull()
  
  lapply(all_features, function(x){
    get_surprise_for_feature_updates(x, observation, updates_df)
  }) %>% 
    bind_rows()
  
  
}