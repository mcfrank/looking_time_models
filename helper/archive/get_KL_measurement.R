
get_kl_for_creature <- function(distribution_df){
  
  all_features <- distribution_df %>% distinct(feature_index) %>% pull()
  
  all_feature_kl <- lapply(all_features, 
         function(x){
           get_kl_for_feature(x, 
                              distribution_df)
           
         }) %>% 
    bind_rows()
  
  return(all_feature_kl)
}

get_kl_for_feature <- function(feature, 
                               distribution_df){
  
  total_update_number <- length(distribution_df %>% 
                                  distinct(update_number) %>% 
                                  pull())
  
  index_track <- distribution_df %>% 
    distinct(update_number, trial_num, trial_observation_num) %>% 
    # because we won't be look at the first posterior 
    tail(-1)
  
  
  all_learning_step_updates <- c()
  
  for(update_i in 2:total_update_number){
    
    first_update_index <- update_i - 1
    second_update_index <- update_i 
    
    distribtuion_for_feature <- distribution_df %>% 
      filter(feature_index == feature)
    
    second_update <- distribtuion_for_feature %>% 
      filter(update_number == second_update_index)
    
    first_update <- distribtuion_for_feature %>% 
      filter(update_number == first_update_index)
    
    all_thetas <- distribution_df %>% distinct(theta) %>% pull()
    kl <- c() 
    for(t in all_thetas){
      
      second_update_posterior <- second_update %>% 
        filter(theta == t) %>% 
        pull(log_posterior)
      
      first_update_posterior <- first_update %>% 
        filter(theta == t)%>% 
        pull(log_posterior)
        
      
      # because everything is in log
      kl_for_t <- second_update_posterior + second_update_posterior - first_update_posterior  
      
      kl <- c(kl, kl_for_t)
    }
    
    current_step_kl <- matrixStats::logSumExp(kl)
    all_learning_step_updates <- c(all_learning_step_updates, 
                                   current_step_kl)
    
  }
  
  learning_updates <- tibble("kl" = all_learning_step_updates) %>% 
    mutate(update_step = row_number() + 1, 
           feature_index = feature) 
  
  learning_updates$trial_num <- index_track$trial_num
  learning_updates$trial_observation_num <- index_track$trial_observation_num
  
  return(learning_updates)
  
}




