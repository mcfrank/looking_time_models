get_entropy_for_feature_one_update <- function(lps){
  sum(lps * exp(lps))
}

get_entropy_for_feature_updates <- function(feature_i, updates_df){
  
  feature_df <- updates_df %>% 
    filter(feature_index == feature_i)
  
  thetas <- feature_df %>% 
    distinct(theta) %>% 
    pull()
  
  all_updates <- feature_df %>% 
    distinct(update_number) %>% 
    pull()
  
  
  # FIXME: currently not including the prior's entropy
  all_entropy <- c()
  
  for (i in all_updates){
    
    current_lps <- feature_df %>% 
      filter(update_number == i) %>% 
      pull(log_posterior) 
    current_ep <- get_entropy_for_feature_one_update(current_lps)
    all_entropy <- c(all_entropy, current_ep)
  }
  
  entropy_df <- tibble(entropy = all_entropy, 
                       #predictability = -all_entropy,
                       update_number = seq(1, length(all_entropy), 1)) %>% 
    mutate(feature_index = feature_i)
  
  return(entropy_df)
  
}

get_entropy_for_creature_updates <- function(updates_df){
  all_features <- updates_df %>% 
    distinct(feature_index) %>% 
    pull()
  
  lapply(all_features, function(x){
    get_entropy_for_feature_updates(x, updates_df)
  }) %>% 
    bind_rows()
  
}
