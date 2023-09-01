


run_toy_model <- function(prior_df, im = "kl"){
 
  if(im == "kl"){
    false_df <- pmap_df(prior_df, get_kl_after_obs, obs = FALSE) %>% 
      rename(false_obs = `kullback-leibler`)
    true_df <- pmap_df(prior_df, get_kl_after_obs, obs = TRUE) %>% 
      rename(true_obs = `kullback-leibler`)
  }else if(im == "jkl"){
    false_df <- pmap_df(prior_df, get_jkl_after_obs, obs = FALSE) %>% 
      rename(false_obs = `kullback-leibler`)
    true_df <- pmap_df(prior_df, get_jkl_after_obs, obs = TRUE) %>% 
      rename(true_obs = `kullback-leibler`)
  }
  
  full_df <- bind_cols(prior_df, false_df, true_df) %>% 
    pivot_longer(cols = c("false_obs", "true_obs"), names_to = "value_type", values_to = "value")
  
  return(full_df)
  
  
  
}

