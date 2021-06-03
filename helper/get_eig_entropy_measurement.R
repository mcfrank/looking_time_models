get_eig_entropy_for_creature <- function(distribution_df){
  
  
  all_features <- distribution_df %>% distinct(feature_index) %>% pull()
  
  all_feature_kl <- lapply(all_features, 
                           function(x){
                             get_kl_for_feature(x, 
                                                distribution_df)
                             
                           }) %>% 
    bind_rows()
  
  return(all_feature_kl)
}


get_eig_entropy_for_feature <- function(feature, 
                               distribution_df){
  
}