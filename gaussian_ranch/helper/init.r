
initialize_z_given_mu_sig_sq <- function(prior_df, max_observation, feature_number){
  
  prior_df$lp_z_bar_given_all_ys_mu_sig_sq <- rep(NA_real_, nrow(prior_df))
  
  z_given_mu_sig_sq <- lapply(seq(1, max_observation, 1), 
                          function(x){
                            lapply(seq(1, feature_number, 1), 
                                   function(y){
                                     prior_df
                                   })
                          })
  return(z_given_mu_sig_sq)
}