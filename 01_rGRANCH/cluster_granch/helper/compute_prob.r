
# -- get posterior -- #

score_post <- function(lp_z_given_mu_sig_sq,prior_df) {
  
  post_df <- lp_z_given_mu_sig_sq %>% left_join(prior_df, by = c("grid_mu_theta", "grid_sig_sq", "grid_epsilon", "lp_epsilon"))
  # likelihood * prior
  post_df$unnormalized_log_posterior <- post_df$lp_z_given_mu_sig_sq + 
    post_df$lp_mu_sig_sq + 
    post_df$lp_epsilon
  
  # normalize
  post_df$log_posterior <- post_df$unnormalized_log_posterior - matrixStats::logSumExp(post_df$unnormalized_log_posterior)
  post_df$posterior <- exp(post_df$log_posterior)
  
  
  return(post_df)
}


# -- get z_given_mu_sigsq -- #


score_z_given_mu_sig_sq <- function(t, # timestep
                                    f, # feature
                                    df_y_given_mu_sig_sq, # cached likelihoods
                                    ll_z_given_mu_sig_sq, # this is going to be a list of list storing all the relevant info
                                    model) {
  # set up current variables
  this_lp_z_given_mu_sig_sq <- ll_z_given_mu_sig_sq[[t]][[f]]
  grid_epsilon <- unique(this_lp_z_given_mu_sig_sq$grid_epsilon) 
  this_stimulus_idx <- model$stimulus_idx[t]
  f_string <- paste0("f",f)
  
  observations_this_stimulus <- na.omit(pull(model[model$stimulus_idx == this_stimulus_idx, f_string]))
  
  lp_temp <- merge(df_y_given_mu_sig_sq, 
                   this_lp_z_given_mu_sig_sq[c("grid_mu_theta", "grid_sig_sq", "grid_epsilon", "lp_epsilon")])
  
  lp_temp$lp_z_given_mu_sig_sq_for_y <- rowSums(sapply(observations_this_stimulus, score_z_bar_given_y, 
                                                       lp_temp$y, lp_temp$grid_epsilon)) + 
    lp_temp$lp_y_given_mu_sig_sq
  
  this_lp_z_given_mu_sig_sq <- aggregate(lp_z_given_mu_sig_sq_for_y ~ lp_epsilon + grid_epsilon + grid_sig_sq + grid_mu_theta, 
                                              data = lp_temp, FUN = matrixStats::logSumExp)
  names(this_lp_z_given_mu_sig_sq)[names(this_lp_z_given_mu_sig_sq) == 'lp_z_given_mu_sig_sq_for_y'] <- 'lp_z_given_mu_sig_sq'
  
  
  # add in likelihood for last sample from last stimulus, which includes all prior obs
  if (this_stimulus_idx > 1) {
    last_stim_last_t <- max(model$t[model$stimulus_idx == this_stimulus_idx - 1], na.rm=TRUE)
    
    this_lp_z_given_mu_sig_sq$lp_z_given_mu_sig_sq <- this_lp_z_given_mu_sig_sq$lp_z_given_mu_sig_sq + 
      ll_z_given_mu_sig_sq[[last_stim_last_t]][[f]]$lp_z_given_mu_sig_sq
  }
  
  return(this_lp_z_given_mu_sig_sq)
}





# -- get the look up table for y -- # 
get_df_y_given_mu_sig_sq <- function(prior_df, grid_y){
  
  prior_df <- merge(prior_df, grid_y)
  # calculate lp for each y value 
  prior_df$lp_y_given_mu_sig_sq <- mapply(score_y_given_mu_sigma_sq, 
                                          prior_df$y, prior_df$grid_mu_theta, prior_df$grid_sig_sq)
  df_y_given_mu_sig_sq <- prior_df
  
  return(df_y_given_mu_sig_sq)
}


# -- helper function to get range -- # 
get_grid_y_vector <- function(mu_theta, sig_sq_theta, step = 0.2){
  seq(mu_theta - 5*sig_sq_theta, mu_theta + 5*sig_sq_theta, 0.2)
}




# -- score likelihood -- #

score_y_given_mu_sigma_sq <- function(y_val, mu, sigma){
  dnorm(y_val, mean = mu, sd = sigma, log = TRUE) 
}


score_z_ij_given_y <- function(z_val, y_val, epsilon){
  dnorm(z_val, mean = y_val, sd = epsilon, log = TRUE)
}

score_z_bar_given_y <- function(z_bar, y_val, grid_epsilon){
  rowSums(sapply(z_bar, function(x){score_z_ij_given_y(x, y_val, grid_epsilon)}))
}

# --- score prior --- #
# a density function for normal inverse gamma distribution 
score_mu_sig_sq <-function (input_x, input_sig_sq, mu, lambda, alpha, beta, log = TRUE){
  steps <- 500
  max_sig_sq <- qgamma(0.99, alpha, beta) * lambda
  x_range <- c(mu - 5 * max_sig_sq, mu + 5 * max_sig_sq)
  sig_sq_range <- c(0.001, max_sig_sq)
  x <- seq(min(x_range), max(x_range), diff(x_range)/steps)
  sig_sq <- seq(min(sig_sq_range), max(sig_sq_range), diff(sig_sq_range)/steps)
  
  inputs <- expand.grid(x, sig_sq)
  xs <- inputs$Var1
  sig_sqs <- inputs$Var2
  
  output <- sqrt(lambda)/(sqrt(sig_sqs) * sqrt(2 * pi)) * (beta^alpha)/gamma(alpha) * 
    (1/sig_sqs)^(alpha + 1) * exp(-1 * (2 * beta + lambda * 
                                          (xs - mu)^2)/(2 * sig_sqs))
  
  # find the closest value to x
  x_approx <- xs[which.min(abs(xs-input_x))]
  sig_approx <- sig_sqs[which.min(abs(sig_sqs-input_sig_sq))]
  
  dat <- data.frame(x = inputs$Var1, sig_sq = inputs$Var2, 
                    res = output)
  
  if(log){
    return(log(dat[dat$x == x_approx & dat$sig_sq == sig_approx,]$res))
  }else{
    return (dat[dat$x == x_approx & dat$sig_sq == sig_approx,]$res)
  }
  
}

score_epsilon <- function(epsilon, mu_epsilon, sd_epsilon){
  dnorm(x = epsilon, mean = mu_epsilon, sd = sd_epsilon, log = TRUE)
}


get_post_pred <- function(obs, lp_post, df_y_given_mu_sig_sq){

  temp_df <- merge(lp_post, df_y_given_mu_sig_sq)
  temp_df$lp_z_given_mu_sig_sq_for_y = score_z_ij_given_y(z_val = obs, y_val = temp_df$y, epsilon = temp_df$grid_epsilon) +  
    temp_df$lp_y_given_mu_sig_sq
  
  # the order to list out aggregate group matters to get the order of the groupings right
  temp_df <- aggregate(lp_z_given_mu_sig_sq_for_y ~ grid_epsilon + grid_sig_sq
                        + grid_mu_theta, 
                       data = temp_df, FUN = matrixStats::logSumExp)
  
  return(exp(logSumExp(temp_df$lp_z_given_mu_sig_sq + lp_post$log_posterior)))

}

kl_div <- function (x, y) {
  sum(x * log(x/y))
}


# ---------------- rectified_luce_choice ----------------
# this crazy function is necessary because if the values get too close to 0, 
# this can be > 1 or < 0
rectified_luce_choice <- function(x, y) {
  max(min(x / (x + y), 1), 0)
}
