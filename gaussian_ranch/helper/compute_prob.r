
# -- get posterior -- #

score_post <- function(lp_z_given_mu_sig_sq,prior_df) {
  
  post_df <- lp_z_given_mu_sig_sq %>% left_join(prior_df, by = c("grid_mu_theta", "grid_sig_sq", "grid_epsilon"))
  # likelihood * prior
  post_df$unnormalized_log_posterior <- post_df$lp_z_given_mu_sig_sq + 
    post_df$lp_mu_sig_sq + 
    post_df$lp_epsilon # currently missing
  
  # normalize
  post_df$log_posterior <- post_df$unnormalized_log_posterior - matrixStats::logSumExp(post_df$unnormalized_log_posterior)
  post_df$posterior <- exp(post_df$log_posterior)
  
  
  return(post_df)
}


# -- get z_given_mu_sigsq -- #
score_z_given_mu_sig_sq <- function(t, # timestep
                                f, # feature
                                df_y_given_mu_sig_sqr, # cached likelihoods
                                ll_z_given_mu_sig_sq, # this is going to be a list of list storing all the relevant info
                                model) {
  
  # set up current variables
  this_lp_z_given_mu_sig_sq <- ll_z_given_mu_sig_sq[[t]][[f]]
  grid_epsilon <- unique(this_lp_z_given_mu_sig_sq$grid_epsilon) # currently not implemented 
  this_stimulus_idx <- model$stimulus_idx[t]
  
  # need to compute over all noisy observations of this stimulus
  observations_this_stimulus <- filter(model, stimulus_idx == this_stimulus_idx) %>%
    select(paste0("f", f)) %>%
    pull()
  
  # initialize log p(z|y)
  lp_temp <- df_y_given_mu_sig_sq %>% left_join(this_lp_z_given_mu_sig_sq %>% 
                                                  select(grid_mu_theta, grid_sig_sq, grid_epsilon, lp_epsilon), 
                                                by = c("grid_mu_theta", "grid_sig_sq"))
  
  # OPTIMIZATION POSSIBLE: 
  # - we are going to have a lot of overlapping y values 
  # - calculate only unique y values
  # - but need to keep track of which row each element comes from 
 
  # this creates a matrix, that has (# of y value) columns and (# of epsilon value) rows
  m_lp_z_bar_given_this_y_vavlue <- sapply(df_y_given_mu_sig_sq$grid_y, function(y){
    score_z_bar_given_y(observations_this_stimulus, y, grid_epsilon)
  })
  
  lp_temp$lp_z_given_y <- sapply(observations_this_stimulus, score_z_bar_given_y, lp_temp$grid_y, lp_temp$grid_epsilon)
  
  lp_temp$lp_z_given_mu_sig_sq_for_y <- lp_temp$lp_z_given_y + lp_temp$lp_y_given_mu_sig_sq
 

  # adding together all the possible y values for each pair of mu and sig sq 
  this_lp_z_given_mu_sig_sq <- lp_temp %>% 
    group_by(grid_mu_theta, grid_sig_sq, grid_epsilon) %>% 
    summarise(lp_z_given_mu_sig_sq = matrixStats::logSumExp(lp_z_given_mu_sig_sq_for_y))
  # note that the extremely negative values will become -Inf If used regular log(sum(exp)), but the remaining values are the same
  
  
  # add in likelihood for last sample from last stimulus, which includes all prior obs
  if (this_stimulus_idx > 1) {
    last_stim_last_t <- max(model$t[model$stimulus_idx == this_stimulus_idx - 1], na.rm=TRUE)
    this_lp_z_given_mu_sig_sq$lp_z_given_mu_sig_sq_for_y <- this_lp_z_given_theta$lp_z_given_mu_sig_sq_for_y + 
      lp_z_given_mu_sig_sq[[last_stim_last_t]][[f]]$lp_z_given_mu_sig_sq_for_y
  }
  
  return(this_lp_z_given_mu_sig_sq)
}






# -- get the look up table for y -- # 
get_df_y_given_mu_sig_sq <- function(prior_df){
  # first get a list of all the ranges 
  grid_y_list <- mapply(get_grid_y_vector, prior_df$grid_mu_theta, prior_df$grid_sig_sq, step = 0.2)
  # calculate how many values in each range
  prior_df$grid_y_length <- sapply(grid_y_list, function(x){length(x)})
  # repeat each row so that we can align the y value  
  prior_df <- prior_df[rep(row.names(prior_df),prior_df $grid_y_length), 1:3]
  assertthat::assert_that(nrow(prior_df) == length(unlist(grid_y_list)))
  # put all the y values in the df 
  prior_df$grid_y <- unlist(grid_y_list)
  # calculate lp for each y value 
  prior_df$lp_y_given_mu_sig_sq <- mapply(score_y_given_mu_sigma_sq, 
                                           prior_df$grid_y, prior_df$grid_mu_theta, prior_df$grid_sig_sq)
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
                    res = out)
  
  if(log){
    return(log(dat[dat$x == x_approx & dat$sig_sq == sig_approx,]$res))
  }else{
    return (dat[dat$x == x_approx & dat$sig_sq == sig_approx,]$res)
  }
  
}

score_epsilon <- function(epsilon, mu_epsilon, sd_epsilon){
  dnorm(x = epsilon, mean = mu_epsilon, sd = sd_epsilon, log = TRUE)
}


get_post_pred <- function(obs, lp_post, df_y_given_mu_sig_sq) {
 
  # this is to make sure we have the right permutation of y to mu sig sq etc...
  temp_df <- left_join(lp_post, df_y_given_mu_sig_sq)
  temp_df$lp_z_given_y = score_z_ij_given_y(z_val = obs, y_val = temp_df$grid_y, epsilon = temp_df$grid_epsilon)
  
  return(exp(logSumExp(temp_df$lp_z_given_y +  temp_df$lp_y_given_mu_sig_sq + temp_df$log_posterior)))
  
}

kl_div <- function (x, y) {
  sum(x * log(x/y))
}
