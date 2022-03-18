
# -- helper function to get range -- # 
get_grid_y_vector <- function(mu_theta, sig_sq_theta, step = 0.2){
  seq(mu_theta - 5*sig_sq_theta, mu_theta + 5*sig_sq_theta, 0.2)
}




# -- score likelihood -- #

score_y_given_miu_sigma_sqr <- function(y_val, miu, sigma){
  dnorm(y_val, mean = miu, sd = sigma, log = TRUE) 
}


score_z_given_y <- function(z_val, y_val, epsilon){
  dnorm(z_val, mean = y_val, sd = epsilon, log = TRUE)
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
