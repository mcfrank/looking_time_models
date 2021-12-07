# this function calculate the KL distance between the prior beta distribution and observing ONE 
# e.g: if prior_alpha = 1, prior_beta = 2, obs = FALSE, the D_KL is between a1b2 and a1b3


get_kl_after_obs <- function(grid_step, prior_alpha, prior_beta, obs = FALSE){
  
  prior <- dbeta(seq(0.01, 0.99, grid_step), prior_alpha, prior_beta) / sum(dbeta(seq(0.01, 0.99, grid_step), 
                                                                             prior_alpha, 
                                                                             prior_beta))
  
  if(obs){
    post_alpha = prior_alpha + 1
    post_beta = prior_beta
  }else{
    post_alpha = prior_alpha 
    post_beta = prior_beta + 1
  }
  
  post <- dbeta(seq(0.01, 0.99, grid_step), post_alpha, post_beta) / sum(dbeta(seq(0.01, 0.99, grid_step), 
                                                                          post_alpha, 
                                                                          post_beta))
  
  return(philentropy::KL(rbind( post, prior), unit = "log"))
  
}


# this calculate the jeffereys divergence (symmetrized version of the distribution)
get_jkl_after_obs <- function(grid_step, prior_alpha, prior_beta, obs = FALSE){
  
  prior <- dbeta(seq(0.01, 0.99, grid_step), prior_alpha, prior_beta) / sum(dbeta(seq(0.01, 0.99, grid_step), 
                                                                             prior_alpha, 
                                                                             prior_beta))
  
  if(obs){
    post_alpha = prior_alpha + 1
    post_beta = prior_beta
  }else{
    post_alpha = prior_alpha 
    post_beta = prior_beta + 1
  }
  
  post <- dbeta(seq(0.01, 0.99, grid_step), post_alpha, post_beta) / sum(dbeta(seq(0.01, 0.99, grid_step), 
                                                                          post_alpha, 
                                                                          post_beta))
  
  return(philentropy::KL(rbind(prior, post), unit = "log") + philentropy::KL(rbind(post,prior), unit = "log"))
  
}