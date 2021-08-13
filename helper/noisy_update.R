## -------------------------------------------------------------
## update the posterior distribution

update_posterior_distribution <- function(grid_theta, 
                                          grid_epsilon, 
                                          observations, 
                                          alpha_prior, 
                                          beta_prior, 
                                          alpha_epsilon, 
                                          beta_epsilon) {
  
  all_observations <- observations %>% 
    ungroup() %>%
    select(-c(trial_num, observation_num)) %>% 
    as.matrix()
  
  trial_num <- observations$trial_num 
  observation_num <- observations$observation_num
  
  updates <- nrow(all_observation)
  
  datalist <- vector(mode = "list", length = updates)
  
  # walk sequntially through updating on each
  for (i in seq(1, updates, 1)) {
    post_first_update_theta_epsilon_approx <- grid_with_theta_and_epsilon(grid_theta = grid_theta, 
                                                                                               grid_epsilon = grid_epsilon, 
                                                                                               noisy_observation = all_observation[1:i, ], 
                                                                                               alpha_prior = alpha_prior, 
                                                                                               beta_prior= beta_prior, 
                                                                                               alpha_epsilon = alpha_epsilon, beta_epsilon = beta_epsilon) %>% 
      mutate(update_number = i) 
    
    
    
    datalist[[i]] <-  post_first_update_theta_epsilon_approx
    
    
    
  }
  
  all_updates <- dplyr::bind_rows(datalist)
  all_updates <- all_updates %>% left_join(tibble(update_number = all_updates %>% 
                                                    distinct(update_number) %>% pull(),
                                                   trial_num = trial_num, 
                                                  observation_num = observation_num), 
                                           by = "update_number")

  return(all_updates)
  
}



 lp_theta_given_z <- function(z_bar, 
                             theta, epsilon, 
                             alpha_theta, beta_theta, 
                             alpha_epsilon, beta_epsilon ) {


   
  lp_z_given_theta(z_bar, theta, epsilon) + 
    lp_theta(theta, alpha_theta, beta_theta) + 
    lp_epsilon(epsilon, alpha_epsilon, beta_epsilon)
}


lp_z_given_theta <- function(z_bar, 
                             theta, 
                             epsilon){
  
    sum(sapply(z_bar[[1]], 
               function(x){lp_z_ij_given_theta(zij = x, 
                                               theta = theta, 
                                               epsilon = epsilon)}))

}


lp_z_ij_given_theta <- function(zij, theta, epsilon){
  
  
  logSumExp(
    c(lp_z_ij_given_y(zij = zij, yi = 1, epsilon = epsilon) + lp_yi_given_theta(yi = 1, theta = theta ), 
      lp_z_ij_given_y(zij = zij, yi = 0, epsilon = epsilon) + lp_yi_given_theta(yi = 0, theta = theta))
  )
  
}


lp_z_ij_given_y <- function(zij, yi, epsilon){
  
  if (zij == yi){
    log(1 - epsilon)
  }else{
    log(epsilon)
  }
}

lp_yi_given_theta <- function(yi, theta){
  # a cooler way to say that if yi = 1 then theta if yi = 0 then yi = 1-theta? 
  dbinom(yi, size = 1, prob = theta, log = TRUE)
}


lp_theta <- function(theta, alpha_theta, beta_theta){
  # actually i think i'm still a little unsure of what the relationship between theta and p(theta) is
  dbeta(x = theta, shape1 = alpha_theta, shape2 = beta_theta, log = TRUE)
}


p_theta <- function(theta, alpha_theta, beta_theta){
  # actually i think i'm still a little unsure of what the relationship between theta and p(theta) is
  dbeta(x = theta, shape1 = alpha_theta, shape2 = beta_theta, log = FALSE)
}

lp_epsilon <- function(epsilon, alpha_epsilon, beta_epsilon){
  dbeta(x = epsilon, shape1 = alpha_epsilon, shape2 = beta_epsilon, log = TRUE)
}



# below are for updates after the first sample when beta distribution is destroyed 
update_lp_theta <- function(theta_value, updated_posterior){
  updated_posterior %>% 
    filter(theta == theta_value) %>% 
    select(normalized_log_posterior) %>% 
    pull()
}

update_lp_theta_given_z_after_observation <- function(new_observation, 
                                                      theta, 
                                                      epsilon, 
                                                      updated_posterior, 
                                                      alpha_epsilon, 
                                                      beta_epsilon){
  
  
  
  #sampling from the updated posterior, which is a broken beta distribution 
  
  new_lp_theta <- update_lp_theta(theta, updated_posterior)
  new_lp_epsilon <- lp_epsilon(epsilon, alpha_epsilon, beta_epsilon)  
  new_lp_z_given_theta <- lp_z_given_theta(new_observation, theta, epsilon)
  
  return (new_lp_theta + new_lp_epsilon + new_lp_z_given_theta)
  
}





# update_alternative_posterior_distribution <- function(grid_theta, 
#                                           grid_epsilon, 
#                                           observations, 
#                                           alternative_observations, 
#                                           alpha_prior, 
#                                           beta_prior, 
#                                           alpha_epsilon, 
#                                           beta_epsilon){
#   
#   
#   
#   all_observaion <- observations %>% 
#     select(-c(trial_num, observation_num)) %>% 
#     as.matrix()
#   
#   all_alternative_observations <- alternative_observations %>% 
#     select(-c(trial_num, observation_num)) %>% 
#     as.matrix()
#   
#   trial_num <- observations$trial_num 
#   observation_num <- observations$observation_num
#   
#   
#   updates = nrow(all_observaion)
#   
#   
#   datalist = list()
#   for (i in seq(1, updates, 1)){
#     
#     
#     
#     alternative_obs_for_update <- rbind(all_observaion[0:(i-1),], 
#                                         all_alternative_observations[i,])
#     
#     
#     
#     post_first_update_theta_epsilon_approx <- grid_with_theta_and_epsilon(grid_theta = grid_theta, 
#                                                                                                grid_epsilon = grid_epsilon, 
#                                                                                                noisy_observation = alternative_obs_for_update[1:i, ], 
#                                                                                                alpha_prior = alpha_prior, 
#                                                                                                beta_prior= beta_prior, 
#                                                                                                alpha_epsilon = alpha_epsilon, beta_epsilon = beta_epsilon) %>% 
#       mutate(update_number = i) 
#     
#     
#     
#     datalist[[i]] <-  post_first_update_theta_epsilon_approx
#     
#     
#     
#   }
#   
#   all_updates <- dplyr::bind_rows(datalist)
#   all_updates <- all_updates %>% left_join(tibble(update_number = all_updates %>% 
#                                                     distinct(update_number) %>% pull(),
#                                                   trial_num = trial_num, 
#                                                   observation_num = observation_num), 
#                                            by = "update_number")
#   
#   return(all_updates)
#   
# }



# 
# update_posterior_distribution_with_epsilon_tracked <- function(grid_theta, 
#                                           grid_epsilon, 
#                                           observations, 
#                                           alpha_prior, 
#                                           beta_prior, 
#                                           alpha_epsilon, 
#                                           beta_epsilon){
#   
#   
#   
#   all_observaion <- observations %>% 
#     select(-c(trial_num, observation_num)) %>% 
#     as.matrix()
#   
#   trial_num <- observations$trial_num 
#   observation_num <- observations$observation_num
#   
#   
#   updates = nrow(all_observaion)
#   
#   
#   datalist = list()
#   for (i in seq(1, updates, 1)){
#     
#     
#     post_first_update_theta_epsilon_approx <- grid_with_theta_and_epsilon_has_epsilon(grid_theta = grid_theta, 
#                                                                                                grid_epsilon = grid_epsilon, 
#                                                                                                noisy_observation = all_observaion[1:i, ], 
#                                                                                                alpha_prior = alpha_prior, 
#                                                                                                beta_prior= beta_prior, 
#                                                                                                alpha_epsilon = alpha_epsilon, beta_epsilon = beta_epsilon) %>% 
#       mutate(update_number = i) 
#     
#     
#     
#     datalist[[i]] <-  post_first_update_theta_epsilon_approx
#     
#     
#     
#   }
#   
#   all_updates <- dplyr::bind_rows(datalist)
#   all_updates <- all_updates %>% left_join(tibble(update_number = all_updates %>% 
#                                                     distinct(update_number) %>% pull(),
#                                                   trial_num = trial_num, 
#                                                   observation_num = observation_num), 
#                                            by = "update_number")
#   
#   return(all_updates)
#   
# }