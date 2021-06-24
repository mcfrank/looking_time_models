


##########get_kl#############

get_kl <- function (x,y) {
  sum(x * log(x / y)) 
}




#########get_possible_creatures#########

get_possible_creatures <- function(current_observation){
  
  flip_observation <- as.logical(1 - (current_observation) %>% 
                                   as.logical()) %>% 
    as.vector() %>% 
    as_tibble_row(.name_repair = ~ names(current_observation)) 
  
  combine_observations <- bind_rows(current_observation, flip_observation)
  
  all_possible_creatures <- combine_observations %>% 
    cross_df() %>% 
    mutate(index = row_number())
  
  # will return a df of possible creatures 
  return (all_possible_creatures) 
}    


#########get_possible_kls#########

get_possible_kls <- function(
  observations, 
  all_possible_outcomes, 
  posterior_at_t, 
  grid_theta = grid_theta, 
  grid_epsilon = grid_epsilon, 
  alpha_prior = alpha_prior, 
  beta_prior = beta_prior,
  alpha_epsilon = alpha_epsilon, 
  beta_epsilon = beta_epsilon){
  
  all_possible_outcomes <- all_possible_outcomes %>% 
    mutate(index = row_number())
  
  possible_kls <- tibble(
    "index" = all_possible_outcomes$index, 
    "kl" = rep(NA, all_possible_outcomes$index %>% length())
  )
  for (i in 1: nrow(all_possible_outcomes)){
    
    current_scenario <- observations %>% 
      bind_rows(all_possible_outcomes %>% 
                  filter(index == i))
    
    posterior_for_current_scenario <- grid_apprxoimation_with_observation(
      noisy_observation = current_scenario, 
      track_epsilon = TRUE, 
      grid_theta = grid_theta, 
      grid_epsilon = grid_epsilon, 
      alpha_prior = alpha_prior, 
      beta_prior = beta_prior,
      alpha_epsilon = alpha_epsilon, 
      beta_epsilon = beta_epsilon
    )
    
    
    # now calculate kl feature by feature 
    features <- posterior_for_current_scenario %>% 
      distinct(feature_index) %>% 
      pull()
    
    creature_kl <- get_kl(posterior_for_current_scenario$posterior, 
                       posterior_at_t$posterior)
    
    
    # bind creature kl with other kl 
    
    possible_kls$kl[i] <- creature_kl
    
    
  }
  
  
  # will return a df of possible kls 
  return (possible_kls)  
}


get_possible_kls_toggle <- function(
  t, 
  observations, 
  all_possible_outcomes, 
  observation_assumption,
  posterior_at_t, 
  grid_theta = grid_theta, 
  grid_epsilon = grid_epsilon, 
  alpha_prior = alpha_prior, 
  beta_prior = beta_prior,
  alpha_epsilon = alpha_epsilon, 
  beta_epsilon = beta_epsilon, 
  optimize = TRUE){
  
  
  if(optimize){
    
    #create an empty row at the bottom to be modified
    num_observation <- nrow(observations)
    current_scenario <- bind_rows(observations[startsWith(names(observations), 
                                                           "V")], tibble_row())
    
    
    
    possible_kls <- tibble(
      "index" = all_possible_outcomes$index, 
      "kl" = rep(NA, length(all_possible_outcomes$index))
    )
    
    all_possible_outcomes <- all_possible_outcomes[startsWith(names(all_possible_outcomes), 
                                                              "V")]
    for (i in 1: nrow(all_possible_outcomes)){
      
      
      if (observation_assumption == "independent"){
        
        
        # this is currently a very expensive operation because it requires us to recreate an observation df 
        # every single times and that accumulate very fast 
        # maybe the better way to do it is to create a dataframe upfront and update the last row every single times
        current_scenario[num_observation+1, ] <- all_possible_outcomes[i, ]
          
          
        
        posterior_for_current_scenario <- grid_apprxoimation_with_observation(
          noisy_observation = current_scenario, 
          track_epsilon = TRUE, 
          grid_theta = grid_theta, 
          grid_epsilon = grid_epsilon, 
          alpha_prior = alpha_prior, 
          beta_prior = beta_prior,
          alpha_epsilon = alpha_epsilon, 
          beta_epsilon = beta_epsilon
        )
        
      }else{
        
        
        current_scenario <- all_possible_outcomes[i, ]
        
        posterior_for_current_scenario <- faster_grid_apprxoimation_with_observation(
          t, 
          noisy_observation = current_scenario, 
          last_update_posterior_df = posterior_at_t,
          track_epsilon = TRUE, 
          grid_theta = grid_theta, 
          grid_epsilon = grid_epsilon, 
          alpha_prior = alpha_prior, 
          beta_prior = beta_prior,
          alpha_epsilon = alpha_epsilon, 
          beta_epsilon = beta_epsilon
        )
        
        
      }
  
      
      creature_kl <- get_kl(posterior_for_current_scenario$posterior, 
                            posterior_at_t$posterior)
      
      
      # bind creature kl with other kl 
      
      possible_kls$kl[i] <- creature_kl
    }
    
    
    
  }else{
    
    all_possible_outcomes <- all_possible_outcomes %>% 
      mutate(index = row_number())
    
    possible_kls <- tibble(
      "index" = all_possible_outcomes$index, 
      "kl" = rep(NA, all_possible_outcomes$index %>% length())
    )
    for (i in 1: nrow(all_possible_outcomes)){
      
      
      if (observation_assumption == "independent"){
        
        current_scenario <- observations %>% 
          bind_rows(all_possible_outcomes %>% 
                      filter(index == i))
        
        
        posterior_for_current_scenario <- grid_apprxoimation_with_observation(
          noisy_observation = current_scenario, 
          track_epsilon = TRUE, 
          grid_theta = grid_theta, 
          grid_epsilon = grid_epsilon, 
          alpha_prior = alpha_prior, 
          beta_prior = beta_prior,
          alpha_epsilon = alpha_epsilon, 
          beta_epsilon = beta_epsilon
        )
        
      }else{
        
        
        current_scenario <- all_possible_outcomes %>% 
          filter(index == i)
        
        posterior_for_current_scenario <- faster_grid_apprxoimation_with_observation(
          t, 
          noisy_observation = current_scenario, 
          last_update_posterior_df = posterior_at_t,
          track_epsilon = TRUE, 
          grid_theta = grid_theta, 
          grid_epsilon = grid_epsilon, 
          alpha_prior = alpha_prior, 
          beta_prior = beta_prior,
          alpha_epsilon = alpha_epsilon, 
          beta_epsilon = beta_epsilon
        )
        
      }
      
      
      
      
      # now calculate kl feature by feature 
      features <- posterior_for_current_scenario %>% 
        distinct(feature_index) %>% 
        pull()
      
      creature_kl <- get_kl(posterior_for_current_scenario$posterior, 
                            posterior_at_t$posterior)
      
      
      # bind creature kl with other kl 
      
      possible_kls$kl[i] <- creature_kl
      
      
    }
    
    
  }
  
  
  
  
  # will return a df of possible kls 
  return (possible_kls)  
}


#########noisy_post_pred for single creature#########

noisy_post_pred <- function(theta, epsilon, posterior, heads = TRUE) {
  p_1 <- sum(((1 - epsilon) * theta * posterior) + 
               (epsilon * (1-theta) * posterior))
  
  ifelse(heads, p_1, 1 - p_1)
  
}

#########noisy_post_pred for entire creature#########


creature_noisy_post_pred <- function(
  outcome_index, 
  all_possible_outcomes, 
  posterior_at_t){
  
  observation_at_t_plus_one <- all_possible_outcomes %>% 
    filter(index == outcome_index)
  
  feature_n <- observation_at_t_plus_one %>% 
    select(starts_with("V")) %>% 
    ncol()
  
  
  
  # calculate post predctive for each feature
  feature_predictive <- lapply(seq(1, feature_n, 1), 
                               function(x,
                                        observation = observation_at_t_plus_one,
                                        posterior = posterior_at_t){
                                 
                                 f_posterior <- posterior %>% 
                                   filter(feature_index == x)
                                 
                                 f_observation <- observation[x] %>% 
                                   pull()
                                 
                                 noisy_post_pred(f_posterior$theta, 
                                                 f_posterior$epsilon, 
                                                 f_posterior$posterior, 
                                                 f_observation)
                                 
                               }) 
  
  return(feature_predictive %>% unlist() %>% prod())
  
  
}


###########get_all_possible_creature_pred###########

get_all_possible_creature_pred <- function(
  all_possible_outcomes, 
  posterior_at_t
){
  
  
  all_preds <- lapply(all_possible_outcomes$index, 
                      function(x, 
                               apo = all_possible_outcomes, 
                               pt = posterior_at_t){
                        
                        creature_noisy_post_pred(x, apo, pt)
                        
                        
                      }) %>% unlist()
  
  return(all_preds %>% unlist())
  
}


#########get_eig#########

get_eig <- function(current_observation, 
                    observations, 
                    posterior_at_t, 
                    grid_theta = grid_theta, 
                    grid_epsilon = grid_epsilon, 
                    alpha_prior = alpha_prior, 
                    beta_prior = beta_prior,
                    alpha_epsilon = alpha_epsilon, 
                    beta_epsilon = beta_epsilon){
  
  all_possible_outcomes <- get_possible_creatures(current_observation)
  all_possible_kls <- get_possible_kls(
    observations, 
    all_possible_outcomes, 
    posterior_at_t, 
    grid_theta = grid_theta, 
    grid_epsilon = grid_epsilon, 
    alpha_prior = alpha_prior, 
    beta_prior = beta_prior,
    alpha_epsilon = alpha_epsilon, 
    beta_epsilon = beta_epsilon)
  
  all_predictives <- get_all_possible_creature_pred(
    all_possible_outcomes, 
    posterior_at_t)
  
  return (all_possible_kls$kl %*% all_predictives)
  
  
}




get_eig_toggle <- function(
                    t, 
                    current_observation, 
                    observations, 
                    observation_assumption, 
                    im, 
                    posterior_at_t, 
                    grid_theta = grid_theta, 
                    grid_epsilon = grid_epsilon, 
                    alpha_prior = alpha_prior, 
                    beta_prior = beta_prior,
                    alpha_epsilon = alpha_epsilon, 
                    beta_epsilon = beta_epsilon, 
                    optimize = TRUE){
  
  all_possible_outcomes <- get_possible_creatures(current_observation)
  
  if (im == "kl"){
        all_possible_kls <- get_possible_kls_toggle(
          t, 
          observations, 
          all_possible_outcomes,
          observation_assumption,
          posterior_at_t, 
          grid_theta = grid_theta, 
          grid_epsilon = grid_epsilon, 
          alpha_prior = alpha_prior, 
          beta_prior = beta_prior,
          alpha_epsilon = alpha_epsilon, 
          beta_epsilon = beta_epsilon, 
          optimize)
  }
  
  all_predictives <- get_all_possible_creature_pred(
    all_possible_outcomes, 
    posterior_at_t)
  
  return (all_possible_kls$kl %*% all_predictives)
  
  
}