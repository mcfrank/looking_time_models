
main_simulation <- function(
  subject, 
  stimuli_sequence, 
  noise_parameter, 
  eig_from_world = .005,
  max_observation = 500, # should this be per trial or in total? currently in total 
  grid_theta = grid_theta, 
  grid_epsilon = grid_epsilon, 
  alpha_prior = alpha_prior, 
  beta_prior = beta_prior,
  alpha_epsilon = alpha_epsilon, 
  beta_epsilon = beta_epsilon, 
  exposure_type = "forced_short", 
  forced_exposure = TRUE,
  forced_sample = 5,
  optimize = TRUE 
){
  
  # set up the df that tracks eig  
  df <- tibble(t = rep(NA,max_observation),
               stimulus_idx = rep(NA,max_observation), 
               EIG = rep(NA,max_observation), 
               EIG_from_world = rep(eig_from_world,max_observation),
               p_look_away = rep(NA,max_observation), 
               look_away = rep(NA,max_observation))
  
  # set up the df that keep trakc of observation 
  
  observations <- data.frame(matrix(ncol = ncol(stimuli_sequence), 
                                    nrow = max_observation)) %>% 
    tibble()  
  
  colnames(observations) <- colnames(stimuli_sequence)
  
  observations$t <- seq(1, max_observation, 1)
  observations$trial_type <- rep(NA_character_, max_observation)
  observations$trial_number <- rep(NA_integer_, max_observation)
  
  
  # the total number of stimuli 
  total_trial_number = nrow(stimuli_sequence)
  total_feature_number = stimuli_sequence %>% 
    select(starts_with("V")) %>% 
    ncol()
  
  # which stimulus are we looking at
  stimulus_idx <- 1
  t <- 1
  posterior_at_t <- NULL
  
  while(stimulus_idx <= total_trial_number && t <= max_observation){
    
    current_stimulus <- stimuli_sequence[stimulus_idx,]
    
    current_observation <- noisy_observation_creature(
      stimuli_df = stimuli_sequence,
      trial_index  = stimulus_idx, 
      n_sample = 1, 
      epsilon = noise_parameter
    )
    
    # add to current observation 
    observations[observations$t == t, str_detect(names(observations), "V")] <-
      current_observation
    observations[observations$t == t, "trial_type"] <- current_stimulus$trial_type
    observations[observations$t == t, "trial_number"] <- stimulus_idx
    
    
    
    # calculate posterior at t 
    # optimization possible!
    
    posterior_at_t <- grid_apprxoimation_with_observation(
      noisy_observation = observations, 
      track_epsilon = TRUE, 
      grid_theta = grid_theta, 
      grid_epsilon = grid_epsilon, 
      alpha_prior = alpha_prior, 
      beta_prior = beta_prior,
      alpha_epsilon = alpha_epsilon, 
      beta_epsilon = beta_epsilon, 
      optimize = optimize
    )
    
    df$t[t] = t
    df$stimulus_idx[t] = stimulus_idx
    
    
    df$EIG[t] = get_eig_toggle(
      t, 
      current_observation,
      observations, 
      im = "kl", 
      posterior_at_t, 
      grid_theta = grid_theta, 
      grid_epsilon = grid_epsilon, 
      alpha_prior = alpha_prior, 
      beta_prior = beta_prior,
      alpha_epsilon = alpha_epsilon, 
      beta_epsilon = beta_epsilon, 
      optimize = optimize)
    
    # flip a coin with p_keep_looking weight
    df$p_look_away[t] = eig_from_world / (df$EIG[t] + eig_from_world)
    
    # try to force short exposure at the first trial 
    if(forced_exposure){
      
      if(stimulus_idx == 1 && t >= forced_sample){
        df$look_away[t] = TRUE
      }else{
        df$look_away[t] = rbinom(1, 1, prob = df$p_look_away[t]) == 1
      }
    }else{
      
      df$look_away[t] = rbinom(1, 1, prob = df$p_look_away[t]) == 1
    }
    
    
    if (df$look_away[t]==TRUE) {
      stimulus_idx <- stimulus_idx + 1
    }
    
    t <- t + 1 
    
    
    
  }
  
  # maybe needs scaling?
  
  
  
  # df$EIG[t] = get_eig(current_observation, 
  #                observations, 
  #                posterior_at_t, 
  #                grid_theta = grid_theta, 
  #                grid_epsilon = grid_epsilon, 
  #                alpha_prior = alpha_prior, 
  #                beta_prior = beta_prior,
  #                alpha_epsilon = alpha_epsilon, 
  #                beta_epsilon = beta_epsilon)
  
  
  
  
  df$id  <- subject 
  df$forced_sample_n <- forced_sample
  
  
  return(df)
}

noisy_observation_feature <- function(
  feature, 
  n_sample, 
  epsilon 
){
  real_features <- rep(feature, n_sample)
  noisy <- rbernoulli(p = epsilon, n = n_sample)
  return(ifelse(noisy, as.logical(1-real_features), real_features))
  
}

# noisy_observation_creature takes a stimulus dataframe
# take stimulus dataframe and change the particular columns of the 
# selected row according to epsilon
# returns tibble with Vn columns 

noisy_observation_creature <- function(
  stimuli_df,
  trial_index, 
  n_sample = 1, 
  epsilon
) {
  
  
   creature <- stimuli_df %>% 
     filter(trial_number == trial_index) %>% 
     select_at(vars(starts_with("V"))) 
  
  observations <- sapply(creature, function(y) {noisy_observation_feature(
    feature = y, 
    n_sample = n_sample, 
    epsilon = epsilon
  ) %>%  
      as_tibble_row(.name_repair =  make.names)
    
  }) 
  
  rename_column <- function(x){paste0("V", x)}
  tidy_column_names <- lapply(1:ncol(creature), rename_column)
  names(observations) <- tidy_column_names
  
  observations <- observations %>% as_tibble()  %>% unnest(cols = starts_with("V"))
  
  return(observations)
}

update_grid_with_theta_and_epsilon <- function(
  feature_i, 
  grid_theta, 
  grid_epsilon, 
  observations, 
  alpha_theta, beta_theta, 
  alpha_epsilon, beta_epsilon, 
  optimize = TRUE
){
  
  
  samps <- expand_grid(theta = grid_theta,
                       epsilon = grid_epsilon) 
  
  
  samps$unnormalized_log_posterior <- mapply(function(x, y) 
    lp_theta_given_z(z_bar = na.omit(observations), 
                     theta = x, 
                     epsilon = y, 
                     alpha_theta = alpha_theta, 
                     beta_theta = beta_theta,
                     alpha_epsilon = alpha_epsilon, 
                     beta_epsilon = beta_epsilon, 
                     optimize), 
    samps$theta, 
    samps$epsilon)
  
  samps$log_posterior = samps$unnormalized_log_posterior - matrixStats::logSumExp(samps$unnormalized_log_posterior)
  
  
  if(optimize == TRUE){
    samps$posterior <- exp(samps$log_posterior)
    samps$feature_index <- feature_i
    
  }else{
    samps <- samps %>%
      mutate(posterior = exp(log_posterior)) %>% 
      mutate(feature_index = feature_i)
    
  }
  
  
  
  
  return(samps)
  
}


grid_apprxoimation_with_observation <- function(
  noisy_observation, 
  track_epsilon = TRUE, 
  grid_theta = seq(0.01, .99, .01), 
  grid_epsilon = seq(0.01, .99, .01), 
  alpha_prior = 1, 
  beta_prior = 1,
  alpha_epsilon = 10, 
  beta_epsilon = 1, 
  optimize = TRUE
  
){
  
  if(optimize){
    
    posterior_df <- lapply(seq(1, 
                               ncol(noisy_observation[startsWith(names(noisy_observation), 
                                                                 "V")]), 
                               1), 
                           function(x){
                             update_grid_with_theta_and_epsilon(
                               feature_i = x, 
                               grid_theta = grid_theta, 
                               grid_epsilon = grid_epsilon, 
                               observations = noisy_observation[,x], 
                               alpha_theta = alpha_prior, 
                               beta_theta = beta_prior,
                               alpha_epsilon = alpha_epsilon, 
                               beta_epsilon = beta_epsilon, 
                               optimize = optimize
                             )
                           }
    ) %>% 
      bind_rows()
    
    
    
  }else{
    
    total_feature_number = ncol(noisy_observation %>% select(starts_with("V")))
    posterior_df <- lapply(seq(1, 
                               total_feature_number,  
                               1), 
                           function(x){
                             update_grid_with_theta_and_epsilon(
                               feature_i = x, 
                               grid_theta = grid_theta, 
                               grid_epsilon = grid_epsilon, 
                               observations = noisy_observation[,x], 
                               alpha_theta = alpha_prior, 
                               beta_theta = beta_prior,
                               alpha_epsilon = alpha_epsilon, 
                               beta_epsilon = beta_epsilon, 
                               optimize = optimize
                             )
                           }
    ) %>% 
      bind_rows()
    
  }
  
  
  
  if (track_epsilon){
    return (posterior_df)
  }else{
    return (
      
      if(optimize){
        
        posterior_df <- setNames(aggregate(posterior_df$log_posterior,             # Sum by group
                                           by = list(posterior_df$theta),
                                           FUN = matrixStats::logSumExp), 
                                 c("theta", "log_posterior"))
        
        posterior_df$posterior <- exp(pos_new$log_posterior)
        return(as_tibble(pos_new))
        
      }else{
        
        posterior_df <- posterior_df %>% 
          group_by(theta) %>%
          summarise(log_posterior = matrixStats::logSumExp(log_posterior)) %>%
          mutate(posterior = exp(log_posterior))
        
      }
    )
    
    
  }
  
  
}

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
  
  datalist <- list()
  
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
                             alpha_epsilon, beta_epsilon, 
                             optimize = TRUE) {
  
  lp_z_given_theta(z_bar, theta, epsilon, optimize) + 
    lp_theta(theta, alpha_theta, beta_theta) + 
    lp_epsilon(epsilon, alpha_epsilon, beta_epsilon)
}


lp_z_given_theta <- function(z_bar, 
                             theta, 
                             epsilon, 
                             optimize = TRUE){
  
  if(optimize == TRUE){
    
    sum(sapply(z_bar[[1]], 
               function(x){lp_z_ij_given_theta(zij = x, 
                                               theta = theta, 
                                               epsilon = epsilon)}))
  }else{
    
    z_bar = z_bar %>% pull()
    sum(sapply(z_bar, 
               function(x){lp_z_ij_given_theta(zij = x, 
                                               theta = theta, 
                                               epsilon = epsilon)}))
    
  }
  
  
}




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
  posterior_at_t, 
  grid_theta = grid_theta, 
  grid_epsilon = grid_epsilon, 
  alpha_prior = alpha_prior, 
  beta_prior = beta_prior,
  alpha_epsilon = alpha_epsilon, 
  beta_epsilon = beta_epsilon, 
  optimize = TRUE){
  
  
  if(optimize){
    
    
    possible_kls <- tibble(
      "index" = all_possible_outcomes$index, 
      "kl" = rep(NA, length(all_possible_outcomes$index))
    )
    
    all_possible_outcomes <- all_possible_outcomes[startsWith(names(all_possible_outcomes), 
                                                              "V")]
    
    current_scenario <- observations[startsWith(names(observations), 
                                                "V")]
    
    
    for (i in 1: nrow(all_possible_outcomes)){
      
      
      
      
      
      # this is currently a very expensive operation because it requires us to recreate an observation df 
      # every single times and that accumulate very fast 
      # maybe the better way to do it is to create a dataframe upfront and update the last row every single times
      current_scenario[length(na.omit(current_scenario[1]))+1, ] <- all_possible_outcomes[i, ]
      
      
      
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
  
  # pre-optimization: 1260
  # post-optimization: 
  
  
  
  # calculate post predctive for each feature
  feature_predictive <- lapply(seq(1, 
                                   ncol(all_possible_outcomes[startsWith(names(all_possible_outcomes), 
                                                                         "V")]), 
                                   1), 
                               function(x,
                                        observation = all_possible_outcomes[all_possible_outcomes$index == outcome_index,],
                                        posterior = posterior_at_t){
                                 
                                 f_posterior <- posterior[posterior$feature_index == x, ]
                                 
                                 f_observation <- observation[x] 
                                 
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
      posterior_at_t, 
      grid_theta = grid_theta, 
      grid_epsilon = grid_epsilon, 
      alpha_prior = alpha_prior, 
      beta_prior = beta_prior,
      alpha_epsilon = alpha_epsilon, 
      beta_epsilon = beta_epsilon, 
      optimize = optimize)
  }
  
  all_predictives <- get_all_possible_creature_pred(
    all_possible_outcomes, 
    posterior_at_t)
  
  return (all_possible_kls$kl %*% all_predictives)
  
  
}



