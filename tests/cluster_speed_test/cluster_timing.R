library(tidyverse)
library(here)
library(matrixStats)
library(partitions)
library(tictoc)
library(LaplacesDemon)
library(Brobdingnag)
library(doParallel)
library(foreach)



registerDoParallel(cores=32)
getDoParWorkers()

source("~/poke_model/cluster_speed_test/forced_helper/initialization.R") 
source("~/poke_model/cluster_speed_test/forced_helper/probability_computations.R")

source("~/poke_model/cluster_speed_test/forced_helper/make_scheme_and_params.r")
source("~/poke_model/cluster_speed_test/old_helper/make_scheme_and_params.r")
source("~/poke_model/cluster_speed_test/forced_helper/main_simulation_helper.R")
source("~/poke_model/cluster_speed_test/old_helper/main_simulation_helper.R")


alpha_epsilons = c(1)
beta_epsilons = c(4)
alpha_priors = c(1)
beta_priors = c(10)
noise_parameters = c(0.065)
world_EIGs = c(0.01)
forced_exposure_n = c(0)

# set model-related parameters 
alpha_epsilons = c(1)
beta_epsilons = c(4)
alpha_priors = c(1)
beta_priors = c(10)
noise_parameters = c(0.065)
world_EIGs = c(0.01)
forced_exposure_n = c(30)
max_observation = 2000


forced_model_params <- forced_set_model_params(alpha_priors, beta_priors, 
                                               alpha_epsilons, beta_epsilons, 
                                               noise_parameters, world_EIGs, max_observation, forced_exposure_n)
old_model_params <- old_set_model_params(alpha_priors, beta_priors, 
                                         alpha_epsilons, beta_epsilons, 
                                         noise_parameters, world_EIGs, max_observation)


# set stimuli-related parameters 
features_df <- tibble(
  n_features = c(1, 3, 5), 
  on_features_n = c(1)
)
sequence_scheme = c("BBB")


stims_df <- set_stim_params(sequence_scheme, features_df)


forced_full_params_df <- make_simulation_params(n_sim = 1,
                                                forced_model_params, 
                                                stims_df)
old_full_params_df <- make_simulation_params(n_sim = 1,
                                             old_model_params, 
                                             stims_df)


estimate_time <- function(run_n, 
                          forced_df, 
                          original_df){
  
  
  n_features <- tibble(n_feature = 
                         forced_df$n_features)
  
  run_t_df <- tibble(
    sim_id = 1:run_n,
    forced_t = rep(NA_real_, run_n), 
    original_t = rep(NA_real_, run_n)
  )
  
  for(nf in n_features$n_feature){
    run_t_df_for_feature_n <- run_t_df %>% 
      mutate(n_feature = nf)
    
    for(i in 1:run_n){
      
      t_forced <- system.time({all_sims_res <- forced_df %>%
        filter(n_features == nf) %>% 
        mutate(row_number = row_number()) %>% 
        group_by(row_number) %>% 
        nest() %>%
        mutate(results = map(data,
                             function(df) main_simulation_forced(params = df))) %>%
        unnest(cols = c(data, results))})
      
      t_original <- system.time({all_sims_res <- original_df %>%
        filter(n_features == nf) %>% 
        mutate(row_number = row_number()) %>% 
        group_by(row_number) %>% 
        nest() %>%
        mutate(results = map(data,
                             function(df) main_simulation_original(params = df))) %>%
        unnest(cols = c(data, results))})
      
      # the third element of the return object is time elapsed
      run_t_df_for_feature_n$forced_t[[i]] <- t_forced[[3]]
      run_t_df_for_feature_n$original_t[[i]] <- t_original[[3]] 
    }
    run_t_df <- bind_rows(run_t_df, run_t_df_for_feature_n)
    
  }
  return (run_t_df)
  
}

estimate_time_df <- estimate_time(run_n = 100, 
                                  forced_df = forced_full_params_df, 
                                  original_df = old_full_params_df)


saveRDS(estimate_time_df, "time_estimate.RDS")
