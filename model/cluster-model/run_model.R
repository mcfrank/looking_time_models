library(matrixStats)
#library(partitions) <- not downloaded on the server 
library(LaplacesDemon)
library(tidyverse)
library(doParallel)
library(foreach)

registerDoParallel(cores=32)
getDoParWorkers()

ON_CLUSTER = FALSE

if (ON_CLUSTER){
  source("~/poke_model/helper/make_scheme_and_params.r") # can't believe it's case sensitive on the server!
  source("~/poke_model/helper/initialization.r")
  source("~/poke_model/helper/probability_computations.R")
  source("~/poke_model/helper/main_simulation_helper.R")
  
  # prior search upper bound 
  ps_ub = 3
  # prior search lower bound
  ps_lb = 1
  # prior search step 
  ps_s = 0.5
  alpha_epsilons = seq(ps_lb, ps_ub, ps_s)
  beta_epsilons = seq(ps_lb, ps_ub, ps_s)
  alpha_priors = seq(ps_lb, ps_ub, ps_s)
  beta_priors = seq(ps_lb, ps_ub, ps_s)
  
  # noise search upper bound 
  ns_ub = 0.08
  # noise search lower bound 
  ns_lb = 0.01
  # noise search step
  ns_s = 0.05
  noise_parameters = seq(ns_lb, ns_ub, ns_s)
  
  # wEIG upper bound 
  weig_ub = 0.005
  # wEIG lower bound
  weig_lb = 0.001
  # wEIG 
  weig_s = 0.001
  
  weig_parameters = seq(weig_lb, weig_ub, weig_s)
  
  # delete laterr when i actually know how to do paremeter search 
  alpha_epsilons = c(1)
  beta_epsilons = c(10)
  alpha_priors = c(1)
  beta_priors = c(30)
  alpha_priors = c(1)
  beta_priors = c(30)
  noise_parameters = c(0.01, 0.05)
  world_EIGs = c(0.005, 0.01)
  
  
}else{
  source(here("helper/make_scheme_and_params.r"))
  source(here("helper/initialization.r"))
  source(here("helper/probability_computations.R"))
  source(here("helper/main_simulation_helper.R"))
  
  alpha_epsilons = c(1)
  beta_epsilons = c(10)
  alpha_priors = c(1)
  beta_priors = c(30)
  alpha_priors = c(1)
  beta_priors = c(30)
  noise_parameters = c(0.01, 0.05)
  world_EIGs = c(0.005, 0.1)
  
}


max_observation = c(3000) # some combination of the prams are going to end up having very high observation




model_params <- set_model_params(alpha_priors, beta_priors,
                                 alpha_epsilons, beta_epsilons,
                                 noise_parameters, world_EIGs, max_observation) %>% 
  filter(alpha_prior < beta_prior, alpha_epsilon < beta_epsilon) %>% 
  mutate(params_id = row_number() 
  )


# set stimuli-related parameters
features_df <- tibble(
  n_features = c(6, 1),
  on_features_n = c(6, 3)
)
sequence_scheme = c("BBBBBB", "BDBBBB", "BBBDBB", "BBBBBD")

stims_df <- set_stim_params(sequence_scheme, features_df)


full_params_df <- make_simulation_params(n_sim = 200,
                                         model_params,
                                         stims_df)


all_sims_params <- full_params_df %>%
  mutate(sim_id = row_number()) %>% 
  group_by(sim_id,stim_info, params_info) %>% 
  nest() 



all_res <- foreach(i = 1:(max(all_sims_params$sim_id)), .combine=rbind, .errorhandling = "remove") %dopar% {
  
  res <- all_sims_params[i, ] %>% 
    mutate(res = main_simulation(params = (all_sims_params[i, ]$data)[[1]]) %>% nest(res = everything()))
  
} 

# this indirect approach is to prevent weird unnesting behavior 
sim_res <- all_res$res %>% 
  mutate(sim_id = row_number()) %>% 
  unnest(res) %>% 
  group_by(sim_id, stimulus_idx) %>% 
  summarise(sample_n = n()) %>% 
  filter(!is.na(stimulus_idx))

tidy_sim <- left_join(sim_res, 
                      all_res %>% select(-c(data, res)), 
                      by = "sim_id")




saveRDS(tidy_sim, "tidy_sim.RDS")

