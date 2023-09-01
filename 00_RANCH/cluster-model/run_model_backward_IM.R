library(matrixStats)
#library(partitions) <- not downloaded on the server 
library(LaplacesDemon)
library(tidyverse)
library(doParallel)
library(foreach)

registerDoParallel(cores=32)
getDoParWorkers()

ON_CLUSTER = TRUE


if (ON_CLUSTER){
  source("~/poke_model/helper/make_scheme_and_params.r") # can't believe it's case sensitive on the server!
  source("~/poke_model/helper/initialization.r")
  source("~/poke_model/helper/probability_computations.R")
  source("~/poke_model/helper/main_simulation_helper.R")
  source("~/poke_model/helper/backward_IM_main_simulation_helper.R")

  
  # delete laterr when i actually know how to do paremeter search 
  if(im_type == "KL"){
    
    alpha_epsilons = c(1)
    beta_epsilons = c(10)
    alpha_priors = c(1)
    beta_priors = c(5)
    noise_parameters = c(0.055)
    world_EIGs = c(0.006)
    forced_exposure_n = c(1, 300)
    
  }else if(im_type == "surprisal"){
    
    alpha_epsilons = c(1)
    beta_epsilons = c(10)
    alpha_priors = c(1)
    beta_priors = c(3)
    noise_parameters = c(0.07)
    world_EIGs = c(8)
    forced_exposure_n = c(1, 300)
    
  }
  
  
}else{
  source(here("helper/make_scheme_and_params.r"))
  source(here("helper/initialization.r"))
  source(here("helper/probability_computations.R"))
  source(here("helper/main_simulation_helper.R"))
  source(here("helper/backward_IM_main_simulation_helper.R"))
  
  im_type = "KL"
  
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
  n_features = c(3),
  on_features_n = c(1)
)
sequence_scheme = c("BBBBBB", "BDBBBB", "BBBDBB", "BBBBBD")

stims_df <- set_stim_params(sequence_scheme, features_df)


full_params_df <- make_simulation_params(n_sim = 500,
                                         model_params,
                                         stims_df)


all_sims_params <- full_params_df %>%
  mutate(sim_id = row_number(), 
         measurement = im_type) %>% 
  group_by(sim_id,stim_info, params_info) %>% 
  nest() 


all_res <- foreach(i = 1:(max(all_sims_params$sim_id)), .combine=rbind, .errorhandling = "remove") %dopar% {
  
  res <- all_sims_params[i, ] %>% 
    mutate(res = backward_IM_main_simulation(params = (all_sims_params[i, ]$data)[[1]]) %>% nest(res = everything()))
  
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




saveRDS(tidy_sim, "KL_tidy_sim.RDS")

