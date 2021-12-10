library(matrixStats)
#library(partitions) <- not downloaded on the server 
library(LaplacesDemon)
library(tidyverse)
library(doParallel)
library(foreach)

registerDoParallel(cores=16)
getDoParWorkers()


source("~/poke_model/helper/make_scheme_and_params.r") # can't believe it's case sensitive on the server!
source("~/poke_model/helper/initialization.r")
source("~/poke_model/helper/probability_computations.R")
source("~/poke_model/helper/main_simulation_helper.R")


alpha_epsilons = c(1)
beta_epsilons = c(10)
alpha_priors = c(1)
beta_priors = c(30)
alpha_priors = c(1)
beta_priors = c(30)
noise_parameters = c(0.01, 0.05)
world_EIGs = c(0.005, 0.1)
max_observation = c(1500)

model_params <- set_model_params(alpha_priors, beta_priors,
                                 alpha_epsilons, beta_epsilons,
                                 noise_parameters, world_EIGs, max_observation)


# set stimuli-related parameters
features_df <- tibble(
  n_features = c(6, 6),
  on_features_n = c(1, 3)
)
sequence_scheme = c("BBBBDB")

stims_df <- set_stim_params(sequence_scheme, features_df)


full_params_df <- make_simulation_params(n_sim = 100,
                                         model_params,
                                         stims_df)


all_sims_params <- full_params_df %>%
  mutate(row_number = row_number()) %>% 
  group_by(row_number) %>% 
  nest() 


all_res <- foreach(i = 1:(max(all_sims_params$row_number)), .combine=rbind, .errorhandling = "remove") %dopar% {
  
  res <- all_sims_params[i, ] %>% 
    mutate(res = main_simulation(params = (all_sims_params[i, ]$data)[[1]]) %>% nest(res = everything()))
    
  sprintf("currently running task %s out of %s tasks", as.character(i), as.character(max(all_sims_params$row_number))) 
} 
  

lk_table <- all_res %>% 
  unnest(data) %>% 
  mutate(
    params_info = paste0("ep_", noise_parameter, "_wEIG_", # this needs to be adapted to recognizing changed parameters
                         world_EIG, "_of_", on_features_n)  
  ) %>% 
  select(row_number, params_info)

tidy_sim <- all_res %>% 
  ungroup() %>% 
  select(row_number, res) %>% 
  unnest(cols = c(res)) %>% 
  unnest(res) %>% 
  select(!starts_with("f")) %>% 
  left_join(lk_table, by = "row_number") %>% 
  group_by(params_info, stimulus_idx) %>% 
  summarise(sample_n = n())


saveRDS(tidy_sim, "tidy_sim_res.RDS")

