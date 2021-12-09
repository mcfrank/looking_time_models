library(matrixStats)
#library(partitions) <- not downloaded on the server 
library(LaplacesDemon)
library(tidyverse)


source("~/poke_model/helper/make_scheme_and_params.r") # can't believe it's case sensitive on the server!
source("~/poke_model/helper/initialization.r")
source("~/poke_model/helper/probability_computations.R")
source("~/poke_model/helper/main_simulation_helper.R")


alpha_epsilons = c(1)
beta_epsilons = c(10)
alpha_priors = c(1,10,30,50)
beta_priors = c(1,10,30, 50)
noise_parameters = c(0.01)
world_EIGs = c(0.005)
max_observation = c(800)

model_params <- set_model_params(alpha_priors, beta_priors, 
                                 alpha_epsilons, beta_epsilons, 
                                 noise_parameters, world_EIGs, max_observation)


# set stimuli-related parameters 
features_df <- tibble(
  n_features = c(3, 3), 
  on_features_n = c(1, 2)
)
sequence_scheme = c("BBBBDB")

stims_df <- set_stim_params(sequence_scheme, features_df)


full_params_df <- make_simulation_params(n_sim = 100,
                                         model_params, 
                                         stims_df)


all_sims_res <- full_params_df %>%
  mutate(row_number = row_number()) %>% 
  group_by(row_number) %>% 
  nest() %>%
  mutate(results = map(data,
                       function(df) main_simulation(params = df))) %>%
  unnest(cols = c(data, results))


saveRDS(all_sims_res, Sys.time() + "_sim_res.RDS")

