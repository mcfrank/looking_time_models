library(reshape2)
library(tidyverse)
library(here)
library(matrixStats)

## source relevant files
source(here("adult_modeling/scripts/get_stimuli_and_observations.R"))
source(here("adult_modeling/scripts/get_entropy.R"))
source(here("adult_modeling/scripts/get_KL_measurement_with_epsilon.R"))
source(here("adult_modeling/scripts/get_surprise.R"))
source(here("adult_modeling/scripts/noisy_update.R"))
source(here("adult_modeling/scripts/grid_approximation.R"))

max_feature_num = 10
num_features_simple = 3
num_features_complex = 8
trials_per_block = 8
deviant_positions = c(3,5)
dissimilarity_ratio = 0.2

t_simple <- generate_creature_sequence(
  block_length = trials_per_block, 
  deviant_positions = deviant_positions,  # takes a vector, 
  total_feature = max_feature_num, 
  feature_theta = 0.8, 
  feature_number = num_features_simple, 
  dissimilar_ratio = dissimilarity_ratio)

obs <- generate_noisy_observations(
  block = t_simple, 
  exposure_type = "self_paced", 
  short_exposure_samps = 1, 
  long_exposure_samps = 10, 
  normal_exposure_samps = 10, 
  epsilon = 0.02)    

obs_1 <-  readRDS(here("adult_modeling/m_res/obs_1"))


# generate the necessary df: 
df_posterior <- update_posterior_distribution_with_epsilon_tracked(
  grid_theta = seq(0.1, .99, .1),
  grid_epsilon = seq(0.1, .99, .1),
  obs,
  alpha_prior = 1,
  beta_prior = 5,
  alpha_epsilon = 1,
  beta_epsilon = 10)

saveRDS(df_posterior, here("adult_modeling/m_res/s_a1b5_posterior_df.rds"))

df_posterior_kl <- get_kl_for_creature_with_epsilon(df_posterior)

saveRDS(df_posterior_kl, here("adult_modeling/m_res/s_a1b5_posterior_df.rds"))



