library(matrixStats)
library(LaplacesDemon)
library(tidyverse)
library(doParallel)
library(foreach)

registerDoParallel(cores=32)
getDoParWorkers()

ON_CLUSTER = FALSE

### Set up helper function and paths ------- 

if (ON_CLUSTER){
  source("~/cluster_granch/helper/compute_prob.r") # can't believe it's case sensitive on the server!
  source("~/cluster_granch/helper/embedding_helper.r")
  source("~/cluster_granch/helper/init.r")
  source("~/cluster_granch/helper/granch_main_simulation_helper.R")
  source("~/cluster_granch/helper/granch_sim_setup_helper.R")
 
  embedding_path <- "~/cluster_granch/embeddings/embedding_PCA.csv"
  
  
}else{
  library(here)
  source(here("cluster_granch/helper/compute_prob.r")) # can't believe it's case sensitive on the server!
  source(here("cluster_granch/helper/embedding_helper.r"))
  source(here("cluster_granch/helper/init.r"))
  source(here("cluster_granch/helper/granch_main_simulation_helper.R"))
  source(here("cluster_granch/helper/granch_sim_setup_helper.R"))
  
  embedding_path <- here("cluster_granch/embeddings/embedding_PCA.csv")
  
}


### Set up stimuli ------- 

n_run <- 1
stimuli_pool_size <- 10
n_feature <- 3
stimuli_pool <-  get_stimuli_pool(stimuli_pool_size, n_feature, embedding_path)
sequence_schemes =  c("BBBBBB", "BDBBBB", "BBBDBB", "BBBBBD")


### Set up grid parameters ------- 

# sd_n: range decided by n*sd away 
# sd_step: step decided by sd / sd_step 
grid_mu_theta = get_grid_mu_theta(stimuli_pool, sd_n = 5, sd_step = 10)

# not sure about the principled way to select these three yet
grid_sig_sq = seq(0.001, 2, 0.001) # not sure about the principled way to select this yet
grid_y <- grid_mu_theta 
grid_epsilon = seq(0.001, 1, 0.001)


### Set up priors ------- 

mu_priors = c(0)
mu_priors = c(0)
V_priors = c(0.001)
alpha_priors = c(1) 
beta_priors = c(1) 
epsilons = c(0.000001) 
mu_epsilon = c(0.001)
sd_epsilon = c(2)
world_EIGs = c(0.0001) 
max_observation = 500

model_params <- set_granch_params(
  grid_mu_theta, grid_sig_sq, grid_y, grid_epsilon, hypothetical_obs_grid_n,
  mu_priors, V_priors, 
  alpha_priors, beta_priors, 
  epsilons, mu_epsilon, sd_epsilon,
  world_EIGs, max_observation)


before_prior_calc_t <- Sys.time()
model_params <- add_precalculated_prior_dfs(model_params)
after_prior_calc_t <- Sys.time()

### Set up parameters ------- 

stims_df <- tibble(sequence_scheme = c("BBBBBB"),
                   n_features = n_run
) %>% 
  mutate(
    stimuli_sequence = map(sequence_scheme, function(ss){make_real_stimuli_df(ss, 
                                                                              background = get_bd_pair(stimuli_pool)[1, ], 
                                                                              deviant = get_bd_pair(stimuli_pool)[2, ])}))



full_params_df <- make_simulation_params(n_sim = n_run,
                                         model_params, 
                                         stims_df)


### run model -------
before_sim_t <- Sys.time()
all_sims_res <- full_params_df %>%
  mutate(row_number = row_number()) %>% 
  group_by(row_number) %>% 
  nest() %>%
  mutate(results = map(data,
                       function(df) granch_main_simulation(params = df))) %>%
  unnest(cols = c(data, results))

after_sim_t <- Sys.time()


all_sim_res <- all_sims_res %>% 
  mutate(before_prior_calc_t = before_prior_calc_t, 
         after_prior_calc_t = after_prior_calc_t, 
         before_sim_t = before_sim_t, 
         after_sim_t)

saveRDS(all_sims_res, paste0(after_sim_t, ".RDS"))


