library(matrixStats)
library(LaplacesDemon)
library(tidyverse)
library(doParallel)
library(foreach)

registerDoParallel(cores=32)
getDoParWorkers()

ON_CLUSTER = TRUE

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
stimuli_pool_size <- 100
n_feature <- 3
stimuli_pool <-  get_stimuli_pool(stimuli_pool_size, n_feature, embedding_path)
sequence_schemes =  c("BBBBBB", "BDBBBB", "BBBDBB", "BBBBBD")
hypothetical_obs_grid_n <- 3

### Set up grid parameters ------- 

# sd_n: range decided by n*sd away 
# sd_step: step decided by sd / sd_step 
grid_mu_theta = get_grid_mu_theta(stimuli_pool, sd_n = 5, sd_step = 3)

# not sure about the principled way to select these three yet
grid_sig_sq = seq(0.01, 1, 0.1) # not sure about the principled way to select this yet
grid_y <- grid_mu_theta 
grid_epsilon = seq(0.01, 1, 0.1)


### Set up priors ------- 

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

stims_df <- tibble(sequence_scheme = c("BBBBBB", 
                                       "BBBBBD", 
                                       "BBBDBB", 
                                       "BDBBBB"),
                   n_features = n_feature
) %>% 
  mutate(
    stimuli_sequence = map(sequence_scheme, function(ss){make_real_stimuli_df(ss, 
                                                                              background = get_bd_pair(stimuli_pool)[1, ], 
                                                                              deviant = get_bd_pair(stimuli_pool)[2, ])}))



full_params_df <- make_simulation_params(n_sim = n_run,
                                         model_params, 
                                         stims_df)


### run model -------



all_res <- foreach(i = 1:nrow(full_params_df), .combine=rbind, .errorhandling = "remove") %dopar% {
  
  before_sim_t <- Sys.time()
  res <- granch_main_simulation(full_params_df[i, ]) 
  after_sim_t <- Sys.time()
  res <- res %>% mutate(lapse_t = after_sim_t - before_sim_t)
  saveRDS(res, paste0((full_params_df[i,])$sequence_scheme, "_res.RDS"))
  
} 




