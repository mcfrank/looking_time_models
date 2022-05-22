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
  source("~/cluster_granch/helper/time_granch_helper.r")
  source("~/cluster_granch/helper/compute_prob.r") # can't believe it's case sensitive on the server!
  source("~/cluster_granch/helper/embedding_helper.r")
  source("~/cluster_granch/helper/init.r")
  source("~/cluster_granch/helper/granch_main_simulation_helper.R")
  source("~/cluster_granch/helper/granch_sim_setup_helper.R")
  
  embedding_path <- "~/cluster_granch/embeddings/embedding_PCA.csv"
  
}else{
  library(here)
  source(here("cluster_granch/helper/time_granch_helper.r"))
  source(here("cluster_granch/helper/compute_prob.r")) # can't believe it's case sensitive on the server!
  source(here("cluster_granch/helper/embedding_helper.r"))
  source(here("cluster_granch/helper/init.r"))
  source(here("cluster_granch/helper/granch_main_simulation_helper.R"))
  source(here("cluster_granch/helper/granch_sim_setup_helper.R"))
  
  embedding_path <- here("cluster_granch/embeddings/embedding_PCA.csv")
  
}


time_granch_df <- tibble(
  embedding_path = embedding_path, 
  grid_mu_n = seq(1, 5, 1), 
  grid_sig_sq_n = seq(1, 5, 1),
  grid_y_n = seq(1, 5, 1), 
  grid_epsilon_n = seq(1, 5, 1)
) %>% 
  crossing(
    n_feature = seq(1, 5, 1), 
    hypothetical_obs_grid_n = seq(2, 3, 1), 
  )


all_res <- foreach(i = 1:nrow(time_granch_df), .combine=rbind, .errorhandling = "remove") %dopar% {


  res <- time_granch_simulation(time_granch_df[i, ]$embedding_path, 
                                time_granch_df[i, ]$grid_mu_n, 
                                time_granch_df[i, ]$grid_sig_sq_n, 
                                time_granch_df[i, ]$grid_y_n, 
                                time_granch_df[i, ]$grid_epsilon_n, 
                                time_granch_df[i, ]$hypothetical_obs_grid_n, 
                                time_granch_df[i, ]$n_feature) 
  
} 

saveRDS(all_res, "fake_med_granch_timing.RDS")
