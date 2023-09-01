# understand why the bumps
library(tidyverse)
library(here)

res <- readRDS(here("first_embeddings.RDS"))

model <- res$results[[2]]
model  %>% 
  group_by(stimulus_idx) %>% 
  filter(!is.na(stimulus_idx)) %>% 
  summarise(sample_n = n()) %>% 
  ggplot(aes(x = stimulus_idx, y = sample_n)) + 
  stat_summary(position = position_dodge(width = .1),fun.data = "mean_cl_boot") 

model  %>% 
  ggplot(aes(x = t, y = EIG, color = as.factor(stimulus_idx))) + 
  stat_summary(position = position_dodge(width = .1),fun.data = "mean_cl_boot") 
all_sims_res$results[[2]] %>% 

  
model %>% 
  filter(stimulus_idx == 4)

# t = 89 where the first observation on stimulus 4
# t = 88 the last observaton on stimulu 3

# 3 -> 4
model %>% 
  filter(t == 88 | t == 89)
# 2 -> 3
model %>% 
  filter(t == 60 | t == 61)


l_posteriors <-  res$results[[1]][[1]]
 
l_posteriors[[88]]

# check likelihood

ll_z_given_mu_sig_sq[[1]][[1]] |>
  group_by(grid_mu_theta) |>
  summarise(lp_z_given_mu_sig_sq = matrixStats::logSumExp(lp_z_given_mu_sig_sq)) |>
  ggplot(aes(x = grid_mu_theta, y = lp_z_given_mu_sig_sq)) + 
  geom_point() 


six_this_lp_z_given_mu_sig_sq |>
  group_by(grid_mu_theta) |>
  summarise(lp_z_given_mu_sig_sq = matrixStats::logSumExp(lp_z_given_mu_sig_sq)) |>
  ggplot(aes(x = grid_mu_theta, y = lp_z_given_mu_sig_sq)) + 
  geom_point() 

seven_this_lp_z_given_mu_sig_sq |>
  group_by(grid_mu_theta) |>
  summarise(lp_z_given_mu_sig_sq = matrixStats::logSumExp(lp_z_given_mu_sig_sq)) |>
  ggplot(aes(x = grid_mu_theta, y = lp_z_given_mu_sig_sq)) + 
  geom_point() 

real_seven_lp_z_given_mu_sig_sq |>
  group_by(grid_mu_theta) |>
  summarise(lp_z_given_mu_sig_sq = matrixStats::logSumExp(lp_z_given_mu_sig_sq)) |>
  ggplot(aes(x = grid_mu_theta, y = lp_z_given_mu_sig_sq)) + 
  geom_point() 
# check posterior

posts <- map(ll_post, function(x) x[[1]])
all_posts <- map_df(1:11, function(x) {
  foo <- posts[[x]] 
  foo$idx <- x
  foo
})

all_posts |>
  group_by(idx, grid_mu_theta) |>
  summarise(posterior = sum(posterior)) |>
  ggplot(aes(x = grid_mu_theta, y = posterior)) + 
  geom_point() + 
  facet_wrap(~idx)

max(ll_post[[8]][[1]]$posterior)
max(ll_post[[7]][[1]]$posterior)
