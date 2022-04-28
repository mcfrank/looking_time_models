# check prior


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
