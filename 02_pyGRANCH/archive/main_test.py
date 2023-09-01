import torch 
import pandas as pd
import init_params
import compute_prob
import init_model
import helper
import main_sim
import importlib 


importlib.reload(helper)
importlib.reload(compute_prob)
importlib.reload(init_params)
importlib.reload(init_model)
importlib.reload(main_sim)


p = init_params.granch_params(
    grid_mu_theta = torch.linspace(start = -1, end = 1, steps = 3), 
    grid_sig_sq = torch.linspace(start = 0.001, end = 2, steps = 3), 
    grid_y = torch.linspace(start = -1, end = 1, steps = 3), 
    grid_epsilon =  torch.linspace(start = 0.001, end = 2, steps = 3), 
    hypothetical_obs_grid_n = 3, 
    mu_prior = 0.001,
    V_prior = 0.001, 
    alpha_prior = 1, 
    beta_prior = 1,
    epsilon  = 0.000001, 
    mu_epsilon = torch.tensor([0.001]), 
    sd_epsilon = torch.tensor([4]), 
    world_EIGs = 0.001,
    max_observation = 500)


# set up the prior dfs 
p.add_meshed_grid()
p.add_lp_mu_sig_sq()
p.add_y_given_mu_sig_sq()
p.add_lp_epsilon()
# this added components to be used that can be used to calculate posterior
p.add_priors()


s = init_model.granch_stimuli(1, 'BBBBBB')
# s.get_stimuli_sequence("embedding_PCA.csv")


m = init_model.granch_model(500, s)

res = main_sim.granch_main_simulation(p, m, s)

