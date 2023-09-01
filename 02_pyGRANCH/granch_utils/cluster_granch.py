
# helper functions from within the library
import helper
import importlib
import numpy as np
import torch 
import pandas as pd
import pickle
from pickle import FALSE, TRUE
from scipy.stats import norm 
import itertools
from itertools import repeat 
import scipy.stats as sts 
from torch.distributions import Normal  
import time 


# in library 
import init_params_tensor
import init_model_tensor
import main_sim_tensor
import compute_prob_tensor


importlib.reload(helper)
importlib.reload(compute_prob_tensor)
importlib.reload(init_params_tensor)
importlib.reload(init_model_tensor)
importlib.reload(main_sim_tensor)


device = 'cuda' if torch.cuda.is_available() else 'cpu'


def test_step_stability(grid_step, hypo_obs_step): 
  # initialize parameters
  start_time = time.perf_counter()
  params = init_params_tensor.granch_params(
      grid_mu = torch.linspace(start = -1, end = 1, steps = grid_step).to(device),
      grid_sigma = torch.linspace(start = 0.001, end = 1.8, steps = grid_step).to(device), 
      grid_y = torch.linspace(start = -1, end = 1, steps = grid_step).to(device), 
      grid_epsilon =  torch.linspace(start = 0.001, end = 1.8, steps = grid_step).to(device), 
      hypothetical_obs_grid_n = hypo_obs_step, 
      mu_prior = 0.001,
      V_prior = 0.001, 
      alpha_prior = 1, 
      beta_prior = 1,
      epsilon  = 0.000001, 
      mu_epsilon = torch.tensor([0.001]).to(device), 
      sd_epsilon = torch.tensor([4]).to(device), 
      world_EIGs = 0.0001,
      max_observation = 500)
  # add the various different cached bits
  params.add_meshed_grid()
  params.add_lp_mu_sigma()
  params.add_y_given_mu_sigma()
  params.add_lp_epsilon()
  params.add_priors()
  tensor_stimuli = init_model_tensor.granch_stimuli(1, 'BBBDBB')
  tensor_model = init_model_tensor.granch_model(500, tensor_stimuli)
  res = main_sim_tensor.granch_main_simulation(params, tensor_model, tensor_stimuli)
  end_time = time.perf_counter()
  t = end_time - start_time
  b = res.behavior
  b["grid_step"] = grid_step 
  b["hypo_obs_step"] = hypo_obs_step
  b["time"] = t
  return b 




grid_step_n = pd.DataFrame({"grid_step":[20, 50, 80]})
hypo_step_n = pd.DataFrame({"hypo_obs_step": [10]})
grid_step_n["key"] = 0
hypo_step_n["key"] = 0

step_df = grid_step_n.merge(hypo_step_n, on = "key", how = "outer")
#print(step_df)
res_df = pd.DataFrame()
for i in range(0, len(step_df)): 
  temp_res = test_step_stability(step_df["grid_step"][i], step_df["hypo_obs_step"][i])
  temp_res.index.name = 't'
  temp_res.reset_index(inplace = True)
  res_df = pd.concat([res_df, temp_res])


with open('res.pkl', 'wb') as outp:
    pickle.dump(res_df, outp, pickle.HIGHEST_PROTOCOL)