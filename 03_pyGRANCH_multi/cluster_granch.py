from granch_utils import  compute_prob_tensor,init_model_tensor, main_sim_tensor, init_params_tensor 
from granch_utils import num_stab_help, params_search
import torch 
import pyro
import pickle
import torch.distributions as dist
import pandas as pd
import os 
import re
import numpy as np
import itertools

device = 'cuda' if torch.cuda.is_available() else 'cpu'
print('device:', device)

# stim_set: "unity" or "spore", paradigm: "adult" or "infant"
EXP_INFO = {"stim_set": "unity", "paradigm": "infant"}

BATCH_INFO = {
        "jitter_n": 10, 
        "total_batch_n": 10, # can change this reduce memory load
        "jitter_mode": "sampling"
}

GRID_INFO = {
        "grid_mu_start": -4, "grid_mu_end": 4, "grid_mu_step": 20, # can change this reduce memory load
        "grid_sigma_start": 0.001, "grid_sigma_end": 1.8, "grid_sigma_step": 20, 
        "grid_y_start": -4, "grid_y_end": 4, "grid_y_step": 20, 
        "grid_epsilon_start": 0.001, "grid_epsilon_end": 1.8, "grid_epsilon_step": 20}


BATCH_GRID_INFO = num_stab_help.get_batch_grid(BATCH_INFO, GRID_INFO)

param_dict = {'mu_prior': [0],
                'V_prior': np.arange(1, 4, 1).tolist(),
                'alpha_prior': [0.1,1,10],
                'beta_prior': [0.1,1,10],
                "epsilon": [0.0001], "mu_epsilon": [0.001], "sd_epsilon": [0.1,0.5,1], 
                "hypothetical_obs_grid_n": [5], 
                "world_EIGs": [0.0001, 0.00001], "max_observation": [500], "forced_exposure_max":[5], # for KL try ~10e-5, for surprisal between 1 and 10
            }

all_params_combinations = list(itertools.product(*list(param_dict.values())))
all_priors = params_search.get_param_list(all_params_combinations)[:1]

print(all_priors)
stimuli_info_list = num_stab_help.sample_baby_stims(pair_each_stim=1, n_feature=3)


for PRIOR_INFO in all_priors:
    for STIMULI_INFO in stimuli_info_list: 
        print('running:', STIMULI_INFO)
        num_stab_help.run_all_sim(EXP_INFO, BATCH_GRID_INFO, PRIOR_INFO, STIMULI_INFO, MODEL_TYPE = "no_learning")



