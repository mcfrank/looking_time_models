from granch_utils import  compute_prob_tensor,init_model_tensor, main_sim_tensor, init_params_tensor 
from granch_utils import num_stab_help
import torch 
import pyro
import pickle
import torch.distributions as dist
import pandas as pd
import os 
import re
import numpy as np

device = 'cuda' if torch.cuda.is_available() else 'cpu'

print(device)

#mu_0_v_1_a_1_b_1_ep_1e-04

# stim_set: "unity" or "spore", paradigm: "adult" or "infant"
EXP_INFO = {"stim_set": "spore", "paradigm": "adult"}
#MODEL_TYPE = "no_learning"
BATCH_INFO = {
    "jitter_n": 20, 
    "total_batch_n": 20, 
    "jitter_mode": "sampling"
}



GRID_INFO = {
    "grid_mu_start": -4, "grid_mu_end": 4, "grid_mu_step": 5, 
    "grid_sigma_start": 0.001, "grid_sigma_end": 1.8, "grid_sigma_step": 5, 
    "grid_y_start": -4, "grid_y_end": 4, "grid_y_step": 5, 
    "grid_epsilon_start": 0.001, "grid_epsilon_end": 1.8, "grid_epsilon_step": 5, 
    "hypothetical_obs_grid_n": 5}

BATCH_GRID_INFO = num_stab_help.get_batch_grid(BATCH_INFO, GRID_INFO)

PRIOR_INFO = {
    "mu_prior": 0,  
    "V_prior": 1, 
    "alpha_prior": 1, 
    "beta_prior": 1, 
    "epsilon": 0.0001, "mu_epsilon": 0.001, "sd_epsilon": 4, 
    "hypothetical_obs_grid_n": 5, 
    "world_EIGs": 2, "max_observation": 500, 
    "forced_exposure_max": np.nan, 
    "linking_hypothesis":"surprisal"
}

p = [PRIOR_INFO]

if EXP_INFO['stim_set'] == "spore": 
    stimuli_info_list = num_stab_help.sample_spore_experiment(pair_each_stim = 1, n_feature=3)
elif EXP_INFO['stim_set'] == "unity":
    stimuli_info_list = num_stab_help.sample_condition_experiment(1, EXP_INFO['paradigm'])

for STIMULI_INFO in stimuli_info_list: 
    for PRIOR_INFO in p: 
        num_stab_help.run_all_sim(EXP_INFO, BATCH_GRID_INFO, PRIOR_INFO, STIMULI_INFO)
       