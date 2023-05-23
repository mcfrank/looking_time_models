
from granch_utils import  compute_prob_tensor,init_model_tensor, main_sim_tensor, init_params_tensor 
from granch_utils import num_stab_help

import torch 
import pyro
import pickle
import torch.distributions as dist
import pandas as pd


device = 'cuda' if torch.cuda.is_available() else 'cpu'

BATCH_INFO = {
    "jitter_n": 20, 
    "total_batch_n": 20, 
    "jitter_mode": "sampling"
}

GRID_INFO = {
    "grid_mu_start": -1, "grid_mu_end": 1, "grid_mu_step": 20, 
    "grid_sigma_start": 0.001, "grid_sigma_end": 1.8, "grid_sigma_step": 20, 
    "grid_y_start": -1, "grid_y_end": 1, "grid_y_step": 20, 
    "grid_epsilon_start": 0.001, "grid_epsilon_end": 1.8, "grid_epsilon_step": 20, 
    "hypothetical_obs_grid_n": 10
}



BATCH_GRID_INFO = num_stab_help.get_batch_grid(BATCH_INFO, GRID_INFO)

PRIOR_INFO = {
    "mu_prior": 0.001,  
    "V_prior": 0.001, 
    "alpha_prior": 1, 
    "beta_prior": 1, 
    "epsilon": 0.000001, "mu_epsilon": 0.001, "sd_epsilon": 4, 
    "hypothetical_obs_grid_n": 10, 
    "world_EIGs": 0.0001, "max_observation": 500
}

stimuli_info_list = [init_model_tensor.granch_stimuli(1, 'BBBBBB', 0.1, 0.7), 
                     init_model_tensor.granch_stimuli(1, 'BDBBBB', 0.1, 0.7), 
                     init_model_tensor.granch_stimuli(1, 'BBBDBB', 0.1, 0.7),
                     init_model_tensor.granch_stimuli(1, 'BBBBBD', 0.1, 0.7)]

for STIMULI_INFO in stimuli_info_list: 
    num_stab_help.run_all_sim(BATCH_GRID_INFO, PRIOR_INFO, STIMULI_INFO)
