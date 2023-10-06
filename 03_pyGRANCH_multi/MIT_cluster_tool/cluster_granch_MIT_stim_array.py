import ipdb
from ..granch_utils import compute_prob_tensor,init_model_tensor, main_sim_tensor, init_params_tensor 
from ..granch_utils import num_stab_help
import torch 
import pyro
import pickle
import torch.distributions as dist
import pandas as pd
import os 
import re
import argparse
import numpy as np

def run_model(args):
    device = 'cuda' if torch.cuda.is_available() else 'cpu'
    print('device:', device)

    # stim_set: "unity" or "spore", paradigm: "adult" or "infant"
    EXP_INFO = {"stim_set": "unity", "paradigm": "adult"}

    BATCH_INFO = {
        "jitter_n": 3, 
        "total_batch_n": 3, 
        "jitter_mode": "sampling"
    }

    GRID_INFO = {
        "grid_mu_start": -4, "grid_mu_end": 4, "grid_mu_step": 20, 
        "grid_sigma_start": 0.001, "grid_sigma_end": 1.8, "grid_sigma_step": 20, 
        "grid_y_start": -4, "grid_y_end": 4, "grid_y_step": 20, 
        "grid_epsilon_start": 0.001, "grid_epsilon_end": 1.8, "grid_epsilon_step": 20, 
        "hypothetical_obs_grid_n": 10
    }

    BATCH_GRID_INFO = num_stab_help.get_batch_grid(BATCH_INFO, GRID_INFO)

    PRIOR_INFO = {
        "mu_prior": -1,  
        "V_prior": 1, 
        "alpha_prior": 1, 
        "beta_prior": 35, 
        "epsilon": 0.001, "mu_epsilon": 0.001, "sd_epsilon": 4, 
        "hypothetical_obs_grid_n": 10, 
        "world_EIGs": 0.00001, "max_observation": 500, 
        "forced_exposure_max": np.nan}

    p = [PRIOR_INFO]

    STIMULI_INFO = torch.load(args.stim_info_path)
    
    for PRIOR_INFO in p: 
        num_stab_help.run_all_sim(EXP_INFO, BATCH_GRID_INFO, PRIOR_INFO, STIMULI_INFO)

if __name__ == '__main__':
    print('entered main script')
    parser = argparse.ArgumentParser()
    parser.add_argument("stim_info_path", type=str, help="path to stim torch object")
    args = parser.parse_args()
    print(args)
    run_model(args)