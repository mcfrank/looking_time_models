import ipdb
from ..granch_utils import compute_prob_tensor,init_model_tensor, main_sim_tensor, init_params_tensor 
from ..granch_utils import num_stab_help, param_funcs
import torch 
import pyro
import pickle
import torch.distributions as dist
import pandas as pd
import os 
import re
import argparse

def run_model(args):
    device = 'cuda' if torch.cuda.is_available() else 'cpu'
    print('device:', device)

    # stim_set: "unity" or "spore", paradigm: "adult" or "infant"
    EXP_INFO = {"stim_set": "unity", "paradigm": "adult"}

    BATCH_INFO = {
        "jitter_n": 20, 
        "total_batch_n": 20, 
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

    PRIOR_INFO = param_funcs.get_params(args.param_names_path, args.param_values_path) 

    ipdb.set_trace()

    # convert prior info to float
    for k, v in PRIOR_INFO.items():
        print(v)
        PRIOR_INFO[k] = float(v)
        if PRIOR_INFO[k].is_integer():
            PRIOR_INFO[k] = int(v)

    if EXP_INFO['stim_set'] == "spore": 
        stimuli_info_list = num_stab_help.sample_spore_experiment(1)
    elif EXP_INFO['stim_set'] == "unity":
        stimuli_info_list = num_stab_help.sample_condition_experiment(1, EXP_INFO['paradigm'])

    for STIMULI_INFO in stimuli_info_list: 
        print('running:', STIMULI_INFO)
        num_stab_help.run_all_sim(BATCH_GRID_INFO, PRIOR_INFO, STIMULI_INFO)

if __name__ == '__main__':
    print('entered main script')
    parser = argparse.ArgumentParser()
    parser.add_argument("param_values_path", type=str, help="Path to csv with parameters")
    parser.add_argument("param_names_path", type=str, help="Path to csv with parameter names")
    args = parser.parse_args()
    print(args)
    run_model(args)