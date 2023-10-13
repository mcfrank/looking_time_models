
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
import argparse

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
    "hypothetical_obs_grid_n": 5
}

BATCH_GRID_INFO = num_stab_help.get_batch_grid(BATCH_INFO, GRID_INFO)

PRIOR_INFO = param_funcs.get_params(args.param_names_path, args.param_values_path) 


p = [PRIOR_INFO]

if EXP_INFO['stim_set'] == "spore": 
    stimuli_info_list = num_stab_help.sample_spore_experiment(1)
elif EXP_INFO['stim_set'] == "unity":
    stimuli_info_list = num_stab_help.sample_condition_experiment(1, EXP_INFO['paradigm'])

for STIMULI_INFO in stimuli_info_list: 
    for PRIOR_INFO in p: 
        num_stab_help.run_all_sim(EXP_INFO, BATCH_GRID_INFO, PRIOR_INFO, STIMULI_INFO, MODEL_TYPE = "no_learning")
        num_stab_help.run_all_sim(EXP_INFO, BATCH_GRID_INFO, PRIOR_INFO, STIMULI_INFO, MODEL_TYPE = "no_noise")


if __name__ == '__main__':
    print('entered main script')
    parser = argparse.ArgumentParser()
    parser.add_argument("param_values_path", type=str, help="Path to csv with parameters")
    parser.add_argument("param_names_path", type=str, help="Path to csv with parameter names")
    args = parser.parse_args()
    print(args)
    run_model(args)
