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
import numpy as np
import ipdb

def run_model(args):
    device = 'cuda' if torch.cuda.is_available() else 'cpu'
    print('device:', device)

    # stim_set: "unity" or "spore", paradigm: "adult" or "infant"
    EXP_INFO = {"stim_set": "unity", "paradigm": "infant"}

    BATCH_INFO = {
        "jitter_n": 20, 
        "total_batch_n": 1, # can change this reduce memory load
        "jitter_mode": "sampling"
    }

    GRID_INFO = {
        "grid_mu_start": -4, "grid_mu_end": 4, "grid_mu_step": 20, # can change this reduce memory load
        "grid_sigma_start": 0.001, "grid_sigma_end": 1.8, "grid_sigma_step": 20, 
        "grid_y_start": -4, "grid_y_end": 4, "grid_y_step": 20, 
        "grid_epsilon_start": 0.001, "grid_epsilon_end": 1.8, "grid_epsilon_step": 20}


    BATCH_GRID_INFO = num_stab_help.get_batch_grid(BATCH_INFO, GRID_INFO)

    PRIOR_INFO = param_funcs.get_params(args.param_names_path, args.param_values_path) 

    PRIOR_INFO["linking_hypothesis"] = "EIG"

    stimuli_info_list = num_stab_help.sample_baby_stims(pair_each_stim=1, n_feature=3)

    for STIMULI_INFO in stimuli_info_list: 
        print('running:', STIMULI_INFO)
        num_stab_help.run_all_sim(EXP_INFO, BATCH_GRID_INFO, PRIOR_INFO, STIMULI_INFO)


if __name__ == '__main__':
    print('entered main script')
    parser = argparse.ArgumentParser()
    parser.add_argument("param_values_path", type=str, help="Path to csv with parameters")
    parser.add_argument("param_names_path", type=str, help="Path to csv with parameter names")
    args = parser.parse_args()
    print(args)
    run_model(args)