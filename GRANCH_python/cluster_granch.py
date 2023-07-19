
from granch_utils import  compute_prob_tensor,init_model_tensor, main_sim_tensor, init_params_tensor 
from granch_utils import num_stab_help

import torch 
import pyro
import pickle
import torch.distributions as dist
import pandas as pd
import os 
import re


device = 'cuda' if torch.cuda.is_available() else 'cpu'

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

PRIOR_INFO = {
    "mu_prior": 0,  
    "V_prior": 3, 
    "alpha_prior": 1, 
    "beta_prior": 2, 
    "epsilon": 0.001, "mu_epsilon": 0.001, "sd_epsilon": 4, 
    "hypothetical_obs_grid_n": 10, 
    "world_EIGs": 0.0001, "max_observation": 500
}



p = num_stab_help.create_prior_list(PRIOR_INFO, "epsilon", prior_val_list=[0.5, 0.3, 0.1, 0.001, 0.0001, 0.00001, 0.000001, 0.0000001])


l_small = num_stab_help.set_up_toy_example(0.10101012, 0.40101012)
l_big = num_stab_help.set_up_toy_example(0.10101012, 0.90101012)
stimuli_info_list = []
stimuli_info_list.extend(l_small)
stimuli_info_list.extend(l_big)


for STIMULI_INFO in stimuli_info_list: 
    for PRIOR_INFO in p: 
        num_stab_help.run_all_sim(BATCH_GRID_INFO, PRIOR_INFO, STIMULI_INFO)


folder_path = "cache_results/"
df_list = []
for file_name in os.listdir(folder_path): 
    pattern_batch_info = r"batch_(\d+)_cache_([A-Z]+)"
    #pattern_stim_spec = r"b_([\d\.]+)_d_([\d\.]+)"
    if file_name.endswith(".pickle"):    
        batch_id, stimuli = re.search(pattern_batch_info, file_name).groups()
        file_path = os.path.join(folder_path, file_name)
        df = pd.read_pickle(file_path)
        df["batch_id"] = batch_id
        df["stimuli"] = stimuli
        df_list.append(df)

main_df = pd.concat(df_list)
main_df = main_df.dropna()

with open("cache_results/cache_summary/summary.pickle", 'wb') as f:
    pickle.dump(main_df, f)