
from granch_utils import  compute_prob_tensor,init_model_tensor, main_sim_tensor, init_params_tensor 
from granch_utils import num_stab_help
import torch 
import pyro
import pickle
import torch.distributions as dist
import pandas as pd
import os 
import re
import ipdb


device = 'cuda' if torch.cuda.is_available() else 'cpu'


print(device)

BATCH_INFO = {
    "jitter_n": 1, 
    "total_batch_n": 1, 
    "jitter_mode": "sampling"
}

GRID_INFO = {
    "grid_mu_start": -4, "grid_mu_end": 4, "grid_mu_step": 2, 
    "grid_sigma_start": 0.001, "grid_sigma_end": 1.8, "grid_sigma_step": 3, 
    "grid_y_start": -4, "grid_y_end": 4, "grid_y_step": 4, 
    "grid_epsilon_start": 0.001, "grid_epsilon_end": 1.8, "grid_epsilon_step": 5, 
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
    "world_EIGs": 0.00001, "max_observation": 500
}

p = [PRIOR_INFO]

stimuli_info_list = num_stab_help.sample_condition_experiment(1)

for STIMULI_INFO in stimuli_info_list: 
    for PRIOR_INFO in p: 
        num_stab_help.run_all_sim(BATCH_GRID_INFO, PRIOR_INFO, STIMULI_INFO)


folder_path = "cache_results/"
df_list = []
for file_name in os.listdir(folder_path): 
    pattern_batch_info = r"batch_(\d+)_cache_([A-Z]+)"
    #pattern_stim_spec = r"b_([\d\.]+)_d_([\d\.]+)"
    if file_name.endswith(".pickle"):    
        file_path = os.path.join(folder_path, file_name)
        df = pd.read_pickle(file_path)
        df_list.append(df)

main_df = pd.concat(df_list)
main_df = main_df.dropna()

with open("cache_results/cache_summary/summary.pickle", 'wb') as f:
    pickle.dump(main_df, f)