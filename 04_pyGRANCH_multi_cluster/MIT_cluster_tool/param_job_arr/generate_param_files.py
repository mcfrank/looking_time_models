import numpy as np
import itertools
import csv
import os
import glob

# generate less params if it's a test run
real_run = True

## this generates parameter values for the main model pipeline
    
# empty all files first
files = glob.glob('02_pyGRANCH/MIT_cluster_tool/param_job_arr/params/param_vals/*')
for f in files:
    os.remove(f)

# parameters
if real_run:
    param_dict = {'mu_prior': [0],
                'V_prior': np.arange(1, 4, 1).tolist(),
                'alpha_prior': [0.1,1,10],
                'beta_prior': [0.1,1,10],
                "epsilon": [0.0001], "mu_epsilon": [0.001], "sd_epsilon": [1,2,3], 
                "hypothetical_obs_grid_n": [10], 
                "world_EIGs": [0.0001], "max_observation": [500], "forced_exposure_max":[5,10]}
else:
    param_dict = {'mu_prior': [1,2],
                'V_prior': 1,
                'alpha_prior': 1,
                'beta_prior': 1,
                "epsilon": 0.000001, "mu_epsilon": 0.001, "sd_epsilon": 4, 
                "hypothetical_obs_grid_n": 10, 
                "world_EIGs": 0.0001, "max_observation": 500, "forced_exposure_max":5}

all_params_combinations = itertools.product(*list(param_dict.values()))

# write param value files
for param_idx, combination in enumerate(list(all_params_combinations)):
    with open('02_pyGRANCH/MIT_cluster_tool/param_job_arr/params/param_vals/params' + str(param_idx) + '.csv', 'w') as myfile:
        wr = csv.writer(myfile)
        wr.writerow(list(combination))

# write param name file
with open('02_pyGRANCH/MIT_cluster_tool/param_job_arr/params/param_names.csv', 'w') as myfile:
    wr = csv.writer(myfile)
    wr.writerow(param_dict.keys()) 