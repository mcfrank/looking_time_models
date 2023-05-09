import time 
import numpy as np
import seaborn as sns
from torch.distributions import Normal, uniform
import torch.distributions as dist
import torch
#import init_params_tensor
#import init_model_tensor
#import main_sim_tensor
import pandas as pd
from granch_utils import init_model_tensor, main_sim_tensor, init_params_tensor, compute_prob_tensor 


# just in case wanna implement the sample version
#grid_mu_distribution = uniform.Uniform(all_jittered_params["grid_mu_starts"][0], all_jittered_params["grid_mu_ends"][0])
#grid_sigma_distribution = uniform.Uniform(max(0.0000001, all_jittered_params["grid_sigma_starts"][0]), all_jittered_params["grid_sigma_ends"][0])
# grid_y_distribution = uniform.Uniform(all_jittered_params["grid_y_starts"][0], all_jittered_params["grid_y_ends"][0])
#grid_epsilon_distribution = uniform.Uniform(max(0.0000001, all_jittered_params["grid_epsilon_starts"][0]), all_jittered_params["grid_epsilon_ends"][0])


def run_all_sim(
        BATCH_GRID_INFO, 
        PRIOR_INFO, 
        STIMULI_INFO
): 
    device = 'cuda' if torch.cuda.is_available() else 'cpu'

    
    # access all the priors 
    mu_prior = PRIOR_INFO["mu_prior"]
    V_prior = PRIOR_INFO["V_prior"]
    alpha_prior = torch.tensor([PRIOR_INFO["alpha_prior"]]).to(device)
    beta_prior = torch.tensor([PRIOR_INFO["beta_prior"]]).to(device)
    epsilon = PRIOR_INFO["epsilon"]
    mu_epsilon = torch.tensor([PRIOR_INFO["mu_epsilon"]]).to(device)
    sd_epsilon = torch.tensor([PRIOR_INFO["sd_epsilon"]]).to(device)
    world_EIGs = PRIOR_INFO["world_EIGs"]
    hypothetical_obs_grid_n = PRIOR_INFO["hypothetical_obs_grid_n"]
    max_observation = PRIOR_INFO["max_observation"]

    tensor_stimuli = STIMULI_INFO
    tensor_model =  init_model_tensor.granch_model(max_observation, tensor_stimuli)
    # access all the grid_info 
    res_df = pd.DataFrame()

    for i in range(0,BATCH_GRID_INFO["jitter_n"]): 

        params = init_params_tensor.granch_params(
            grid_mu =  torch.linspace(start = BATCH_GRID_INFO["grid_mu_starts"][i], 
                                      end = BATCH_GRID_INFO["grid_mu_ends"][i], 
                                      steps = BATCH_GRID_INFO["grid_mu_step"]).to(device),
            grid_sigma = torch.linspace(start = max(0.000000001, BATCH_GRID_INFO["grid_sigma_starts"][i]), 
                                      end = BATCH_GRID_INFO["grid_sigma_ends"][i], 
                                      steps = BATCH_GRID_INFO["grid_sigma_step"]).to(device),
            grid_y = torch.linspace(start = BATCH_GRID_INFO["grid_y_starts"][i], 
                                      end = BATCH_GRID_INFO["grid_y_ends"][i], 
                                      steps = BATCH_GRID_INFO["grid_y_step"]).to(device),
            grid_epsilon = torch.linspace(start = max(0.000000001, BATCH_GRID_INFO["grid_epsilon_starts"][i]), 
                                      end = BATCH_GRID_INFO["grid_epsilon_ends"][i], 
                                      steps = BATCH_GRID_INFO["grid_epsilon_step"]).to(device),
            hypothetical_obs_grid_n = hypothetical_obs_grid_n, 
            mu_prior = mu_prior,
            V_prior = V_prior, 
            alpha_prior = alpha_prior, 
            beta_prior = beta_prior,
            epsilon  = epsilon, 
            mu_epsilon = mu_epsilon, 
            sd_epsilon = sd_epsilon, 
            world_EIGs = world_EIGs,
            max_observation = max_observation)
        
        # add the various different cached bits
        params.add_meshed_grid()
        params.add_lp_mu_sigma()
        params.add_y_given_mu_sigma()
        params.add_lp_epsilon()
        params.add_priors()

        res = main_sim_tensor.granch_main_simulation(params, tensor_model, tensor_stimuli)
        b = res.behavior
        b["j_i"] = i

        res_df = pd.concat([res_df, b])

    return res_df



def add_jitter(val, jitter_range, jitter_n): 
    return (np.random.uniform(low = val-jitter_range, high = val+jitter_range,size = jitter_n))


def get_batch_grid(BATCH_INFO, 
                   GRID_INFO):
    
    if BATCH_INFO["jitter_mode"] == "sampling": 
        pass 
    elif BATCH_INFO["jitter_mode"] == "single_end": 


        jitter_range = (GRID_INFO["grid_mu_end"] - GRID_INFO["grid_mu_start"]) / GRID_INFO["grid_mu_step"]

        batch_grid = {
            "jitter_n": BATCH_INFO["jitter_n"],
            "grid_mu_starts": add_jitter(GRID_INFO["grid_mu_start"], jitter_range, BATCH_INFO["jitter_n"]), 
            "grid_mu_step":  GRID_INFO["grid_mu_step"],
            "grid_sigma_starts": add_jitter(GRID_INFO["grid_sigma_start"], jitter_range, BATCH_INFO["jitter_n"]), 
            "grid_sigma_step":  GRID_INFO["grid_sigma_step"],
            "grid_y_starts": add_jitter(GRID_INFO["grid_y_start"], jitter_range, BATCH_INFO["jitter_n"]), 
            "grid_y_step":  GRID_INFO["grid_y_step"],
            "grid_epsilon_starts": add_jitter(GRID_INFO["grid_epsilon_start"], jitter_range, BATCH_INFO["jitter_n"]),
            "grid_epsilon_step": GRID_INFO["grid_epsilon_step"]
        }
        batch_grid["grid_mu_ends"] = batch_grid["grid_mu_starts"] + (GRID_INFO["grid_mu_end"] - GRID_INFO["grid_mu_start"])
        batch_grid["grid_sigma_ends"] = batch_grid["grid_sigma_starts"] + (GRID_INFO["grid_sigma_end"] - GRID_INFO["grid_sigma_start"])
        batch_grid["grid_y_ends"] = batch_grid["grid_y_starts"] + (GRID_INFO["grid_y_end"] - GRID_INFO["grid_y_start"])
        batch_grid["grid_epsilon_ends"] = batch_grid["grid_epsilon_starts"] + (GRID_INFO["grid_epsilon_end"] - GRID_INFO["grid_epsilon_start"])

    return batch_grid
