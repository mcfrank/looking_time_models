
import numpy as np
import torch
import init_params_tensor
import init_model_tensor
import main_sim_tensor
import pandas as pd
import compute_prob_tensor

def set_param_jitter(grid_mu_start, grid_mu_end,
              grid_sigma_start, grid_sigma_end,
              grid_y_start, grid_y_end,
              grid_epsilon_start, grid_epsilon_end,
              jitter_range, jitter_n
): 
    

    jittered_params = {
        "grid_mu_starts": add_jitter(grid_mu_start, jitter_range, jitter_n), 
        "grid_mu_ends": add_jitter(grid_mu_end, jitter_range, jitter_n), 
        "grid_sigma_starts": add_jitter(grid_sigma_start, jitter_range, jitter_n), 
        "grid_sigma_ends": add_jitter(grid_sigma_end, jitter_range, jitter_n),
        "grid_y_starts": add_jitter(grid_y_start, jitter_range, jitter_n), 
        "grid_y_ends": add_jitter(grid_y_end, jitter_range, jitter_n), 
        "grid_epsilon_starts": add_jitter(grid_epsilon_start, jitter_range, jitter_n),
        "grid_epsilon_ends": add_jitter(grid_epsilon_end, jitter_range, jitter_n)
    }

    return jittered_params




def add_jitter(val, jitter_range, jitter_n): 
    return (np.linspace(start = val-jitter_range, stop = val+jitter_range,num = jitter_n))


def run_jitter_simulation(grid_mu_start, grid_mu_end, grid_mu_step,
              grid_sigma_start, grid_sigma_end,grid_sigma_step, 
              grid_y_start, grid_y_end,grid_y_step, 
              grid_epsilon_start, grid_epsilon_end,grid_epsilon_step,
              jitter_range, jitter_n): 

    all_jittered_params = set_param_jitter(grid_mu_start, grid_mu_end,
              grid_sigma_start, grid_sigma_end,
              grid_y_start, grid_y_end,
              grid_epsilon_start, grid_epsilon_end,
              jitter_range, jitter_n)
    
    res_df = pd.DataFrame()
    for i in range(0, jitter_n): 
        print(i)

        params = init_params_tensor.granch_params(
            grid_mu = torch.linspace(start = all_jittered_params["grid_mu_starts"][i], 
                                 end = all_jittered_params["grid_mu_ends"][i], steps = grid_mu_step),
            grid_sigma = torch.linspace(start = all_jittered_params["grid_sigma_starts"][i], 
                                    end = all_jittered_params["grid_sigma_ends"][i],steps = grid_sigma_step), 
            grid_y = torch.linspace(start = all_jittered_params["grid_y_starts"][i], 
                                end = all_jittered_params["grid_y_ends"][i], steps = grid_y_step), 
            grid_epsilon = torch.linspace(start = all_jittered_params["grid_epsilon_starts"][i], 
                                      end = all_jittered_params["grid_epsilon_ends"][i], steps = grid_epsilon_step), 
            hypothetical_obs_grid_n = 10, 
            mu_prior = 0.001,
            V_prior = 0.001, 
            alpha_prior = 1, 
            beta_prior = 1,
            epsilon  = 0.000001, 
            mu_epsilon = torch.tensor([0.001]), 
            sd_epsilon = torch.tensor([4]), 
            world_EIGs = 0.0001,
            max_observation = 500)
        
        # add the various different cached bits
        params.add_meshed_grid()
        #print(params.grid_mu)
        #print(params.meshed_grid_mu)
        #print(params.meshed_grid_sigma.size())
        #print(params.mu_prior)

        params.add_lp_mu_sigma()
        params.add_y_given_mu_sigma()
        params.add_lp_epsilon()
        params.add_priors()

        tensor_stimuli = init_model_tensor.granch_stimuli(1, 'BBBBBB')
        tensor_model = init_model_tensor.granch_model(500, tensor_stimuli)

        res = main_sim_tensor.granch_main_simulation(params, tensor_model, tensor_stimuli)
        b = res.behavior
        b["j_i"] = i
        b.index.name = 't'
        b.reset_index(inplace = True)
        res_df = pd.concat([res_df, b])

    return res_df

