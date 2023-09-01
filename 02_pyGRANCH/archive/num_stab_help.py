
import numpy as np
import seaborn as sns
from torch.distributions import Normal, uniform
import torch.distributions as dist
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
        "jitter_n": jitter_n,
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
    return (np.random.uniform(low = val-jitter_range, high = val+jitter_range,size = jitter_n))


def visualize_jitter_mu(all_jittered_params, tensor_stimuli, grid_mu_step):

    background = 0.1
    deviant = 0.7

    jitter_pd = pd.DataFrame(all_jittered_params)
    grid_mu_df = jitter_pd[["grid_mu_starts", "grid_mu_ends"]]
    grid_mu_df["mu"] = [np.linspace(start, end, grid_mu_step) for start, end in zip(grid_mu_df['grid_mu_starts'], grid_mu_df['grid_mu_ends'])]
    
    grid_mu_df = grid_mu_df.explode("mu")
    grid_mu_df = grid_mu_df.reset_index()
    grid_mu_df = grid_mu_df.rename(columns={'index': 'j_id'})

    grid_mu_df["index"] = grid_mu_df["j_id"].astype("string")
    grid_mu_df["j_id"] = grid_mu_df["j_id"]
    grid_mu_df

    p = sns.scatterplot(x = "mu", 
             y = "j_id", 
             data = grid_mu_df, 
             hue = "index")

    # blue is background 
    p.axvline(background)
    p.axvline(deviant, color = "red")
    p.legend([], [], frameon = False)

    return p


       


def run_jitter_simulation_with_fix_ends(
                                         grid_mu_start, grid_mu_end, grid_mu_step, 
                                         grid_sigma_start, grid_sigma_end, grid_sigma_step,
                                         grid_y_start, grid_y_end, grid_y_step,
                                         grid_epsilon_start, grid_epsilon_end, grid_epsilon_step,
                                         run_n, 
               visualize_mu = False): 
    

    device = 'cuda' if torch.cuda.is_available() else 'cpu'

    tensor_stimuli = init_model_tensor.granch_stimuli(1, 'BBBBBB')
    tensor_model = init_model_tensor.granch_model(500, tensor_stimuli)
    
    res_df = pd.DataFrame()
    
    if visualize_mu: 
        pass        
    
    grid_mu_distribution = dist.Uniform(grid_mu_start, grid_mu_end)
    grid_sigma_distribution = dist.Uniform(max(0.0000000000001,grid_sigma_start), grid_sigma_end)
    grid_y_distribution = dist.Uniform(grid_y_start, grid_y_end)
    grid_epsilon_distribution = dist.Uniform(max(0.0000000000001, grid_epsilon_start), grid_epsilon_end)

    print(grid_mu_distribution.low.device)

    for i in range(0, run_n): 
        print(i)

        params = init_params_tensor.granch_params(
            grid_mu = grid_mu_distribution.sample((grid_mu_step,)).to(device),
            grid_sigma = grid_sigma_distribution.sample((grid_sigma_step,)).to(device),
            grid_y = grid_y_distribution.sample((grid_y_step,)).to(device),
            grid_epsilon = grid_epsilon_distribution.sample((grid_epsilon_step,)).to(device),
            hypothetical_obs_grid_n = 10, 
            mu_prior = 0.001,
            V_prior = 0.001, 
            alpha_prior = torch.tensor([1]).to(device), 
            beta_prior = torch.tensor([1]).to(device),
            epsilon  = 0.000001, 
            mu_epsilon = torch.tensor([0.001]).to(device), 
            sd_epsilon = torch.tensor([4]).to(device), 
            world_EIGs = 0.0001,
            max_observation = 500)
        
        # add the various different cached bits
        params.add_meshed_grid()
        params.add_lp_mu_sigma()
        params.add_y_given_mu_sigma()
        params.add_lp_epsilon()
        params.add_priors()

        

        res = main_sim_tensor.granch_main_simulation(params, tensor_model, tensor_stimuli)
        b = res.behavior
        b["j_i"] = i
        #b.index.name = 't'
        #b.reset_index(inplace = True)
        res_df = pd.concat([res_df, b])

    return res_df





def run_jitter_simulation(all_jittered_params, 
                grid_mu_step, grid_sigma_step,grid_y_step, grid_epsilon_step,
               visualize_mu = False): 
    

    device = 'cuda' if torch.cuda.is_available() else 'cpu'

    
    tensor_stimuli = init_model_tensor.granch_stimuli(1, 'BBBBBB')
    tensor_model = init_model_tensor.granch_model(500, tensor_stimuli)
    
    res_df = pd.DataFrame()
    
    if visualize_mu: 
        visualize_jitter_mu(all_jittered_params, tensor_stimuli, grid_mu_step)
        
    
    grid_mu_distribution = uniform.Uniform(all_jittered_params["grid_mu_starts"][0], all_jittered_params["grid_mu_ends"][0])
    grid_sigma_distribution = uniform.Uniform(max(0.0000001, all_jittered_params["grid_sigma_starts"][0]), all_jittered_params["grid_sigma_ends"][0])
    grid_y_distribution = uniform.Uniform(all_jittered_params["grid_y_starts"][0], all_jittered_params["grid_y_ends"][0])
    grid_epsilon_distribution = uniform.Uniform(max(0.0000001, all_jittered_params["grid_epsilon_starts"][0]), all_jittered_params["grid_epsilon_ends"][0])

    for i in range(0, all_jittered_params["jitter_n"]): 
        print(i)


        # equal distance version
        
        #params = init_params_tensor.granch_params(
        #    grid_mu = torch.linspace(start = all_jittered_params["grid_mu_starts"][i], 
        #                         end = all_jittered_params["grid_mu_ends"][i], steps = grid_mu_step).to(device),
        #    grid_sigma = torch.linspace(start = max(0.0000001, all_jittered_params["grid_sigma_starts"][i]), 
        #                            end = all_jittered_params["grid_sigma_ends"][i],steps = grid_sigma_step).to(device), 
        #    grid_y = torch.linspace(start = all_jittered_params["grid_y_starts"][i], 
        #                        end = all_jittered_params["grid_y_ends"][i], steps = grid_y_step).to(device), 
        #    grid_epsilon = torch.linspace(start =  max(0.0000001, all_jittered_params["grid_epsilon_starts"][i]), 
        #                              end = all_jittered_params["grid_epsilon_ends"][i], steps = grid_epsilon_step).to(device), 
        #    hypothetical_obs_grid_n = 10, 
        #    mu_prior = 0.001,
        #    V_prior = 0.001, 
        #    alpha_prior = torch.tensor([1]).to(device), 
        #    beta_prior = torch.tensor([1]).to(device),
        #    epsilon  = 0.000001, 
        #    mu_epsilon = torch.tensor([0.001]).to(device), 
        #    sd_epsilon = torch.tensor([4]).to(device), 
        #    world_EIGs = 0.0001,
        #    max_observation = 500)
        
        # spread version 

        params = init_params_tensor.granch_params(
            grid_mu = grid_mu_distribution.sample((grid_mu_step,)).to(device),
            grid_sigma = grid_sigma_distribution.sample((grid_sigma_step,)).to(device),
            grid_y = grid_y_distribution.sample((grid_y_step,)).to(device),
            grid_epsilon = grid_epsilon_distribution.sample((grid_epsilon_step,)).to(device),
            hypothetical_obs_grid_n = 10, 
            mu_prior = 0.001,
            V_prior = 0.001, 
            alpha_prior = torch.tensor([1]).to(device), 
            beta_prior = torch.tensor([1]).to(device),
            epsilon  = 0.000001, 
            mu_epsilon = torch.tensor([0.001]).to(device), 
            sd_epsilon = torch.tensor([4]).to(device), 
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

        

        res = main_sim_tensor.granch_main_simulation(params, tensor_model, tensor_stimuli)
        b = res.behavior
        b["j_i"] = i
        #b.index.name = 't'
        #b.reset_index(inplace = True)
        res_df = pd.concat([res_df, b])

    return res_df

