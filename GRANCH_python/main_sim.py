from scipy.stats import norm 
from itertools import repeat 

import scipy.stats as sts 
import numpy as np
import torch 
import importlib 
from torch.distributions import Normal  

import compute_prob
import init_params
import helper 
import init_model

""" 
p = init_params.granch_params(
    grid_mu_theta = torch.tensor([0.1, 0.2, 0.3]), 
    grid_sig_sq = torch.tensor([0.4, 0.5, 0.6]), 
    grid_y = torch.tensor([0.7, 0.8, 0.9]), 
    grid_epsilon = torch.tensor([0.11, 0.12, 0.13]), 
    hypothetical_obs_grid_n = 2, 
    mu_prior = 1,
    V_prior = 1, 
    alpha_prior = 1, 
    beta_prior = 1, 
    epsilon  = 0.01, 
    mu_epsilon = torch.tensor([0.01]), 
    sd_epsilon = torch.tensor([0.01]), 
    world_EIGs = 0.01,
    max_observation = 500
)

# set up the prior dfs 
p.add_meshed_grid()
p.add_lp_mu_sig_sq()
p.add_y_given_mu_sig_sq()
p.add_lp_epsilon()
s = init_model.granch_stimuli(1, 'BBB')
m = init_model.granch_model(500, s)
m.update_model_stimulus_id()

"""

def granch_main_simulation(params, model, stimuli): 

    stimulus_idx = 0
    t = 0 # following python tradition we are using 0-indexed
    # pending stimulus 
    #while stimulus_idx <= stimuli.n_trial and t <= model.max_observation:  
    while t < 10: 
        # update model behavior with current t and current stimulus_idx 
        model.current_t = t 
        model.current_stimulus_idx = stimulus_idx
        model.update_model_stimulus_id()

        # make noisy observation and keep track 
        model.update_noisy_observation(params.epsilon)

        # get all possible observation on current stimulus 
        # if we didn't change stimulus 
        if model.current_t == 0 or (not model.if_same_stimulus_as_previous_t()): 
            model.update_possible_observations(params.epsilon, 
                                               params.hypothetical_obs_grid_n)
        



        # this is for EIG, wait till later 


        # update the likelihood and posterior 

        # first sketch out an iterative version with all feature 
        # then try to do it in parallel 
        
        # iterate through features: 
        # for: 
        # core: 
               
        model.update_likelihood(compute_prob.score_likelihood(model, params))
        model.update_posterior(compute_prob.score_posterior(model,params))
       
        if t >=1: 
            # calculate basic KL and PP 
            # later needs to be in a loop or some sort of fancy matrix multiplication
            model.all_basic_KL[model.current_t] = compute_prob.kl_div(model.all_posterior[model.current_t], 
                                                                model.all_posterior[model.current_t - 1])
            model.all_basic_PP[model.current_t] = compute_prob.score_post_pred(torch.tensor([0.11]), 
                                                                               model, params)
        
       
        # then calculate EIG 
        t = t + 1






