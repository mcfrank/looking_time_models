from pickle import FALSE, TRUE
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
    #while stimulus_idx < stimuli.n_trial and t < model.max_observation: 
    while t < 2:
    #while t < params.max_observation and stimulus_idx < stimuli.n_trial: 
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
                            

        # updating the likelihood and posterior for the observed 
        model.update_likelihood(compute_prob.score_likelihood(model, params, hypothetical_obs = False))
        model.update_posterior(compute_prob.score_posterior(model,params, hypothetical_obs=False))
       
        # this iteration/index will currently work for only single feature
        n_possible_observations = len(model.possible_observations.tolist())
        #reset every timestep
        model.ps_kl = torch.tensor([])
        model.ps_pp = torch.tensor([])

        for i in range(0, n_possible_observations): 
            

            model.current_ps_obs = model.possible_observations[i]
            model.ps_likelihood = compute_prob.score_likelihood(model, params, hypothetical_obs=True, test = True)
            model.ps_posterior = compute_prob.score_posterior(model, params, hypothetical_obs=True)

            cur_ps_kl = compute_prob.kl_div(model.ps_posterior, model.all_posterior[model.current_t])
            model.ps_kl = torch.cat((model.ps_kl,cur_ps_kl.unsqueeze(0)))
            model.all_ps_kl[model.current_t] = model.ps_kl.tolist()


            cur_ps_pp = compute_prob.score_post_pred(model.current_ps_obs, model, params)
            model.ps_pp = torch.cat((model.ps_pp, cur_ps_pp.unsqueeze(0)))
            model.all_ps_pp[model.current_t] = model.ps_pp.tolist()

            model.ps_likelihood = None
            model.ps_posterior = None
            
        eig = torch.sum(model.ps_kl * model.ps_pp)

       

        model.update_model_eig(eig.item())
        if (eig < params.world_EIGs): 
            stimulus_idx = stimulus_idx + 1
            model.update_model_decision(True)
        else: 
            model.update_model_decision(False)

        # then calculate EIG 
        t = t + 1








