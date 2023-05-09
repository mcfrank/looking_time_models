import numpy as np
import torch 
import pandas as pd
from pickle import FALSE, TRUE
from scipy.stats import norm 
import itertools
from itertools import repeat 
import scipy.stats as sts 
from torch.distributions import Normal  
import time
import cProfile as profile
import pstats
import seaborn as sns
import importlib
from plotnine import *
import time
from memory_profiler import profile



# for profiling
import cProfile, pstats, io
from pstats import SortKey

# helper functions from within the library
import helper
import init_params_tensor
import init_model_tensor
import main_sim_tensor
import compute_prob_tensor




@profile
def run_model():
    params = init_params_tensor.granch_params(
      grid_mu = torch.linspace(start = -1, end = 1, steps = 20),
      grid_sigma = torch.linspace(start = 0.001, end = 1.8, steps = 20), 
      grid_y = torch.linspace(start = -1, end = 1, steps = 20), 
      grid_epsilon =  torch.linspace(start = 0.001, end = 1.8, steps = 20), 
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
    params.add_lp_mu_sigma()
    params.add_y_given_mu_sigma()
    params.add_lp_epsilon()
    params.add_priors()

    tensor_stimuli = init_model_tensor.granch_stimuli(1, 'BBBBBB')
    tensor_model = init_model_tensor.granch_model(500, tensor_stimuli)
    


    stimulus_idx = 0
    t = 0 # following python tradition we are using 0-indexed
   
    
    while t < params.max_observation and stimulus_idx < tensor_stimuli.n_trial: 
    # update tensor_model behavior with current t and current stimulus_idx 
        tensor_model.current_t = t 
        tensor_model.current_stimulus_idx = stimulus_idx
    
        # get all possible observation on current stimulus 
        # if we change stimulus 
        if tensor_model.current_t == 0 or (not tensor_model.if_same_stimulus_as_previous_t()): 
            tensor_model.update_possible_observations(params.epsilon, params.hypothetical_obs_grid_n)
            # update the previous likelihood to be the "current likelihood"
            tensor_model.prev_likelihood = tensor_model.cur_likelihood


        # update tensor_model stimulus id 
        # update the noisy observation on current tensor_model stimulus 
        tensor_model.update_model_stimulus_id()
        tensor_model.update_noisy_observation(params.epsilon)

        current_likelihood = compute_prob_tensor.score_likelihood(tensor_model, params, hypothetical_obs=False)
        tensor_model.cur_likelihood = current_likelihood
        
        current_posterior = compute_prob_tensor.score_posterior(tensor_model,params, hypothetical_obs=False)
        tensor_model.cur_posterior = current_posterior       

        # this will currently work for only single feature    
        # in the tensor mode we don't need to iterate through possibilities anymore
        tensor_model.ps_likelihood = compute_prob_tensor.score_likelihood(tensor_model, params, hypothetical_obs=True)
        tensor_model.ps_posteriror = compute_prob_tensor.score_posterior(tensor_model, params, hypothetical_obs=True)
        tensor_model.ps_kl = compute_prob_tensor.kl_div(tensor_model.ps_posteriror, tensor_model.cur_posterior)
        tensor_model.ps_pp = compute_prob_tensor.score_post_pred(tensor_model, params)
    
        eig = torch.sum(tensor_model.ps_kl * tensor_model.ps_pp)
        tensor_model.update_model_eig(eig.item())

    
        if (eig < params.world_EIGs): 
        # if EIG below threshold, increment stimulus
            stimulus_idx = stimulus_idx + 1
            tensor_model.update_model_decision(True)
        else: 
        # otherwise keep looking at this one
            tensor_model.update_model_decision(False)

        t = t+1  


    print("finished running")

run_model()