
from scipy.stats import norm 
from itertools import repeat 
import pandas as pd
import scipy.stats as sts 
import numpy as np
import torch 
from torch.distributions import Normal  
from . import helper

# --- major functions -- 

# compute KL divergence
def kl_div(new_post, prev_post): 
    # this is to make sure the prev post is of the same dimension as the new post
    paded_prev_post = prev_post.repeat(new_post.size()[0], 1, 1, 1)

    return torch.sum(torch.mul(new_post, 
                         torch.log(new_post/paded_prev_post)), dim = (1, 2, 3))

# score posterior predictive 
def score_post_pred(model, params): 
    obs = model.possible_observations
    res = score_z_ij_given_y(obs, params.meshed_grid_y,  params.meshed_grid_epsilon)
    padded_lp_y_given_mu_sigma = params.lp_y_given_mu_sigma.repeat(res.size()[0], 1, 1, 1, 1)
    lp_hypo_z_given_mu_sigma_for_y = res + padded_lp_y_given_mu_sigma

    # goal: apply logSumExp based on the grouping of y
    hypo_likelihood =  torch.logsumexp(lp_hypo_z_given_mu_sigma_for_y, dim = 3)
    log_posterior = torch.log(model.cur_posterior )

    padded_log_posterior = log_posterior.repeat(obs.size()[0], 1, 1, 1)
    return (torch.exp(torch.logsumexp(torch.add(hypo_likelihood, padded_log_posterior), dim = (1, 2,3))))

# score posterior 
def score_posterior(model, params, hypothetical_obs):
   
   if hypothetical_obs: 
        likelihood = model.ps_likelihood
        unlz_p = likelihood + params.lp_epsilon.mean(dim = 2) + params.lp_mu_sigma.mean(dim = 2)

        logsumxp_dim = (1, 2, 3)
        normalizing_term = helper.add_singleton_dim(unlz_p.logsumexp(dim = logsumxp_dim), 3)
   else: 
        
        likelihood = model.cur_likelihood 
        unlz_p = likelihood + params.lp_epsilon.mean(dim = 2) + params.lp_mu_sigma.mean(dim = 2)

        logsumxp_dim = (0, 1, 2)
        normalizing_term = unlz_p.logsumexp(dim = logsumxp_dim)

   normalized_posterior = torch.exp(unlz_p - normalizing_term)
   normalized_posterior[normalized_posterior < np.exp(-720)] = 1/(10 ** 320)
   return normalized_posterior



# score likelihood
def score_likelihood(model, params, hypothetical_obs): 
    
    # if we are calculating EIG from hypothetical obs 
    # then we need to concatenate the hypothetical obs 
    if hypothetical_obs: 
        current_obs = model.get_all_observations_on_current_stimulus()
        ps_obs = model.possible_observations
        # needs to be changed when incorporating multiple feature
        current_obs = current_obs.flatten(start_dim = 0)
        obs = torch.cat([current_obs.repeat(ps_obs.size()[0], 1), ps_obs.unsqueeze(1)], dim = 1)
        z_ij_collapse_dim = 1
        y_dim = 3

    else: 
        obs = model.get_all_observations_on_current_stimulus()
        z_ij_collapse_dim = 0
        y_dim = 2

    # lp(z|mu, sigma^2) = lp(z | y) + lp(y | mu, sigma^2)
    # note that we are using all the observations on the current stimuli z
    # and sum them together
    # z_ij_collapse_dim is difference for hypothetical obs and actual observation



    lp_z_given_mu_sigma_for_y = torch.add(torch.sum(score_z_ij_given_y(obs,
                                                                      params.meshed_grid_y, 
                                                                      params.meshed_grid_epsilon), dim =  z_ij_collapse_dim).squeeze(), 
                                                params.lp_y_given_mu_sigma  
                           
                                             )    

    # goal: apply logSumExp based on the grouping of y
    likelihood = lp_z_given_mu_sigma_for_y.logsumexp(dim = y_dim)
    

    if(model.current_stimulus_idx > 0): 

        if hypothetical_obs: 
            padded_last_stimuli_likelihood = model.prev_likelihood.repeat(likelihood.size()[0], 1, 1, 1)
            likelihood = likelihood + padded_last_stimuli_likelihood
        
        else: 
            # likelihood from all observations on the current simulus + likelihood from past stimuli
            likelihood = likelihood +model.prev_likelihood




    return likelihood


# ---- core function ---- #


# y needs to be a tensor

def score_y_given_mu_sigma(y_val, mu, sigma): 
    return Normal(mu, sigma).log_prob(y_val)


## = score_y_given_mu_sigmama_sq(y, mu, sigmama)
# following the guidance here: https://github.com/pytorch/pytorch/issues/76709
# needs to wrangle the shape of the z value

def score_z_ij_given_y(z_val, y_val, epsilon):
    dist = Normal(y_val, epsilon)
    # add 4 dimension because the grid are four dimension
    # when padded 4 dimension singleton dimension the tensor became broadcastable
    padded_obs = helper.add_singleton_dim(z_val,4)
    res = dist.expand((1,) + y_val.size()).log_prob(padded_obs)      
    return res

def score_z_bar_given_y(z_bar, y_val, grid_epsilon): 
    return torch.sum(score_z_ij_given_y(z_bar,y_val,grid_epsilon), dim=0)


# print(score_z_ij_given_y(z, grid_y, grid_epsilon))
# score prior 
# got the function from here: https://notebooks.githubusercontent.com/view/ipynb?color_mode=auto&commit=6817e1bc34e70aa89233bad658b70176178c247d&enc_url=68747470733a2f2f7261772e67697468756275736572636f6e74656e742e636f6d2f676973742f6e696b6974612d6b6f7473656875622f64356361653561646630363166633234313865336464653565323830333138382f7261772f363831376531626333346537306161383932333362616436353862373031373631373863323437642f63733134362d332e312d7072652d636c6173732d776f726b2e6970796e62&logged_in=false&nwo=nikita-kotsehub%2Fd5cae5adf061fc2418e3dde5e2803188&path=cs146-3.1-pre-class-work.ipynb&repository_id=107531601&repository_type=Gist
# will need to double check 

# we don't have a torch equivalence of inverse gamma 
# so we might want to do calculation wi numpy under the hood and then convert to torch 
# the goode news is that this calcualtion doesn't happen very often so it's good

# NOTE: we are using numpy here, and these functions take VARIANCE (sigma^2) not STD (sigma), hence
# the use of **2 for each of htem

def score_mu_sigma(input_x, input_sigma, mu, nu, alpha, beta, device):
    '''
    The probability density function of the normal-inverse-gamma distribution at
    input_x (mean) and input_sigma (variance).
    '''

    inv_gamma_distribution = helper.InverseGamma(alpha, beta)

    res = (
        Normal(mu, torch.sqrt(input_sigma ** 2 / nu)).log_prob(input_x)  + 
        inv_gamma_distribution.log_prob(input_sigma ** 2)
    )
    
    return res

def score_epsilon(epsilon, mu_epsilon, sd_epsilon): 
    return Normal(mu_epsilon, sd_epsilon).log_prob(epsilon)
