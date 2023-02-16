
from scipy.stats import norm 
from itertools import repeat 
import pandas as pd
import scipy.stats as sts 
import numpy as np
import torch 
from torch.distributions import Normal  

import helper

# --- major functions -- 

# compute KL divergence
def kl_div(new_post, prev_post): 
    return torch.sum(torch.mul(new_post, 
                         torch.log(new_post/prev_post)))

# score posterior predictive 
def score_post_pred(hypo_obs, model, params): 
    lp_hypo_z_given_mu_sigma_for_y = torch.add(score_z_ij_given_y(hypo_obs,
                                                                   params.meshed_grid_y, 
                                                                   params.meshed_epsilon), 
                                                params.lp_y_given_mu_sigma  
                                                )

    # goal: apply logSumExp based on the grouping of y
    # first we need to putting all the grouping base together 
    # note the order of the tensor matters to provide a grouping base 
    # that algins with lp_z_given_mu_sigma_for_y grouping base 
    grouping_base = torch.cat(
            (
             params.meshed_grid_mu.unsqueeze(1),
             params.meshed_grid_sigma.unsqueeze(1),
            params.meshed_epsilon.unsqueeze(1)
            ),
             dim = 1)

    # crossed checked in R that likelihood_df group by operation 
    # is the same with the one using the homebased function
    hypo_likelihood = helper.group_by_logsumexp(grouping_base, lp_hypo_z_given_mu_sigma_for_y)    
    log_posterior = torch.log(model.all_posterior[model.current_t])
    return (torch.exp(torch.logsumexp(torch.add(hypo_likelihood, log_posterior), 0)))

# score posterior 
def score_posterior(model, params, hypothetical_obs):
   
   if hypothetical_obs: 
        likelihood = model.ps_likelihood
   else: 
        likelihood = model.all_likelihood[model.current_t]

   unlz_p = likelihood + params.lp_epsilon.mean(dim = 2) + params.lp_mu_sigma.mean(dim = 2)
   normalized_posterior = torch.exp(unlz_p - unlz_p.logsumexp(dim = (0, 1, 2)))
   normalized_posterior[normalized_posterior < np.exp(-720)] = 1/(10 ** 320)
   return normalized_posterior

# score likelihood
def score_likelihood(model, params, hypothetical_obs, test = False): 
    
    # if we are calculating EIG from hypothetical obs 
    # then we need to concatenate the hypothetical obs 
    if hypothetical_obs: 
        obs = torch.cat((model.get_all_observations_on_current_stimulus().squeeze(1),
        model.current_ps_obs.unsqueeze(0)),0).unsqueeze(1)
    else: 
        obs = model.get_all_observations_on_current_stimulus()

    # lp(z|mu, sigma^2) = lp(z | y) + lp(y | mu, sigma^2)
    # note that we are using all the observations on the current stimuli z
    # and sum them together


    lp_z_given_mu_sigma_for_y = torch.add(torch.sum(score_z_ij_given_y(obs,
                                                                      params.meshed_grid_y, 
                                                                      params.meshed_epsilon), dim = 0), 
                                                params.lp_y_given_mu_sigma  
                                                )

    # goal: apply logSumExp based on the grouping of y
    likelihood = lp_z_given_mu_sigma_for_y.logsumexp(dim = 2)

    # if(test):

        #print(grouping_base)
        #print(lp_z_given_mu_sigma_for_y)
        #ol = helper.group_by_logsumexp(grouping_base.float(), lp_z_given_mu_sigma_for_y.float())
        #nl = helper.group_by_logsumexp_improved(grouping_base.float(), lp_z_given_mu_sigma_for_y.float())
        #print(ol)
        #print(nl)

    if(model.current_stimulus_idx > 0): 
        likelihood = likelihood + model.get_last_stimuli_likelihood()

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
    padded_obs = helper.add_singleton_dim(z_val, z_val.size()[0])
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

def score_mu_sigma(input_x, input_sigma, mu, nu, alpha, beta, log = True):
    '''
    The probability density function of the normal-inverse-gamma distribution at
    input_x (mean) and input_sigma (variance).
    '''

    res = (
        sts.norm.pdf(input_x.numpy(), loc=mu, scale=np.sqrt(input_sigma**2 / nu)) *
        sts.invgamma.pdf(input_sigma.numpy()**2, a=alpha, scale=beta))

    return torch.from_numpy(np.log(res)) if log else torch.from_numpy(res) 

def score_epsilon(epsilon, mu_epsilon, sd_epsilon): 
    return Normal(mu_epsilon, sd_epsilon).log_prob(epsilon)
