
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
    lp_hypo_z_given_mu_sig_sq_for_y = torch.add(score_z_ij_given_y(hypo_obs,
                                                                   params.meshed_grid_y, 
                                                                   params.meshed_epsilon), 
                                                params.lp_y_given_mu_sig_sq  
                                                
                                                )

    # goal: apply logSumExp based on the grouping of y
    # first we need to putting all the grouping base together 
    # note the order of the tensor matters to provide a grouping base 
    # that algins with lp_z_given_mu_sig_sq_for_y grouping base 
    grouping_base = torch.cat(
            (
             params.meshed_grid_mu_theta.unsqueeze(1),
             params.meshed_grid_sig_sq.unsqueeze(1),
            params.meshed_epsilon.unsqueeze(1)
            ),
             dim = 1)

    # crossed checked in R that likelihood_df group by operation 
    # is the same with the one using the homebased function
    hypo_likelihood = helper.group_by_logsumexp(grouping_base, lp_hypo_z_given_mu_sig_sq_for_y)    
    log_posterior = torch.log(model.all_posterior[model.current_t])
    #print(torch.logsumexp(torch.add(hypo_likelihood, log_posterior), 0))

    return (torch.exp(torch.logsumexp(torch.add(hypo_likelihood, log_posterior), 0)))

# score posterior 
def score_posterior(model, params, hypothetical_obs):
   
   if hypothetical_obs: 
        likelihood = model.ps_likelihood
   else: 
        likelihood = model.all_likelihood[model.current_t]


   unlz_p = torch.add(torch.add(likelihood, params.prior_lp_epsilon), 
                        params.prior_lp_mu_sig_sq)
   normalized_posterior = torch.exp(unlz_p - torch.logsumexp(unlz_p, 0))

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

    # lp(z|mu, sig^2) = lp(z | y) + lp(y | mu, sig^2)
    # note that we are using all the observations on the current stimuli z
    # and sum them together 

    res = score_z_ij_given_y(obs, params.meshed_grid_y, params.meshed_epsilon)
    
    lp_z_given_mu_sig_sq_for_y = torch.add(torch.sum(score_z_ij_given_y(obs,
                                                                      params.meshed_grid_y, 
                                                                      params.meshed_epsilon), dim = 0), 
                                                params.lp_y_given_mu_sig_sq  
                                                )
    
    #print(lp_z_given_mu_sig_sq_for_y)
    
    # goal: apply logSumExp based on the grouping of y

    # first we need to putting all the grouping base together 
    # note the order of the tensor matters to provide a grouping base 
    # that algins with lp_z_given_mu_sig_sq_for_y grouping base 
    grouping_base = torch.cat(
            (
             params.meshed_grid_mu_theta.unsqueeze(1),
             params.meshed_grid_sig_sq.unsqueeze(1),
            params.meshed_epsilon.unsqueeze(1)
            ),
             dim = 1)

   
    # crossed checked in R that likelihood_df group by operation 
    # is the same with the one using the homebased function
    likelihood = helper.group_by_logsumexp(grouping_base.float(), lp_z_given_mu_sig_sq_for_y.float())
    

    if(model.current_stimulus_idx > 0): 
        likelihood = likelihood + model.get_last_stimuli_likelihood()
        

    return likelihood


# ---- core function ---- #
# y needs to be a tensor 

def score_y_given_mu_sigma_sq(y_val, mu, sigma): 
    return Normal(mu, sigma).log_prob(y_val)


##a = score_y_given_mu_sigma_sq(y, mu, sigma)
##print(a)
def score_z_ij_given_y(z_val, y_val, epsilon): 
    return Normal(y_val, epsilon).log_prob(z_val)

def score_z_bar_given_y(z_bar, y_val, grid_epsilon): 
    return torch.sum(score_z_ij_given_y(z_bar,y_val,grid_epsilon), dim=0)


# print(score_z_ij_given_y(z, grid_y, grid_epsilon))
# score prior 
# got the function from here: https://notebooks.githubusercontent.com/view/ipynb?color_mode=auto&commit=6817e1bc34e70aa89233bad658b70176178c247d&enc_url=68747470733a2f2f7261772e67697468756275736572636f6e74656e742e636f6d2f676973742f6e696b6974612d6b6f7473656875622f64356361653561646630363166633234313865336464653565323830333138382f7261772f363831376531626333346537306161383932333362616436353862373031373631373863323437642f63733134362d332e312d7072652d636c6173732d776f726b2e6970796e62&logged_in=false&nwo=nikita-kotsehub%2Fd5cae5adf061fc2418e3dde5e2803188&path=cs146-3.1-pre-class-work.ipynb&repository_id=107531601&repository_type=Gist
# will need to double check 

# we don't have a torch equivalence of inverse gamma 
# so we might want to do calculation wi numpy under the hood and then convert to torch 
# the goode news is that this calcualtion doesn't happen very often so it's good
def score_mu_sig_sq(input_x, input_sig_sq, mu, nu, alpha, beta, log = True):
    '''
    The probability density function of the normal-inverse-gamma distribution at
    input_x (mean) and input_sig_sq (variance).
    '''
    res = (
        sts.norm.pdf(input_x, loc=mu, scale=np.sqrt(input_sig_sq ** 2 / nu)) *
        sts.invgamma.pdf(input_sig_sq ** 2, a=alpha, scale=beta))


    return np.log(res) if log else res 

def score_epsilon(epsilon, mu_epsilon, sd_epsilon): 
    return Normal(mu_epsilon, sd_epsilon).log_prob(epsilon)
