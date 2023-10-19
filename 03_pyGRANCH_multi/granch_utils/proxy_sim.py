
import torch
from . import compute_prob_tensor
import numpy as np
import pandas as pd


def granch_proxy_sim(params, model, stimuli): 

    kl_test_df = pd.DataFrame(None, index=np.arange(model.max_observation),
                                     columns=["t", "kl1", "kl2"])

    prev_observation_posterior = None
    stimulus_idx = 0
    t = 0 # following python tradition we are using 0-indexed
    current_stim_t = 0
    while t < params.max_observation and stimulus_idx < stimuli.n_trial: 
        # update model behavior with current t and current stimulus_idx 
        model.current_t = t 
        model.current_stimulus_idx = stimulus_idx
       
       #t = 0

        if model.current_t == 0: 
            prior =  params.lp_epsilon.mean(dim = 2) + params.lp_mu_sigma.mean(dim = 2)
            padded_prior = prior.unsqueeze(0).repeat(3, 1, 1, 1)   
            normalizing_term = torch.logsumexp(padded_prior, dim = (1, 2, 3)).view(3, 1, 1, 1)
            normalized_prior= torch.exp(padded_prior - normalizing_term)
            normalized_prior[normalized_prior < 0.0000000000000000000000000000001] = 0.0000000000000000000000000000001
            #print(normalized_prior[normalized_prior < np.exp(-720)])
            #print(normalized_prior[normalized_prior < 0.00000000000001])
            prev_observation_posterior = normalized_prior
            #print(normalized_prior)

        else: 
            prev_observation_posterior = model.cur_posterior

        # get all possible observation on current stimulus 
        # if we change stimulus 
        if model.current_t == 0 or (not model.if_same_stimulus_as_previous_t()): 

            model.update_possible_observations(params.epsilon, params.hypothetical_obs_grid_n)

            # update the previous likelihood to be the "current likelihood"
            model.prev_likelihood = model.cur_likelihood

        # update model stimulus id 
        # update the noisy observation on current model stimulus 
        model.update_model_stimulus_id()
        model.update_noisy_observation(params.epsilon)

        # can calculate surprisal here 
        #SURPRISAL
        surprisal = compute_prob_tensor.score_surprisal(model, params, prev_observation_posterior)
        
        model.update_model_surprisal(surprisal.item())

        current_likelihood = compute_prob_tensor.score_likelihood(model, params, hypothetical_obs=False)
        model.cur_likelihood = current_likelihood
        
        current_posterior = compute_prob_tensor.score_posterior(model,params, hypothetical_obs=False)
        model.cur_posterior = current_posterior     
        
        # CAN CALCULATE KL HERE
        kl = compute_prob_tensor.kl_div(model.cur_posterior, prev_observation_posterior, context = "proxy")
        
        kl_df = compute_prob_tensor.kl_div_test(t, kl_test_df, model.cur_posterior, prev_observation_posterior, context = "proxy")
        #print(kl_df)
        #print(kl)
        #print(torch.sum(kl))

        
        # if forced exposure is not nan
        if ~np.isnan(params.forced_exposure_max): 
            # if it's not the last trial, you still have to look
            if (stimulus_idx < (stimuli.n_trial - 1)) & (current_stim_t < params.forced_exposure_max - 1):
                model.update_model_decision(False)

            # if i'm in a fam trial and i reached the max exposure, i have to look away (to go to next stimulus)
            elif (stimulus_idx < (stimuli.n_trial - 1)) & (current_stim_t == params.forced_exposure_max - 1):
                model.update_model_decision(True)
                stimulus_idx += 1
                current_stim_t = -1 

            else:
                p_look_away = max(min(params.world_EIGs / (eig.item() + params.world_EIGs), 1), 0)
                #p_look_away = params.world_EIGs / (eig.item() + params.world_EIGs)
                    
                if (np.random.binomial(1, p_look_away) == 1): 
            # if the model is looking away, increment stimulus
                    stimulus_idx = stimulus_idx + 1
                    current_stim_t = -1 # -1 so it starts with 0 when incremented 
                    model.update_model_decision(True)
                else: 
                # otherwise keep looking at this one
                    model.update_model_decision(False)
                

        # if it's a self-paced paradigm
        else:
            # luce's choice rule 
            p_look_away = max(min(params.world_EIGs / (surprisal.item() + params.world_EIGs), 1), 0)
            #p_look_away = params.world_EIGs / (eig.item() + params.world_EIGs)
            
            if (np.random.binomial(1, p_look_away) == 1): 
            # if the model is looking away, increment stimulus
                stimulus_idx = stimulus_idx + 1
                current_stim_t = -1 # -1 so it starts with 0 when incremented 
                model.update_model_decision(True)
            else: 
            # otherwise keep looking at this one
                model.update_model_decision(False)

        t += 1  
        current_stim_t += 1 

    #return(model)
    return kl_df