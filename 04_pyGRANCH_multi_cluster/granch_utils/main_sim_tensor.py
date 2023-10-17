# version of the main simulation to support matrix multiplication instead

import torch
from . import compute_prob_tensor
import numpy as np
#import ipdb

# main simulation function
def granch_main_simulation(params, model, stimuli): 

    stimulus_idx = 0
    t = 0 # following python tradition we are using 0-indexed
    current_stim_t = 0
    while t < params.max_observation and stimulus_idx < stimuli.n_trial: 

        # update model behavior with current t and current stimulus_idx 
        model.current_t = t 
        model.current_stimulus_idx = stimulus_idx
    
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

        current_likelihood = compute_prob_tensor.score_likelihood(model, params, hypothetical_obs=False)
        model.cur_likelihood = current_likelihood
        
        current_posterior = compute_prob_tensor.score_posterior(model,params, hypothetical_obs=False)
        model.cur_posterior = current_posterior     
        

        # in the tensor mode we don't need to iterate through possibilities anymore
        model.ps_likelihood = compute_prob_tensor.score_likelihood(model, params, hypothetical_obs=True)
        model.ps_posteriror = compute_prob_tensor.score_posterior(model, params, hypothetical_obs=True)
        model.ps_kl = compute_prob_tensor.kl_div(model.ps_posteriror, model.cur_posterior)
        model.ps_pp = compute_prob_tensor.score_post_pred(model, params)
       
        # compute EIG
        print("eig kl")
        print(model.ps_kl)
        print(model.ps_pp)
        print(torch.sum(model.ps_kl * model.ps_pp))
        eig = torch.sum(model.ps_kl * model.ps_pp)

       
        
        
        # threshold at 0 for now to deal with negative EIG's
        #eig = torch.clamp(eig, min=0)

        model.update_model_eig(eig.item())

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

                if not ((p_look_away >= 0) & (p_look_away <= 1)):
                    print("p_look_away")
                    print(p_look_away)
                    print("params.world_EIGs")
                    print(params.world_EIGs)
                    print("eig.item()")
                    print(eig.item())
                    
                if (np.random.binomial(1, p_look_away) == 1): 
            # if the model is looking away, increment stimulus
                    stimulus_idx = stimulus_idx + 1
                    current_stim_t = -1 # -1 so it starts with 0 when incremented 
                    model.update_model_decision(True)
                else: 
                # otherwise keep looking at this one
                    model.update_model_decision(False)
                
                # if (eig < params.world_EIGs): 
                # # if EIG below threshold, increment stimulus
                #     stimulus_idx += 1
                #     current_stim_t = -1 # -1 so it starts with 0 when incremented 
                #     model.update_model_decision(True)
                # else: 
                # # otherwise keep looking at this one
                #     model.update_model_decision(False)

        # if it's a self-paced paradigm
        else:
            # luce's choice rule 
            p_look_away = max(min(params.world_EIGs / (eig.item() + params.world_EIGs), 1), 0)
            #p_look_away = params.world_EIGs / (eig.item() + params.world_EIGs)
            
            if not ((p_look_away >= 0) & (p_look_away <= 1)):
                print("p_look_away")
                print(p_look_away)
                print("params.world_EIGs")
                print(params.world_EIGs)
                print("eig.item()")
                print(eig.item())

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

    return(model)








