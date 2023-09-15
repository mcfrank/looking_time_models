# no learning model and no noise model 


import torch
from . import compute_prob_tensor
import numpy as np
import ipdb

def granch_no_learning_simulation(params, model, stimuli): 

    stimulus_idx = 0
    t = 0 # following python tradition we are using 0-indexed
    current_stim_t = 0
    while t < params.max_observation and stimulus_idx < stimuli.n_trial: 

        # update model behavior with current t and current stimulus_idx 
        model.current_t = t 
        model.current_stimulus_idx = stimulus_idx
    
        # update model stimulus id 
        # update the noisy observation on current model stimulus 
        model.update_model_stimulus_id()
        

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
                p_look_away = np.random.uniform(0, 1)
                #p_look_away = params.world_EIGs / (eig.item() + params.world_EIGs)

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
            p_look_away = np.random.uniform(0, 1)
    
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









