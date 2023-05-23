# version of the main simulation to support matrix multiplication instead

import torch
from granch_utils import compute_prob_tensor


# main simulation function
def granch_main_simulation(params, model, stimuli): 

    stimulus_idx = 0
    t = 0 # following python tradition we are using 0-indexed
   
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

        # this will currently work for only single feature    
        # in the tensor mode we don't need to iterate through possibilities anymore
        model.ps_likelihood = compute_prob_tensor.score_likelihood(model, params, hypothetical_obs=True)
        model.ps_posteriror = compute_prob_tensor.score_posterior(model, params, hypothetical_obs=True)
        model.ps_kl = compute_prob_tensor.kl_div(model.ps_posteriror, model.cur_posterior)
        model.ps_pp = compute_prob_tensor.score_post_pred(model, params)
    
        eig = torch.sum(model.ps_kl * model.ps_pp)
        model.update_model_eig(eig.item())

    
        if (eig < params.world_EIGs): 
        # if EIG below threshold, increment stimulus
            stimulus_idx = stimulus_idx + 1
            model.update_model_decision(True)
        else: 
        # otherwise keep looking at this one
            model.update_model_decision(False)

        t = t+1  

   

    return(model)








