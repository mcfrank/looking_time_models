


def get_param_list(raw_param_values):
    param_list = []
    for param in raw_param_values: 
        PRIOR_INFO = {
            "mu_prior": param[0],  
            "V_prior": param[1], 
            "alpha_prior": param[2], 
            "beta_prior": param[3], 
            "epsilon": param[4], "mu_epsilon": param[5], "sd_epsilon": param[6], 
            "hypothetical_obs_grid_n":  param[7],
            "world_EIGs": param[8], "max_observation":  param[9], 
            "forced_exposure_max": param[10], 
            "linking_hypothesis":"EIG"
            }
        param_list.append(PRIOR_INFO)
    return param_list


