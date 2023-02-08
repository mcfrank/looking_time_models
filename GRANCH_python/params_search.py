import itertools
import helper
import numpy as np
import time 

import init_params
import compute_prob
import init_model
import main_sim



def run_parameter_search(all_formatted_params, stimuli):     
    return(list(map(run_model_with_parameter, all_formatted_params, itertools.repeat(stimuli, len(all_formatted_params)))))

def run_model_with_parameter(parameter, stimuli):
    start_time = time.perf_counter()
    p = init_params.granch_params(
      grid_mu_theta = parameter["grid_mu_theta"],
      grid_sig_sq = parameter["grid_sig_sq"], 
      grid_y = parameter["grid_y"], 
      grid_epsilon =  parameter["grid_epsilon"], 
      hypothetical_obs_grid_n = 3, 
      mu_prior = parameter["mu_prior"],
      V_prior = parameter["V_prior"], 
      alpha_prior = parameter["alpha_prior"], 
      beta_prior = parameter["beta_prior"],
      epsilon  = parameter["epsilon"], 
      mu_epsilon = parameter["mu_epsilon"], 
      sd_epsilon = parameter["sd_epsilon"], 
      world_EIGs = parameter["eig"],
      max_observation = 500)
    p.add_meshed_grid()
    p.add_lp_mu_sig_sq()
    p.add_y_given_mu_sig_sq()
    p.add_lp_epsilon()
    p.add_priors()
    s = stimuli
    m = init_model.granch_model(500, s)

    
    main_sim.granch_main_simulation(p, m, s)
    end_time = time.perf_counter()
    return (m.behavior, end_time-start_time)


def get_formatted_parameter(raw_parameter): 
    parameter = {
        "grid_mu_theta": raw_parameter[0][0],
        "grid_sig_sq": raw_parameter[0][1],
        "grid_y": raw_parameter[0][2],
        "grid_epsilon": raw_parameter[0][3],
        "mu_prior": raw_parameter[1][0], 
        "V_prior": raw_parameter[1][1],
        "alpha_prior": raw_parameter[1][2], 
        "beta_prior": raw_parameter[1][3],
        "epsilon": raw_parameter[1][4],
        "mu_epsilon": raw_parameter[1][5],
        "sd_epsilon": raw_parameter[1][6],
        "eig": raw_parameter[1][7]
    }
    return (parameter)




def set_parameter_search(grid_parameters, priors):
    all_raw_params = [r for r in itertools.product(grid_parameters, priors)]
    all_formatted_params = list(map(get_formatted_parameter, all_raw_params))
    return (all_formatted_params)

def set_granch_grid_parameter(grid_mu_theta_starts, grid_mu_theta_ends, grid_mu_theta_steps, 
                              grid_sig_sq_starts, grid_sig_sq_ends, grid_sig_sq_steps, 
                              grid_y_starts, grid_y_ends, grid_y_steps,
                              grid_epsilon_starts, grid_epsilon_ends, grid_epsilon_steps):
    
    grid_mu_theta_options = helper.get_grid_parameter_combination(grid_mu_theta_starts, grid_mu_theta_ends,grid_mu_theta_steps)
    grid_sig_sq_options = helper.get_grid_parameter_combination(grid_sig_sq_starts, grid_sig_sq_ends, grid_sig_sq_steps)
    grid_y_options = helper.get_grid_parameter_combination(grid_y_starts, grid_y_ends, grid_y_steps)
    grid_epsilon_options = helper.get_grid_parameter_combination(grid_epsilon_starts, grid_epsilon_ends, grid_epsilon_steps)

    grid_mu_thetas = helper.get_grid_parameter_tensors(grid_mu_theta_options)
    grid_sig_sqs = helper.get_grid_parameter_tensors(grid_sig_sq_options)
    grid_ys = helper.get_grid_parameter_tensors(grid_y_options)
    grid_epsilons = helper.get_grid_parameter_tensors(grid_epsilon_options)
    
    all_grid_combo = list(itertools.product(*[grid_mu_thetas, grid_sig_sqs, grid_ys,grid_epsilons]))

    return (all_grid_combo)


# would want to specify a range for prior
def set_granch_priors(mu_priors, V_priors, alpha_priors, beta_priors, epsilons, 
                      mu_epsilons, sd_epsilons, eigs):
    all_priors = np.stack(np.meshgrid(mu_priors, V_priors, alpha_priors, beta_priors, 
                                     epsilons,mu_epsilons,sd_epsilons, eigs), -1).reshape(-1, 8)
    return(all_priors)
