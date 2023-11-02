from datetime import datetime 
import numpy as np
from torch.distributions import Normal, uniform
import torch.distributions as dist
import torch
#import init_params_tensor
#import init_model_tensor
#import main_sim_tensor
import pandas as pd
import pickle
from . import init_model_tensor, main_sim_tensor, init_params_tensor, compute_prob_tensor, lesioned_sim, proxy_sim
import gc
import ipdb

def run_all_sim(
        EXP_INFO,
        BATCH_GRID_INFO, 
        PRIOR_INFO, 
        STIMULI_INFO, 
        MODEL_TYPE = "normal"): 
    device = 'cuda' if torch.cuda.is_available() else 'cpu'

    # get stimulus set & paradigm 
    stim_set = EXP_INFO['stim_set']
    paradigm = EXP_INFO['paradigm']

    # access all the priors 
    mu_prior = PRIOR_INFO["mu_prior"]
    V_prior = PRIOR_INFO["V_prior"]
    alpha_prior = torch.tensor([PRIOR_INFO["alpha_prior"]]).to(device)
    beta_prior = torch.tensor([PRIOR_INFO["beta_prior"]]).to(device)
    epsilon = PRIOR_INFO["epsilon"]
    mu_epsilon = torch.tensor([PRIOR_INFO["mu_epsilon"]]).to(device)
    sd_epsilon = torch.tensor([PRIOR_INFO["sd_epsilon"]]).to(device)
    world_EIGs = PRIOR_INFO["world_EIGs"]
    hypothetical_obs_grid_n = PRIOR_INFO["hypothetical_obs_grid_n"]
    max_observation = PRIOR_INFO["max_observation"]
    forced_exposure_max = PRIOR_INFO["forced_exposure_max"]
    linking_hypothesis = PRIOR_INFO["linking_hypothesis"]

    tensor_stimuli = STIMULI_INFO
    tensor_model =  init_model_tensor.granch_model(max_observation, tensor_stimuli)
    # access all the grid_info 

    for b_i in range(0, BATCH_GRID_INFO["total_batch_n"]): 
        res_df = pd.DataFrame()
        for i in range(0,BATCH_GRID_INFO["jitter_n"]): 
            tensor_model =  init_model_tensor.granch_model(max_observation, tensor_stimuli)

            index = b_i * BATCH_GRID_INFO["jitter_n"] + i

            params = init_params_tensor.granch_params(
                grid_mu =  BATCH_GRID_INFO["grid_mus"][index].to(device),
                grid_sigma = BATCH_GRID_INFO["grid_sigmas"][index].to(device),
                grid_y = BATCH_GRID_INFO["grid_ys"][index].to(device),
                grid_epsilon = BATCH_GRID_INFO["grid_epsilons"][index].to(device),
                hypothetical_obs_grid_n = hypothetical_obs_grid_n, 
                mu_prior = mu_prior,
                V_prior = V_prior, 
                alpha_prior = alpha_prior, 
                beta_prior = beta_prior,
                epsilon  = epsilon, 
                mu_epsilon = mu_epsilon, 
                sd_epsilon = sd_epsilon, 
                world_EIGs = world_EIGs,
                max_observation = max_observation,
                forced_exposure_max = forced_exposure_max, 
                linking_hypothesis = linking_hypothesis)
        
            # add the various different cached bits
            params.add_meshed_grid()
            params.add_lp_mu_sigma()
            params.add_y_given_mu_sigma()
            params.add_lp_epsilon()
            params.add_priors()

            if MODEL_TYPE == "normal" and linking_hypothesis == "EIG": 
                res = main_sim_tensor.granch_main_simulation(params, tensor_model, tensor_stimuli)
            elif MODEL_TYPE == "normal" and (linking_hypothesis == "surprisal" or linking_hypothesis == "KL"):
                res = proxy_sim.granch_proxy_sim(params, tensor_model, tensor_stimuli)
            elif MODEL_TYPE == "no_learning": 
                res = lesioned_sim.granch_no_learning_simulation(params, tensor_model, tensor_stimuli)
            elif MODEL_TYPE == "no_noise": 
                res = lesioned_sim.granch_no_noise_simulation(params, tensor_model, tensor_stimuli) 

            b = res.behavior
            b["j_i"] = i

            res_df = pd.concat([res_df, b])

        # Cache each batch 
        res_df["b_val"] = STIMULI_INFO.b_val.iloc[0].at[1]
        res_df["d_val"] = STIMULI_INFO.d_val.iloc[0].at[1]
        #res_df["mu_prior"] = mu_prior 
        res_df["v_prior"] = V_prior     
        res_df["alpha_prior"] = alpha_prior.item()
        res_df["beta_prior"] = beta_prior.item()
        #res_df["epsilon"] = epsilon
        res_df["weig"] = world_EIGs
        res_df["stim_squence"] = tensor_stimuli.sequence_scheme
        res_df["forced_exposure_max"] = forced_exposure_max
        res_df["linking_hypothesis"] = linking_hypothesis[0] # only taking first value to reduce file size
        res_df['sd_epsilon'] = sd_epsilon.item()

        if stim_set == "spore":
            res_df["complexity_type"] = tensor_stimuli.complexity_type
            res_df["violation_type"] = "identity"

        else:
            res_df["violation_type"] = tensor_stimuli.violation_type[0]

        curr_time = datetime.now()
        timestr = curr_time.strftime('%m-%d-%H:%M:%S.%f')[:-3] + linking_hypothesis
        
        batch_name = "04_pyGRANCH_multi_cluster/cache_results/{t}.pickle".format(t = timestr)
        with open(batch_name, 'wb') as f:
            pickle.dump(res_df, f)
        del res_df
       
        # garbage collector
        gc.collect()
        torch.cuda.empty_cache()

    return 



def add_jitter(val, jitter_range, jitter_n): 
    return (np.random.uniform(low = val-jitter_range, high = val+jitter_range,size = jitter_n))


def get_batch_grid(BATCH_INFO, 
                   GRID_INFO):
    
    if BATCH_INFO["jitter_mode"] == "sampling": 
        grid_mu_distribution = uniform.Uniform(GRID_INFO["grid_mu_start"], GRID_INFO["grid_mu_end"])
        grid_sigma_distribution = uniform.Uniform(max(0.0000001, GRID_INFO["grid_sigma_start"]), GRID_INFO["grid_sigma_end"])
        grid_y_distribution = uniform.Uniform(GRID_INFO["grid_y_start"], GRID_INFO["grid_y_end"])
        #grid_epsilon_distribution = uniform.Uniform(max(0.0000001, GRID_INFO["grid_epsilon_start"]), GRID_INFO["grid_epsilon_end"])
        grid_epsilon_distribution = uniform.Uniform(max( 0.0000000000000000000000000000001, GRID_INFO["grid_epsilon_start"]), GRID_INFO["grid_epsilon_end"])
        batch_grid = {
            "total_batch_n": BATCH_INFO["total_batch_n"],
            "jitter_n": BATCH_INFO["jitter_n"]
        }
        batch_grid["grid_mus"] = []
        batch_grid["grid_sigmas"] = []
        batch_grid["grid_ys"] = []
        batch_grid["grid_epsilons"] = []

        for i in range(0, batch_grid["jitter_n"] * batch_grid["total_batch_n"]): 
            grid_mu = grid_mu_distribution.sample([GRID_INFO["grid_mu_step"], ])
            grid_sigma = grid_sigma_distribution.sample([GRID_INFO["grid_sigma_step"], ])
            grid_y = grid_y_distribution.sample([GRID_INFO["grid_y_step"], ])
            grid_epsilon = grid_epsilon_distribution.sample([GRID_INFO["grid_epsilon_step"], ])

            batch_grid["grid_mus"].append(grid_mu)
            batch_grid["grid_sigmas"].append(grid_sigma)
            batch_grid["grid_ys"].append(grid_y)
            batch_grid["grid_epsilons"].append(grid_epsilon)

    elif BATCH_INFO["jitter_mode"] == "single_end": 

        jitter_range = (GRID_INFO["grid_mu_end"] - GRID_INFO["grid_mu_start"]) / GRID_INFO["grid_mu_step"]
        total_batch_jitter = BATCH_INFO["jitter_n"] * BATCH_INFO["total_batch_n"]

        batch_grid = {
            "total_batch_n": BATCH_INFO["total_batch_n"],
            "jitter_n": BATCH_INFO["jitter_n"],
            "grid_mu_starts": add_jitter(GRID_INFO["grid_mu_start"], jitter_range, total_batch_jitter), 
            "grid_mu_step":  GRID_INFO["grid_mu_step"],
            "grid_sigma_starts": add_jitter(GRID_INFO["grid_sigma_start"], jitter_range, total_batch_jitter), 
            "grid_sigma_step":  GRID_INFO["grid_sigma_step"],
            "grid_y_starts": add_jitter(GRID_INFO["grid_y_start"], jitter_range, total_batch_jitter), 
            "grid_y_step":  GRID_INFO["grid_y_step"],
            "grid_epsilon_starts": add_jitter(GRID_INFO["grid_epsilon_start"], jitter_range, total_batch_jitter),
            "grid_epsilon_step": GRID_INFO["grid_epsilon_step"]
        }
        batch_grid["grid_mu_ends"] = batch_grid["grid_mu_starts"] + (GRID_INFO["grid_mu_end"] - GRID_INFO["grid_mu_start"])
        batch_grid["grid_sigma_ends"] = batch_grid["grid_sigma_starts"] + (GRID_INFO["grid_sigma_end"] - GRID_INFO["grid_sigma_start"])
        batch_grid["grid_y_ends"] = batch_grid["grid_y_starts"] + (GRID_INFO["grid_y_end"] - GRID_INFO["grid_y_start"])
        batch_grid["grid_epsilon_ends"] = batch_grid["grid_epsilon_starts"] + (GRID_INFO["grid_epsilon_end"] - GRID_INFO["grid_epsilon_start"])

        # create all the jitter tensor here: 
        batch_grid["grid_mus"] = []
        batch_grid["grid_sigmas"] = []
        batch_grid["grid_ys"] = []
        batch_grid["grid_epsilons"] = []


        for i in range(0, batch_grid["jitter_n"] * batch_grid["total_batch_n"]): 
            grid_mu = torch.linspace(start = batch_grid["grid_mu_starts"][i], 
                                     end = batch_grid["grid_mu_ends"][i], 
                                     steps = batch_grid["grid_mu_step"])
            grid_sigma = torch.linspace(start = batch_grid["grid_sigma_starts"][i], 
                                     end = batch_grid["grid_sigma_ends"][i], 
                                     steps = batch_grid["grid_sigma_step"])
            grid_y = torch.linspace(start = batch_grid["grid_y_starts"][i], 
                                     end = batch_grid["grid_y_ends"][i], 
                                     steps = batch_grid["grid_y_step"])
            grid_epsilon = torch.linspace(start = batch_grid["grid_epsilon_starts"][i], 
                                     end = batch_grid["grid_epsilon_ends"][i], 
                                     steps = batch_grid["grid_epsilon_step"])

            batch_grid["grid_mus"].append(grid_mu)
            batch_grid["grid_sigmas"].append(grid_sigma)
            batch_grid["grid_ys"].append(grid_y)
            batch_grid["grid_epsilons"].append(grid_epsilon)

    return batch_grid

def sample_baby_stims(pair_each_stim, n_feature):

    all_deviant_blocks = ["B"*num_fam + "D" for num_fam in range(1,10)]
    all_background_blocks = ["B"*num_fam for num_fam in range(1,11)]

    all_stimuli_info = []

    # loop through everything with deviant blocks 
    for i in range(pair_each_stim): 
        for s_type in all_deviant_blocks: 
            s = init_model_tensor.granch_stimuli(1, s_type)
            s.get_baby_exposure_duration_pairings("04_pyGRANCH_multi_cluster/embeddings/exposdur_afterPCA.csv", "identity", n_feature)
            
            all_stimuli_info.extend([s])

    # then go through the background blocks 
    for i in range(pair_each_stim): 
        for s_type in all_background_blocks: 
            s = init_model_tensor.granch_stimuli(1, s_type)
            s.get_baby_exposure_duration_pairings("04_pyGRANCH_multi_cluster/embeddings/exposdur_afterPCA.csv", "identity", n_feature)
            all_stimuli_info.extend([s])

    return all_stimuli_info


def sample_spore_experiment(pair_each_stim, n_feature):
   
    all_complexity_type = ["complex", "simple"]
    all_background_blocks = ["BBBBBB", "BBBBBD", "BBBDBB", "BDBBBB"]

    all_stimuli_info = []

    # loop through everything with deviant blocks 
    for i in range(pair_each_stim): 
        for c_type in all_complexity_type: 
            for s_type in all_background_blocks: 
                s = init_model_tensor.granch_stimuli(n_feature, s_type)
                s.get_spore_stimuli_sequence("embeddings/spores_embeddings_afterPCA.csv", c_type)
                all_stimuli_info.extend([s])

    return all_stimuli_info

def sample_condition_experiment(pair_each_stim, paradigm, n_feature):

    if paradigm == "adult":
   
        all_violation_type = ["animacy", "number", "pose", "identity"]
        all_deviant_blocks = [ "BD", "BBBD", "BBBBBD"]
        all_background_blocks = ["BB", "BBBB", "BBBBBB"]

        all_stimuli_info = []

        # loop through everything with deviant blocks 
        for i in range(pair_each_stim): 
            for v_type in all_violation_type: 
                for s_type in all_deviant_blocks: 
                    s = init_model_tensor.granch_stimuli(n_feature, s_type)
                    s.get_violation_stimuli_sequence("04_pyGRANCH_multi_cluster/embeddings/unity_embeddings_afterPCA.csv", v_type)
                    
                    all_stimuli_info.extend([s])

        # then go through the background blocks 
        for i in range(pair_each_stim): 
            for s_type in all_background_blocks: 
                s = init_model_tensor.granch_stimuli(n_feature, s_type)
                # just put one because it doesn't really matter not gonna use the d
                s.get_violation_stimuli_sequence("04_pyGRANCH_multi_cluster/embeddings/unity_embeddings_afterPCA.csv", "animacy")
                all_stimuli_info.extend([s])

    if paradigm == "infant":
        all_violation_type = ["identity"]
        all_deviant_blocks = ["B"*num_fam + "D" for num_fam in range(1,10)]
        all_background_blocks = ["B"*num_fam for num_fam in range(1,11)]

        all_stimuli_info = []

        # loop through everything with deviant blocks 
        for i in range(pair_each_stim): 
            for v_type in all_violation_type: 
                for s_type in all_deviant_blocks: 
                    s = init_model_tensor.granch_stimuli(1, s_type)
                    s.get_violation_stimuli_sequence("04_pyGRANCH_multi_cluster/embeddings/unity_embeddings_afterPCA.csv", v_type)
                    
                    all_stimuli_info.extend([s])

        # then go through the background blocks 
        for i in range(pair_each_stim): 
            for s_type in all_background_blocks: 
                s = init_model_tensor.granch_stimuli(1, s_type)
                # just put one because it doesn't really matter not gonna use the d
                s.get_violation_stimuli_sequence("04_pyGRANCH_multi_cluster/embeddings/unity_embeddings_afterPCA.csv", "animacy")
                all_stimuli_info.extend([s])


    return all_stimuli_info


def sample_multiple_pair(pair_each_stim, distance_range = []):
   

    all_stimuli_info = []
    for i in range(pair_each_stim): 
        s1 = init_model_tensor.granch_stimuli(1, 'BBBBBB')
        s2 = init_model_tensor.granch_stimuli(1, 'BDBBBB')
        s3 = init_model_tensor.granch_stimuli(1, 'BBBDBB')
        s4 = init_model_tensor.granch_stimuli(1, 'BBBBBD')
        s1.get_stimuli_sequence("embedding_PCA.csv", distance_range)
        s2.get_stimuli_sequence("embedding_PCA.csv", distance_range)
        s3.get_stimuli_sequence("embedding_PCA.csv", distance_range)
        s4.get_stimuli_sequence("embedding_PCA.csv", distance_range)
        all_stimuli_info.extend([s1, s2, s3, s4])

    return all_stimuli_info

def set_up_toy_example(b, d): 
    
    s1 = init_model_tensor.granch_stimuli(1, 'BBBBBB')
    s2 = init_model_tensor.granch_stimuli(1, 'BDBBBB')
    s3 = init_model_tensor.granch_stimuli(1, 'BBBDBB')
    s4 = init_model_tensor.granch_stimuli(1, 'BBBBBD')

    s1.add_toy_example(b, d)
    s2.add_toy_example(b, d)
    s3.add_toy_example(b, d)
    s4.add_toy_example(b, d)

    return [s1, s2, s3, s4]

# currently assuming only changing one prior 
def create_prior_list(seed_prior, prior_key, prior_val_list):
    prior_list = []
    prior_list.append(seed_prior)
    for val in prior_val_list: 
        prior = dict(seed_prior)
        prior[prior_key] = val 
        prior_list.append(prior)

    return prior_list




