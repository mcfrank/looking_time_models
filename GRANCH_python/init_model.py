from distutils.log import warn
from pickle import NONE, TRUE
import pandas as pd
import numpy as np
import torch 
from torch.distributions import Normal  


# currently just using toy number
class granch_stimuli: 
    def __init__(self, n_feature, sequence_scheme): 
        self.n_feature = n_feature 
        self.n_trial = len(sequence_scheme)
        self.sequence_scheme = sequence_scheme

        # currently as a toy example 
        self.stimuli_sequence = {
            0: torch.tensor([0.1]),#torch.rand(self.n_feature), 
            1: torch.tensor([0.1]),#torch.rand(self.n_feature), 
            2: torch.tensor([0.1])#torch.rand(self.n_feature)
        }


    def get_stimuli_sequence(self, embedding_path): 
        embeddings = pd.read_csv(embedding_path, header = None)
        # select a pair to be background and deviant 
        bd_pair = embeddings.sample(2).iloc[:, 0:self.n_feature]
        b = torch.tensor(bd_pair.iloc[0, :])
        d = torch.tensor(bd_pair.iloc[1, :])

        idx = 0 
        stimuli_sequence = {}
        while idx < self.n_trial: 
            if(self.sequence_scheme[idx] == "B"): 
                stimuli_sequence[idx] = b
            elif(self.sequence_scheme[idx] == "D"): 
                stimuli_sequence[idx] = d
            else: 
                warn("Wrong sequence scheme ")
            idx = idx + 1
        
        self.stimuli_sequence = stimuli_sequence









class granch_model: 
    def __init__(self, max_observation, stimuli):

        self.current_t = 0
        self.current_stimulus_idx = 0

        self.stimuli = stimuli

        self.max_observation = max_observation
        self.behavior = pd.DataFrame(None, index=np.arange(max_observation),
                                     columns=["stimulus_id", "EIG", "Look_away"])
        self.possible_observations = None 

        
        self.all_observations = pd.DataFrame(0, index=np.arange(max_observation),
                                            columns = np.arange(stimuli.n_feature))
        
        # intiialize an empty list to store the tensor later 
        self.all_likelihood = [None] * self.max_observation
        self.all_posterior = [None] * self.max_observation


        # help with debugging 
        self.all_basic_KL = [None] * self.max_observation
        self.all_basic_PP = [None] * self.max_observation


    def update_model_stimulus_id(self): 
        self.behavior.at[self.current_t, "stimulus_id"] = self.current_stimulus_idx

    def update_model_eig(self, eig):
        self.behavior.at[self.current_t, "EIG"] = eig

    def update_model_decision(self, decision): 
        self.behavior.at[self.current_t, "Look_away"] = decision


    def update_likelihood(self, new_likeihood): 
        self.all_likelihood[self.current_t] = new_likeihood

    def update_posterior(self, new_posterior): 
        self.all_posterior[self.current_t] = new_posterior

    def update_possible_observations(self, noise_epsilon, hypothetical_obs_grid_n): 
        self.possible_observations = torch.linspace((self.stimuli.stimuli_sequence[self.current_stimulus_idx] - noise_epsilon).item(), 
                                                (self.stimuli.stimuli_sequence[self.current_stimulus_idx] + noise_epsilon).item(), 
                                                hypothetical_obs_grid_n)
        

    def update_noisy_observation(self, noise_epsilon): 
        current_stimulus = self.stimuli.stimuli_sequence[self.current_stimulus_idx]
        self.all_observations.loc[self.current_t]  = Normal(current_stimulus, noise_epsilon).sample().tolist()



    

    def get_all_observations_on_current_stimulus(self): 
        obs_index = self.behavior.index[self.behavior['stimulus_id'] == 
                                    self.current_stimulus_idx].tolist()
        return torch.tensor(self.all_observations.iloc[obs_index].values)

    def get_last_stimuli_likelihood(self): 
        last_stimuli_last_obs_t = max(self.behavior.index[self.behavior['stimulus_id'] == 
                                    self.current_stimulus_idx-1].tolist())
        return self.all_likelihood[last_stimuli_last_obs_t]

    def if_same_stimulus_as_previous_t(self): 
        last_t_stimulus = max(self.behavior[pd.notnull(self.behavior["stimulus_id"])]["stimulus_id"])
        return (self.current_stimulus_idx == last_t_stimulus)


        

        
        



