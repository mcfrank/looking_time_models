from distutils.log import warn
from pickle import NONE, TRUE
import pandas as pd
import numpy as np
import torch 
import re 
from torch.distributions import Normal  
import ipdb


class granch_stimuli: 
    def __init__(self, n_feature, sequence_scheme): 

        self.n_feature = n_feature 
        self.n_trial = len(sequence_scheme)
        self.sequence_scheme = sequence_scheme

    def add_toy_example(self, b, d): 
        

        idx = 0 
        stimuli_sequence = {}
        while idx < self.n_trial: 
            if(self.sequence_scheme[idx] == "B"): 
                stimuli_sequence[idx] = torch.tensor([b, b, b])
            elif(self.sequence_scheme[idx] == "D"): 
                stimuli_sequence[idx] = torch.tensor([d, d, d])
            else: 
                warn("Wrong sequence scheme ")
            idx = idx + 1

        self.b_val = torch.tensor([b, b, b])
        self.d_val = torch.tensor([d, d, d])
        
        self.stimuli_sequence = stimuli_sequence
  

    def get_stimuli_sequence(self, embedding_path, distance_range = []): 
        embeddings = pd.read_csv(embedding_path, header = None)
        # select a pair to be background and deviant 
        bd_pair = embeddings.sample(2).iloc[:, 0:self.n_feature]
        b = torch.tensor(bd_pair.iloc[0, :])

        
        if len(distance_range) != 0: 
            d = (embeddings.sample(1).iloc[:, 0:self.n_feature]).iloc[0, :]
            
            #print(abs(d-b))
            while abs(d[0]-b.item()) > distance_range[1] or  abs(d[0]-b.item()) < distance_range[0]: 
                d = (embeddings.sample(1).iloc[:, 0:self.n_feature]).iloc[0, :]
            d = torch.tensor(d)


        else: 
            # if didn't specify a distance range, then randomly select another
            d = torch.tensor(bd_pair.iloc[1, :])

######### Below functions for running experiment with spores 

    def parse_spore_stim_type(self, stim_name):
        if "complex" in stim_name:
            stim_obj = dict(complexity="complex")
        else: 
            stim_obj = dict(complexity="simple")

        pattern = r'\d+'
        match = re.search(pattern, stim_name)
        stim_obj["id"] = match.group()
        return stim_obj

    
    def filtered_spore_embedding_pool(self, embeddings, b_obj_type):
        # first filter by the same complexity type 
        filtered_pool = embeddings[(embeddings[0].str.contains(b_obj_type["complexity"]))]
        # then filter out the same id 
        filtered_pool = filtered_pool[(~filtered_pool[0].str.contains(b_obj_type['id']))]

        return (filtered_pool)



    def get_spore_stimuli_sequence(self, embedding_path, complexity_type): 
        
        embeddings = pd.read_csv(embedding_path, header = None)
        embeddings = embeddings[(embeddings[0].str.contains(complexity_type))]

        b = embeddings.sample(1)
        b_name = b.iloc[:, 0].values[0]
        b_val = b.iloc[:, 1:self.n_feature +1] 

        b_obj_type = self.parse_spore_stim_type(b_name)

        # select a pair to be background and deviant following the violati type
        deviant_pool = self.filtered_spore_embedding_pool(embeddings,b_obj_type)

        d = deviant_pool.sample(1)
        d_val = d.iloc[:, 1:self.n_feature +1] 

        b = torch.tensor(b_val.values[0])
        d = torch.tensor(d_val.values[0])

        self.b_val = b_val
        self.d_val = d_val

       
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
        self.complexity_type = complexity_type

######### Below functions for running experiment with unity 
    def parse_stim_type(self, stim_name):
        if "pair" in stim_name:
            stim_obj = dict(number="pair")
        else: 
            stim_obj = dict(number="single")

        if "left" in stim_name: 
            stim_obj["pose"] = "left"
        else: 
            stim_obj["pose"] = "right"

        pattern = r"(\w+)_(\d+)_([\w_]+?)(?:_pair)?\.png"
        match = re.match(pattern, stim_name)

        if match == None:
            ipdb.set_trace()

        stim_obj["animacy"] = match.group(1)
        stim_obj["id"] = match.group(2)

        return stim_obj



    def find_violation_category(self, violation_type, b_obj_type): 
        violation_convert_dic = dict(animate = "inanimate", inanimate = "animate", left = "right", 
                                 right = "left",pair = "single", single = "pair")

        violation_category = dict(b_obj_type)
        if violation_type == "animacy":
            violation_category["animacy"] = violation_convert_dic[b_obj_type["animacy"]]
            violation_category["id"] = ""
        elif violation_type == "number": 
            violation_category["number"] = violation_convert_dic[b_obj_type["number"]]
        elif violation_type == "pose":
            violation_category["pose"] = violation_convert_dic[b_obj_type["pose"]]
        elif violation_type == "identity":
            violation_category["id"] = ""

        return violation_category 


    def filtered_embedding_pool(self, embeddings, violation_type, b_obj_type):

    # convert violation_condition 
        violation_category = self.find_violation_category(violation_type, b_obj_type)

    # number is the messy case, dealing it with first 
        if violation_category["number"] == "single": 
            filtered_pool = embeddings[(~embeddings[0].str.contains("pair"))]
        else: 
            filtered_pool = embeddings[embeddings[0].str.contains("pair")] 

    # filtering pose 
        filtered_pool = filtered_pool[filtered_pool[0].str.contains(violation_category["pose"])]

    # filtering animacy
        if violation_category["animacy"] == "inanimate": 
            filtered_pool = filtered_pool[filtered_pool[0].str.contains("inanimate")]
        else: 
            filtered_pool = filtered_pool[~filtered_pool[0].str.contains("inanimate")]

    # filtering identity
        if violation_type == "identity": 
            filtered_pool = filtered_pool[(~filtered_pool[0].str.contains(b_obj_type["id"]))]
        elif violation_type == "animacy": 
            pass #animacy doesn't constrain on identity 
        else: 
            filtered_pool = filtered_pool[(filtered_pool[0].str.contains(violation_category["id"]))]
    

        return filtered_pool

    def get_violation_stimuli_sequence(self, embedding_path, violation_type): 
        embeddings = pd.read_csv(embedding_path, header = None)
        
        b = embeddings.sample(1)
        b_name = b.iloc[:, 0].values[0]

        b_val = b.iloc[:, 1:self.n_feature +1] 

        b_obj_type = self.parse_stim_type(b_name)

        # select a pair to be background and deviant following the violati type
        deviant_pool = self.filtered_embedding_pool(embeddings, violation_type, b_obj_type)

        d = deviant_pool.sample(1)
        d_val = d.iloc[:, 1:self.n_feature +1] 

        b = torch.tensor(b_val.values[0])
        d = torch.tensor(d_val.values[0])

        self.b_val = b_val
        self.d_val = d_val

       
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
        self.violation_type = violation_type


    def get_baby_exposure_duration_pairings(self, embedding_path, violation_type, n_feature): 

        self.n_feature  = n_feature

        embeddings = pd.read_csv(embedding_path, header = None)
        
        # sort alphabetically as that is the presentation order
        embeddings = embeddings.sort_values(by=0)
        
        # grab a random animal
        b = embeddings.sample(1)
        b_name = b.iloc[:, 0].values[0].replace("animate_", "")

        b_val = b.iloc[:, 1:self.n_feature +1] 

        b_obj_type = self.parse_stim_type("animate_" + b_name)

        # select the corresponding deviant
        # first get the number
        background_num = int(b_name[0:3])
        
        if background_num % 2 == 0:  # if n is even
            dev_num = background_num - 1
        else:  # if n is odd
            dev_num = background_num + 1

        # get the orientation
        orientation = b_name[4:].replace('.png','')

        # corresponding deviant animal
        if dev_num < 10:
            dev_string = '00' + str(dev_num)
        else:
            dev_string = '0' + str(dev_num)
        
        # name of deviant images
        dev_string = "animate_" + dev_string + '_' + orientation + '.png'

        # index into embeddings to find dev string
        d = embeddings[embeddings.iloc[:,0] == dev_string]

        d_val = d.iloc[:, 1:self.n_feature +1] 

        b = torch.tensor(b_val.values[0])
        d = torch.tensor(d_val.values[0])

        self.b_val = b_val
        self.d_val = d_val
       
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
        self.violation_type = violation_type

    def get_baby_graded_dishab_pairing(self, embedding_path, violation_type): 

        return

class granch_model: 
    def __init__(self, max_observation, stimuli):

        self.device = 'cuda' if torch.cuda.is_available() else 'cpu'

        self.current_t = 0
        self.current_stimulus_idx = 0

        self.stimuli = stimuli

        self.max_observation = max_observation
        self.behavior = pd.DataFrame(None, index=np.arange(max_observation),
                                     columns=["stimulus_id", "EIG", "Look_away"])
        
        self.behavior["surprisal"] = np.nan

        self.possible_observations = None 

        
        self.all_observations = pd.DataFrame(0, index=np.arange(max_observation),
                                            columns = np.arange(stimuli.n_feature))
        
       
        # try to just cached the last stimulus likelihood
        self.cur_likelihood = None
        self.prev_likelihood = None
        self.cur_posterior = None

        self.ps_kl = torch.tensor([])
        self.ps_pp = torch.tensor([])


    def update_model_stimulus_id(self): 
        self.behavior.at[self.current_t, "stimulus_id"] = self.current_stimulus_idx

    def update_model_surprisal(self, surprisal):
        self.behavior.at[self.current_t, "surprisal"] = surprisal
    
    def update_model_eig(self, eig):
        self.behavior.at[self.current_t, "EIG"] = eig

    def update_model_decision(self, decision): 
        self.behavior.at[self.current_t, "Look_away"] = decision

    def update_possible_observations(self, noise_epsilon, hypothetical_obs_grid_n): 
        
        current_stimuli = self.stimuli.stimuli_sequence[self.current_stimulus_idx]
        expanded_tensors = [torch.linspace(val - noise_epsilon, val + noise_epsilon, hypothetical_obs_grid_n) for val in current_stimuli]

        expanded_tensor = torch.stack(expanded_tensors, dim=1).t()
        d1 = expanded_tensor[0]
        d2 = expanded_tensor[1]
        d3 = expanded_tensor[2]

        grid_1, grid_2, grid_3= torch.meshgrid(d1, d2, d3)

        # Stack the grids to form tensor C
        all_possible_obs = torch.stack((grid_1, grid_2, grid_3), dim=-1).view(-1, 3)


        self.possible_observations = all_possible_obs
       

    def update_noisy_observation(self, noise_epsilon): 
        current_stimulus = self.stimuli.stimuli_sequence[self.current_stimulus_idx]
        self.all_observations.loc[self.current_t]  = Normal(current_stimulus, noise_epsilon).sample().tolist()

    def get_all_observations_on_current_stimulus(self): 
        obs_index = self.behavior.index[self.behavior['stimulus_id'] == 
                                    self.current_stimulus_idx].tolist()
        return torch.tensor(self.all_observations.iloc[obs_index].values).to(self.device)

    def get_current_observation(self):
        return torch.tensor(self.all_observations.iloc[self.current_t].values) 

    def get_last_stimuli_likelihood(self): 
        last_stimuli_last_obs_t = max(self.behavior.index[self.behavior['stimulus_id'] == 
                                    self.current_stimulus_idx-1].tolist())
        return self.all_likelihood[last_stimuli_last_obs_t]

    def if_same_stimulus_as_previous_t(self): 

        last_t_stimulus = max(self.behavior[pd.notnull(self.behavior["stimulus_id"])]["stimulus_id"])
        return (self.current_stimulus_idx == last_t_stimulus)


        

        
        



