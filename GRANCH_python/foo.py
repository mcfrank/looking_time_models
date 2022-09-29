import torch 
from itertools import repeat 
import numpy as np
import pandas as pd
import helper
import compute_prob
import warnings


a = pd.read_csv("embedding_PCA.csv", header = None)
print(a)


print(a.sample(2).iloc[:,0:3])

bd_pair = a.sample(2).iloc[:,0:3]

b = torch.tensor(bd_pair.iloc[0, :])
d = torch.tensor(bd_pair.iloc[1, :])

idx = 0
stimuli_sequence = {}
sequence_scheme = "BBDBBBBB"
n_trial = len(sequence_scheme)
while idx < n_trial: 
    if(sequence_scheme[idx] == "B"): 
        stimuli_sequence[idx] = b
    elif(sequence_scheme[idx] == "D"): 
        stimuli_sequence[idx] = d
    else: 
        warnings.warn("Wrong sequence scheme ")
    idx = idx + 1
print(stimuli_sequence)