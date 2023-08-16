import torch
from granch_utils import num_stab_help

stimuli_info_list = num_stab_help.sample_condition_experiment(5)

for idx, f in enumerate(stimuli_info_list):
    torch.save(f, "stimuli_tensors/"+str(idx)+"stim.pt")