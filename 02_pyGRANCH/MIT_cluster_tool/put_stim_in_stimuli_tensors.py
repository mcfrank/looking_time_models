import torch
from granch_utils import num_stab_help

<<<<<<< HEAD
stimuli_info_list = num_stab_help.sample_condition_experiment(5)
=======
stimuli_info_list = num_stab_help.sample_condition_experiment(20)
>>>>>>> a72d9ef5681bb7004ee2bcd318fb351116044c27

for idx, f in enumerate(stimuli_info_list):
    torch.save(f, "stimuli_tensors/"+str(idx)+"stim.pt")