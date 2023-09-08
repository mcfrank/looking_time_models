import torch
from ...granch_utils import num_stab_help

stimuli_info_list = num_stab_help.sample_condition_experiment(1)

for idx, f in enumerate(stimuli_info_list):
    torch.save(f, "/om2/scratch/tmp/galraz/looking_time_models/02_pyGRANCH/MIT_cluster_tool/stimulus_job_arr/stimuli/"+str(idx)+"stim.pt")