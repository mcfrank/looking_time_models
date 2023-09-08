#!/bin/bash -l

project_path="/om2/scratch/tmp/galraz/looking_time_models/02_pyGRANCH/MIT_cluster_tool/"
stim_dir="$project_path/stimulus_job_arr/stimuli"

stim_paths=($(find $stim_dir/ -type f))

len=$(expr ${#stim_paths[@]} - 1) 

cmd="sbatch --array=0-$len $project_path/run_model_MIT.sh $project_path ${stim_paths[@]}"

$cmd
