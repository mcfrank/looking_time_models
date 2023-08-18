#!/bin/bash -l

project_path="/om2/scratch/tmp/galraz/looking_time_models/GRANCH_python_babies"
param_dir="$project_path/params/param_vals"

param_vals=($(find $param_dir/ -type f))

len=$(expr ${#param_vals[@]} - 1)  

cmd="sbatch --array=0-$len $project_path/run_model_MIT.sh $project_path ${param_vals[@]}"

$cmd
