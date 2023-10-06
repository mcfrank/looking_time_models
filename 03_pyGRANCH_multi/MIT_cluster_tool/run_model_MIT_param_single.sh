#!/bin/bash -l
#SBATCH --mail-type=END
#SBATCH -n 1 
#SBATCH --mem=16GB
#SBATCH --constraint=16GB
#SBATCH --gres=gpu:1
#SBATCH --time=24:00:00
#SBATCH --output=R-%x.%j.out
#SBATCH --error=R-%x.%j.err

params=("${@:2}")
current_param_values=${params[${SLURM_ARRAY_TASK_ID}]}
param_names="02_pyGRANCH/MIT_cluster_tool/param_job_arr/params/param_names.csv"

echo $current_param_values

cmd="python -m 02_pyGRANCH.MIT_cluster_tool.cluster_granch_MIT_param_array $current_param_values $param_names" 

echo $cmd 

$cmd

