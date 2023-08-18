#!/bin/bash -l
#SBATCH --mail-type=END
#SBATCH -n 1 
#SBATCH --mem=8GB
#SBATCH --constraint=16GB
#SBATCH --gres=gpu:1
#SBATCH --time=24:00:00
#SBATCH --output=R-%x.%j.out
#SBATCH --error=R-%x.%j.err

params=("${@:2}")
current_param_values=${params[${SLURM_ARRAY_TASK_ID}]}
param_names="params/param_names.csv"
#current_param_values="params/param_vals/params190.csv"

module load openmind/cuda/11.3

cmd="python3 cluster_granch_MIT.py $current_param_values $param_names"

echo $cmd 

$cmd

