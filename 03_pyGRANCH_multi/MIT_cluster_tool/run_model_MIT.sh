#!/bin/bash -l
#SBATCH --mail-type=END
#SBATCH -n 1 
#SBATCH --mem=16GB
#SBATCH --constraint=16GB
#SBATCH --gres=gpu:1
#SBATCH --time=24:00:00
#SBATCH --output=R-%x.%j.out
#SBATCH --error=R-%x.%j.err

stim_paths=("${@:2}")

current_stim_path=${stim_paths[${SLURM_ARRAY_TASK_ID}]}

echo $current_stim_path

cmd="python -m 02_pyGRANCH.MIT_cluster_tool.cluster_granch_MIT $current_stim_path" 

echo $cmd 

$cmd

