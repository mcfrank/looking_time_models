#!/bin/bash
#SBATCH --mail-type=END
#SBATCH -n 1 
#SBATCH --mem=16GB
#SBATCH --constraint=16GB
#SBATCH --gres=gpu:1
#SBATCH --time=24:00:00
#SBATCH --output=R-%x.%j.out
#SBATCH --error=R-%x.%j.err

cmd="python3 cluster_granch.py"

echo $cmd 

$cmd

