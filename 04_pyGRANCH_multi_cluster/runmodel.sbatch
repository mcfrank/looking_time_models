#!/bin/bash
#SBATCH -p gpu
#SBATCH -c 10
#SBATCH -G 1
#SBATCH --output=granch.log
#SBATCH --mail-type=ALL

# load the module
ml python/3.9

python3 cluster_granch.py
 
