#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=32
#SBATCH --time=48:00:00
#SBATCH --output=granch.log
#SBATCH --mail-type=ALL

# load the module
ml python/3.6.1

python3 cluster_granch.py
