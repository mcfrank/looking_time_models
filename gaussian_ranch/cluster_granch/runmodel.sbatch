#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=32
#SBATCH --time=48:00:00
#SBATCH --output=granch.log
#SBATCH --mail-type=ALL

# load the module
ml R/3.6

Rscript run_granch.r

