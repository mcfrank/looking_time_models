#!/bin/bash
#SBATCH --time=5:00:00
#SBATCH --mem=10G
#SBATCH --output=R-model.log

# load the module
module load openmind/R/3.6.1

Rscript run_model.R
