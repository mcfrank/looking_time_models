#!/usr/bin/bash
#SBATCH --time=5:00:00
#SBATCH --mem=10G
#SBATCH --output=R-model.log

# load the module
ml R/3.6

Rscript run_model.R

