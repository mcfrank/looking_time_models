#/usr/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=32
#SBATCH --output=R-model-parallel.log
#SBATCH --mail-type=ALL

# load the module
ml R/3.6

Rscript cluster_timing.R

