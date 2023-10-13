#!/bin/bash -l
#SBATCH --mail-type=END
#SBATCH --mail-type=BEGIN
#SBATCH --mem=128GB
#SBATCH --output=summarize.%A_%a.out
#SBATCH --time=10:00:00

python -m 02_pyGRANCH.MIT_cluster_tool.summarizing_tool.summarize