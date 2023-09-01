#!/bin/bash

project_path=`cat PATHS.txt`
param_dir="$project_path/param_files"

param_files=( $(find $param_dir/ -not -path '*/._*' -type f -name "*.csv") )

len=$(expr ${#param_files[@]} - 1) 

cmd="sbatch --array=0-$len $project_path/run_model.sh $project_path ${param_files[@]}"

echo $cmd
