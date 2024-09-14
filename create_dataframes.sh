#!/bin/bash

#SBATCH --job-name=dataframe
#SBATCH --partition=short
#SBATCH --output=logs/sim_output_%A_%a.out  # Redirect standard output to logs directory
#SBATCH --error=logs/sim_error_%A_%a.err    # Redirect standard error to logs directory
#SBATCH --array=1                           # Job array 
#SBATCH --cpus-per-task=1                   # Number of CPU cores per job

# Load the R module
module load R/4.3.2-gfbf-2023a  # Adjust according to your cluster's configuration

Rscript Scripts/Analysis/big_create_dataframes.R
