#!/bin/bash

#SBATCH --job-name=simulations
#SBATCH --partition=short
#SBATCH --output=logs/sim_output_%A_%a.out  # Redirect standard output to logs directory
#SBATCH --error=logs/sim_error_%A_%a.err    # Redirect standard error to logs directory
#SBATCH --array=1-1080                      # Job array 
#SBATCH --cpus-per-task=1                   # Number of CPU cores per job

# Load the R module
module load R/4.3.2-gfbf-2023a  # Adjust according to your cluster's configuration

# List of data generator names
# data_generators=("3x3-strong" "3x3-moderate" "3x3-weak" "5x5-strong" "5x5-moderate" "5x5-weak" "overlap2-small-strong" "overlap2-small-moderate" "overlap2-small-weak" "overlap2-large-strong" "overlap2-large-moderate" "overlap2-large-weak" "overlap3-small-strong" "overlap3-small-moderate" "overlap3-small-weak" "overlap3-large-strong" "overlap3-large-moderate" "overlap3-large-weak")
data_generators=("simple-strong" "simple-moderate" "simple-weak" 
                 "medium-strong" "medium-moderate" "medium-weak" 
                 "hard-strong" "hard-moderate" "hard-weak")

# Number of simulations per data generator
num_simulations=120

# Calculate the data generator and simulation index based on the SLURM array index
total_simulations=$((${#data_generators[@]} * num_simulations))
index=$((SLURM_ARRAY_TASK_ID - 1))
gen_index=$((index / num_simulations))
sim_index=$((index % num_simulations + 1))

# Run the R script with the appropriate arguments
Rscript Scripts/run_single_simulation.R ${data_generators[$gen_index]} $sim_index
