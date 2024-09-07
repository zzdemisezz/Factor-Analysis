#!/bin/bash

#SBATCH --job-name=dataframe_creation
#SBATCH --partition=short              
#SBATCH --output=logs/df_output_%A_%a.out  
#SBATCH --error=logs/df_error_%A_%a.err    
#SBATCH --array=1-478                   # The final job (478) will combine the results
#SBATCH --cpus-per-task=1               # One CPU core per task

# Load the R module
module load R/4.3.2-gfbf-2023a  # Adjust based on your server's configuration

# Set the base directory for the input .rds files and the output directory
base_dir="/well/nichols/users/rht383/results_complex"
output_dir="/well/nichols/users/rht383/processed_results"  # This is where processed chunks will be saved

if [ "$SLURM_ARRAY_TASK_ID" -le 477 ]; then
    # Processing job
    # Find all .rds files in all subfolders
    rds_files=($(find $base_dir -name '*.rds'))

    # Calculate the total number of .rds files to process
    total_files=${#rds_files[@]}

    # Calculate how many files each job should handle
    files_per_job=$((total_files / 477))
    remainder=$((total_files % 477))

    # Calculate start and end indices for this job
    start_index=$((SLURM_ARRAY_TASK_ID * files_per_job - files_per_job))
    end_index=$((start_index + files_per_job))

    # Adjust to distribute remainder files
    if [ $SLURM_ARRAY_TASK_ID -le $remainder ]; then
      end_index=$((end_index + 1))
    fi

    # Adjust the end index for the last job to cover any remaining files
    if [ $SLURM_ARRAY_TASK_ID -eq 477 ]; then
      end_index=$total_files
    fi

    # Extract the portion of files this job will handle
    job_files=(${rds_files[@]:$start_index:$((end_index - start_index))})

    # Create a directory for this job if it doesn't exist
    mkdir -p $output_dir/job_$SLURM_ARRAY_TASK_ID

    # Run the R script to process this subset of files
    Rscript process_data.R "${job_files[@]}" $output_dir/job_$SLURM_ARRAY_TASK_ID
else
    # Combination job (only runs after all processing jobs complete)
    Rscript combine_data.R $output_dir
fi
