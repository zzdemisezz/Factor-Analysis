#!/usr/bin/env Rscript

# Load the necessary arguments
args <- commandArgs(trailingOnly = TRUE)
output_dir <- args[1]

# Define the directory where the processed chunks are saved
processed_dir <- file.path(output_dir, "processed_chunks")

# Get the list of all processed .rds files from all jobs
chunk_files <- list.files(processed_dir, pattern = "processed_chunk_.*\\.rds$", full.names = TRUE)

# Initialize an empty list to store the dataframes
all_dataframes <- list()

# Loop through each chunk file and read the data
for (chunk_file in chunk_files) {
  chunk_data <- readRDS(chunk_file)
  all_dataframes[[length(all_dataframes) + 1]] <- chunk_data
}

# Combine all dataframes into one
final_dataframe <- do.call(rbind, all_dataframes)

# Save the final combined dataframe as an RDS file
saveRDS(final_dataframe, file = file.path(output_dir, "final_combined_dataframe.rds"))

cat("Final dataframe has been created and saved as 'final_combined_dataframe.rds'\n")
