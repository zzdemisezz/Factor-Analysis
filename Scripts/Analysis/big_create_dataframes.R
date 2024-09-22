# source("Scripts/Analysis/functions.R")
source("Scripts/Analysis/loading-objects.R")

# Specifying the directory results
main_dir <- "/well/nichols/users/rht383/results_5"

# Specify the directory where the result chunks will be saved
chunks_dir <- "/well/nichols/users/rht383/chunks5"

generate_subdir_paths <- function(main_dir) {
  # List all subdirectories under the main directory
  subdir_paths <- list.dirs(main_dir, recursive = FALSE, full.names = TRUE)
  return(subdir_paths)
}

subdir_paths <- generate_subdir_paths(main_dir)

# Process each subdir_path and save results incrementally
for (subdir_path in subdir_paths) {
  print(paste("Performing analysis for:", subdir_path))
  
  # Load data directly
  analysis_results <- load_data(subdir_path)
  
  # Extract the dataset name
  dataset_name <- basename(subdir_path)
  
  # Save the result as an RDS file in the chunks directory
  saveRDS(analysis_results, file = file.path(chunks_dir, paste0("results_", dataset_name, ".rds")))
  
  # Clear memory after saving each result
  rm(analysis_results)
  gc()  # Call garbage collector to free up memory
}

cat("Data processing complete. Results saved in", chunks_dir, "\n")

# Get the list of all saved .rds files
rds_files <- list.files(chunks_dir, pattern = "^results_.*\\.rds$", full.names = TRUE)

# Initialize an empty list to store combined results
all_analysis_results <- list()

# Loop through each saved .rds file and load the results
for (rds_file in rds_files) {
  # Extract the dataset name from the file name
  dataset_name <- gsub("^results_|\\.rds$", "", basename(rds_file))
  
  # Load the individual result
  analysis_result <- readRDS(rds_file)
  
  # Add it to the combined list
  all_analysis_results[[dataset_name]] <- analysis_result
}

# Save the combined result as a single .rds file
saveRDS(all_analysis_results, file = "/well/nichols/users/rht383/dataframes/results_5.rds")
# test <- readRDS("new_results_2.rds")
# cat("All individual results have been combined and saved as 'final_combined_results.rds'.\n")
