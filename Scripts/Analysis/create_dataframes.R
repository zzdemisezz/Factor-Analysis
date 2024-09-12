rm(list = ls())
source("Scripts/Analysis/functions.R")
source("Scripts/Analysis/loading-objects.R")

# Specifying the directory results
main_dir <- "Results/results_pxl" 

generate_subdir_paths <- function(main_dir) {
  
  # List all subdirectories under the main directory
  subdir_paths <- list.dirs(main_dir, recursive = FALSE, full.names = TRUE)
  
  # Return the list of subdirectory paths
  return(subdir_paths)
}

subdir_paths <- generate_subdir_paths(main_dir)

# Initialize an empty list to store all results
all_analysis_results <- list()

# Loop through each subdir_path and perform the analysis
for (subdir_path in subdir_paths) {
  print(paste("Performing analysis for:", subdir_path))

  # Load data directly
  analysis_results <- load_data(subdir_path)

  # Extract the dataset name
  dataset_name <- basename(subdir_path)

  # Store the result in the list with the dataset name as the key
  all_analysis_results[[dataset_name]] <- analysis_results
}

stop()

# Initialize an empty list to store results
converged_counts <- list()

# Loop through each element (dataframe) in all_analysis_results
for (name in names(all_analysis_results)) {
  # Count the number of rows where converged is TRUE
  num_converged_true <- sum(all_analysis_results[[name]]$converged == TRUE, na.rm = TRUE)
  
  # Store the result in the list with the name of the structure
  converged_counts[[name]] <- num_converged_true
}

# Print the results for each structure
for (name in names(converged_counts)) {
  cat("Structure:", name, "- Number of converged = TRUE:", converged_counts[[name]], "\n")
}


# all_analysis_results <- readRDS("all_analysis_results.rds")





