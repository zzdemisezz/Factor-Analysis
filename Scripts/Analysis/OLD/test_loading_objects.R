# Define the path to the specific subdirectory
subdir_path <- "results3/LargeFactors_5x5_Moderate"

# List all .rds files in the "LargeFactors_5x5_Moderate" directory
rds_files <- list.files(path = subdir_path, pattern = "\\.rds$", full.names = TRUE)

# Load all .rds files into a list
LargeFactors_5x5_Moderate_data <- lapply(rds_files, readRDS)

# Optionally, name the list elements after the filenames (removing the path)
names(LargeFactors_5x5_Moderate_data) <- basename(rds_files)
class(LargeFactors_5x5_Moderate_data$results_sim_1.rds$all_results_em[[1]]$best_result$B)
LargeFactors_5x5_Moderate_data[[1]]$all_datasets[[1]]$Covariance_matrix_true

# Initialize an empty list to store the data
simulation_data_list <- list()

# Loop through each simulation and extract the relevant matrices
for (i in 1:length(LargeFactors_5x5_Moderate_data)) {
  # Extract matrices
  B_truncated_matrix <- LargeFactors_5x5_Moderate_data[[i]]$all_results_em[[1]]$best_result$B_truncated
  Covariance_matrix <- LargeFactors_5x5_Moderate_data[[i]]$all_results_em[[1]]$best_result$Covariance_matrix_truncated
  
  # Create a list representing one row of the dataframe, storing matrices as lists
  simulation_row <- data.frame(
    B = I(list(B_truncated_matrix)),
    Covariance_matrix = I(list(Covariance_matrix)),
    Simulation = i
  )
  
  # Add the row to the list
  simulation_data_list[[i]] <- simulation_row
}

# Combine all rows into a single dataframe
LargeFactors_5x5_Moderate_dataframe <- do.call(rbind, simulation_data_list)

# Preview the dataframe
class(LargeFactors_5x5_Moderate_dataframe$B[[1]])
