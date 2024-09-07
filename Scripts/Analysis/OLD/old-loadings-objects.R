source("Scripts/Analysis/permute.R")
# Load Data/Dataframe ####
load_data <- function(subdir_path) {
  # List all .rds files in the specified directory
  rds_files <- list.files(path = subdir_path, pattern = "\\.rds$", full.names = TRUE)
  
  # Load all .rds files into a list
  simulation_data <- lapply(rds_files, readRDS)
  
  # Optionally, name the list elements after the filenames (removing the path)
  names(simulation_data) <- basename(rds_files)
  
  # Initialize an empty list to store the data
  simulation_data_list <- list()
  
  # Loop through each simulation and extract the relevant matrices
  for (i in 1:length(simulation_data)) {
    # Extract matrices
    B_truncated_matrix <- simulation_data[[i]]$all_results_em[[1]]$best_result$B_truncated
    GAMMA_truncated_matrix <- simulation_data[[i]]$all_results_em[[1]]$best_result$GAMMA_truncated
    Covariance_matrix <- simulation_data[[i]]$all_results_em[[1]]$best_result$Covariance_matrix_truncated
    B_True <- simulation_data[[i]]$all_datasets[[1]]$B_true
    Covariance_matrix_true <- simulation_data[[i]]$all_datasets[[1]]$Covariance_matrix_true
    
    # Create a list representing one row of the dataframe, storing matrices as lists
    simulation_row <- data.frame(
      B = I(list(B_truncated_matrix)),
      GAMMA = I(list(GAMMA_truncated_matrix)),
      Covariance_matrix = I(list(Covariance_matrix)),
      B_True = I(list(B_True)),
      Covariance_matrix_true = I(list(Covariance_matrix_true)),
      Simulation = i
    )
    
    # Add the row to the list
    simulation_data_list[[i]] <- simulation_row
  }
  
  # Combine all rows into a single dataframe
  result_dataframe <- do.call(rbind, simulation_data_list)
  
  # Return the dataframe
  return(list(dataframe = result_dataframe, raw_data = simulation_data))
}

# Example usage:
subdir_path <- "results4/LargeFactors_5x5_Moderate"
LargeFactors_5x5_Moderate <- load_data(subdir_path)
LargeFactors_5x5_Moderate_dataframe <- LargeFactors_5x5_Moderate$dataframe
LargeFactors_5x5_Moderate_data <- LargeFactors_5x5_Moderate$raw_data

# Example: Access the first sim LargeFactors_5x5_Moderate_data[[1]] ####

likelihoods <- numeric(length(LargeFactors_5x5_Moderate_dataframe$B))  # Pre-allocate a numeric vector to store the likelihoods

# Loop through indices from 1 to 100
for (i in 1:length(LargeFactors_5x5_Moderate_dataframe$B)) {
  likelihoods[i] <- LargeFactors_5x5_Moderate_data[[i]]$all_results_em[[1]]$best_result$likelihood
}
likelihoods
zero_indices <- which(likelihoods == 0)
print(zero_indices)

# Remove zero rows
LargeFactors_5x5_Moderate_dataframe <- LargeFactors_5x5_Moderate_dataframe[-c(zero_indices), ]

# Add permutations ####
# Function to permute B matrices and add them to the dataframe
add_permuted_B <- function(dataframe, B_true) {
  
  # Apply the permutation and sign adjustment for each row in the dataframe
  dataframe$B_permuted <- lapply(1:nrow(dataframe), function(i) {
    B_truncated <- dataframe$B[[i]]
    GAMMA_truncated <- dataframe$GAMMA[[i]]
    
    # Permute the B matrix using the provided B_true and GAMMA_truncated
    B_permuted <- permute_B(B_true, GAMMA_truncated, B_truncated)
    
    return(B_permuted)
  })
  
  return(dataframe)
}

B_true <- LargeFactors_5x5_Moderate_dataframe$B_True[[1]]
Covariance_matrix_true <- LargeFactors_5x5_Moderate_dataframe$Covariance_matrix_true[[1]]

LargeFactors_5x5_Moderate_dataframe <- add_permuted_B(LargeFactors_5x5_Moderate_dataframe, 
                                                      B_true)

# Now compare the aligned and sign-adjusted B_est_permuted with B_true
MSE_permuted <- numeric(length(LargeFactors_5x5_Moderate_dataframe$B))
for (i in 1:length(LargeFactors_5x5_Moderate_dataframe$B)) {
  MSE_permuted[i] <- sqrt(mean((B_true - LargeFactors_5x5_Moderate_dataframe$B_permuted[[i]])^2))
}
MSE_permuted

MSE <- numeric(length(LargeFactors_5x5_Moderate_dataframe$B))
for (i in 1:length(LargeFactors_5x5_Moderate_dataframe$B)) {
  MSE[i] <- sqrt(mean((B_true - LargeFactors_5x5_Moderate_dataframe$B[[i]])^2))
}
MSE

# Heatmap ####
# Load necessary libraries
library(ggplot2)
library(reshape2)

# Initialize an empty matrix for the average B matrix
average_B <- Reduce(`+`, LargeFactors_5x5_Moderate_dataframe$B_permuted) / length(LargeFactors_5x5_Moderate_dataframe$B)
bias_B <- average_B - B_true

average_Covariance_matrix <- Reduce(`+`, LargeFactors_5x5_Moderate_dataframe$Covariance_matrix) / length(LargeFactors_5x5_Moderate_dataframe$Covariance_matrix)
bias_Covariance_matrix <- average_Covariance_matrix - Covariance_matrix_true

# Function to create a heatmap with the correct orientation
create_heatmap <- function(matrix_data, title = "Heatmap", x_label = "Factors", y_label = "Loadings") {
  
  # Convert the matrix to a format suitable for ggplot2
  matrix_melted <- melt(matrix_data)
  
  # Create the heatmap with the y-axis reversed
  heatmap_plot <- ggplot(matrix_melted, aes(x = Var2, y = Var1, fill = value)) +
    geom_tile() +
    scale_y_reverse() +  # Reverse the y-axis
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
    theme_minimal() +
    labs(title = title, x = x_label, y = y_label)
  
  # Return the plot object
  return(heatmap_plot)
}

heatmap_bias_Covariance_matrix <- create_heatmap(bias_Covariance_matrix, title = "Heatmap of bias Covariance Matrix")
print(heatmap_bias_Covariance_matrix)
heatmap_bias_B <- create_heatmap(bias_B, title = "Heatmap of bias B Matrix")
print(heatmap_bias_B)


