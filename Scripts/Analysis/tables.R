rm(list = ls())
source("Scripts/Analysis/Final_Functions.R")
library(xtable)
library(dplyr)

results <- readRDS("results_5.rds")

# Function to create the summary table based on a results list, maybe to change this to also do var/bias
create_summary_table <- function(results_list) {
  # Define the matrix names you're interested in (hardcoded)
  # matrix_names <- c("B_permuted", "B_permuted_beta", "B_permuted_beta_pxl", "B_permuted_pxl",
  #                   "Covariance_matrix", "Covariance_matrix_beta", "Covariance_matrix_beta_pxl", "Covariance_matrix_pxl")
  matrix_names <- c( "Covariance_matrix", "Covariance_matrix_beta", "Covariance_matrix_pxl", "Covariance_matrix_beta_pxl")
  # matrix_names <- c( "Covariance_matrix", "Covariance_matrix_beta", "Covariance_matrix_pxl")
  
  
  # Initialize an empty list to store the results
  results <- list()
  
  # Loop through each structure in the results list
  for (structure_name in names(results_list)) {
    # Initialize an empty vector to store the MSE averages for this structure
    mse_values <- c()
    
    # Loop through each matrix name
    for (matrix_name in matrix_names) {
      # Determine the corresponding true matrix name
      if (grepl("Covariance", matrix_name)) {
        true_matrix_name <- "Covariance_matrix_true"
      } else {
        true_matrix_name <- "B_True"
      }
      
      # Directly calculate the average MSE
      MSE_matrix <- calculate_elementwise_MSE_matrix(results_list[[structure_name]][[matrix_name]], 
                                                     results_list[[structure_name]][[true_matrix_name]][[1]])
      average_MSE <- mean(MSE_matrix)  # Take the average of the MSE matrix
      
      # Store the result
      mse_values <- c(mse_values, average_MSE)
    }
    
    # Store the results for this structure in the list
    results[[structure_name]] <- mse_values
  }
  
  # Convert the results list to a data frame
  results_df <- do.call(rbind, results)
  colnames(results_df) <- matrix_names
  
  # Return the results as a data frame
  return(results_df)
}

# Print the summary table
summary_table <- create_summary_table(results)
round(summary_table, 4)
# summary_table_old <- create_summary_table(results_old)
# round(summary_table_old,4)

# Convert the table to LaTeX code
latex_code <- xtable(summary_table,digits = 4)
print(latex_code)

stop()
# Function to compare GAMMA to B_True for a given results list
create_comparison_table <- function(results_list) {
  
  # Function to compare GAMMA to B_True for a specific dataframe
  compare_GAMMA_to_B_true <- function(dataframe, type = "em") {
    # Select the appropriate GAMMA_permuted list based on the type
    GAMMA_permuted_list <- switch(type,
                                  em = dataframe$GAMMA_permuted,
                                  em_beta = dataframe$GAMMA_permuted_beta,
                                  em_pxl = dataframe$GAMMA_permuted_pxl,
                                  em_beta_pxl = dataframe$GAMMA_permuted_beta_pxl,
                                  stop("Invalid type specified. Choose 'em', 'em_beta', or 'em_pxl'."))
    
    B_true <- dataframe$B_True[[1]]  # Assuming B_True is consistent across simulations
    
    # Compare each GAMMA_permuted matrix with B_True
    match_results <- sapply(GAMMA_permuted_list, function(GAMMA_permuted) {
      isTRUE(all.equal(GAMMA_permuted, B_true))
    })
    
    # Calculate the number of exact matches
    exact_matches <- sum(match_results)
    
    return(exact_matches)
  }
  
  # Initialize an empty list to store results
  comparison_results <- list()
  
  # Loop over each structure in the results list
  for (structure_name in names(results_list)) {
    dataframe <- results_list[[structure_name]]
    
    # Store the results for different comparison types
    comparison_results[[structure_name]] <- list(
      times_correct = compare_GAMMA_to_B_true(dataframe, "em"),
      times_correct_beta = compare_GAMMA_to_B_true(dataframe, "em_beta"),
      times_correct_pxl = compare_GAMMA_to_B_true(dataframe, "em_pxl"),
      times_correct_beta_pxl = compare_GAMMA_to_B_true(dataframe, "em_beta_pxl")
    )
  }
  
  # Convert the list to a data frame for easier manipulation
  comparison_df <- do.call(rbind, lapply(names(comparison_results), function(name) {
    data.frame(
      Dataset = name,
      times_correct = comparison_results[[name]]$times_correct,
      times_correct_beta = comparison_results[[name]]$times_correct_beta,
      times_correct_pxl = comparison_results[[name]]$times_correct_pxl,
      times_correct_beta_pxl = comparison_results[[name]]$times_correct_beta_pxl
    )
  }))
  
  # Remove row names and reorder columns
  rownames(comparison_df) <- NULL
  comparison_df <- comparison_df %>%
    select(Dataset, times_correct, times_correct_beta, times_correct_pxl, times_correct_beta_pxl)
    # select(Dataset, times_correct, times_correct_beta, times_correct_pxl) # Ensure column order
  
  # Return the comparison data frame
  return(comparison_df)
}

comparison_df <- create_comparison_table(results)
print(comparison_df)
