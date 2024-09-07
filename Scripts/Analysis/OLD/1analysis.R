rm(list = ls())
source("Scripts/Analysis/functions.R")
source("Scripts/Analysis/loading-objects.R")

# Now you can use the function by just specifying the main directory
main_dir <- "results_pxl" # make it "results/results_pxl" I think
subdir_paths <- generate_subdir_paths(main_dir)

# Initialize an empty list to store all results
all_analysis_results <- list()

# Loop through each subdir_path and perform the analysis
for (subdir_path in subdir_paths) {
  print(paste("Performing analysis for:", subdir_path))

  # Load data directly
  analysis_results <- load_data(subdir_path)

  # Extract the dataset name dynamically, regardless of the root directory
  dataset_name <- gsub("/", "_", basename(subdir_path))

  # Store the result in the list with the dataset name as the key
  all_analysis_results[[dataset_name]] <- analysis_results
}

all_analysis_results <- readRDS("all_analysis_results.rds")

# Function to calculate element-wise bias matrix for a given matrix type
calculate_elementwise_bias_matrix <- function(dataframe, matrix_name, true_matrix_name) {
  # Extract the number of matrices and the dimensions of one matrix
  num_matrices <- nrow(dataframe)
  matrix_dim <- dim(dataframe[[matrix_name]][[1]])
  
  # Initialize matrices to store the sum of elements
  sum_matrix <- matrix(0, nrow = matrix_dim[1], ncol = matrix_dim[2])
  
  # Loop through each matrix to compute the sum of elements
  for (i in 1:num_matrices) {
    matrix <- dataframe[[matrix_name]][[i]]
    sum_matrix <- sum_matrix + matrix
  }
  
  # Calculate the mean matrix (average of all permuted matrices)
  mean_matrix <- sum_matrix / num_matrices
  
  # Retrieve the true matrix
  true_matrix <- dataframe[[true_matrix_name]][[1]]
  
  # Calculate the bias matrix as the difference between the mean matrix and the true matrix
  bias_matrix <- mean_matrix - true_matrix
  
  return(bias_matrix)
}
# Function to calculate variance for a given matrix type
calculate_elementwise_variance_matrix <- function(dataframe, matrix_name) {
  # Extract the number of matrices and the dimensions of one matrix
  num_matrices <- nrow(dataframe)
  matrix_dim <- dim(dataframe[[matrix_name]][[1]])
  
  # Initialize matrices to store the sum of elements and the sum of squared differences
  sum_matrix <- matrix(0, nrow = matrix_dim[1], ncol = matrix_dim[2])
  variance_matrix <- matrix(0, nrow = matrix_dim[1], ncol = matrix_dim[2])
  
  # Loop through each matrix to compute the sum of elements
  for (i in 1:num_matrices) {
    matrix <- dataframe[[matrix_name]][[i]]
    sum_matrix <- sum_matrix + matrix
  }
  
  # Calculate the mean matrix
  mean_matrix <- sum_matrix / num_matrices
  
  # Loop again to calculate the variance using the formula you provided
  for (i in 1:num_matrices) {
    matrix <- dataframe[[matrix_name]][[i]]
    variance_matrix <- variance_matrix + (matrix - mean_matrix)^2
  }
  
  # The variance matrix is the sum of squared differences divided by (number of simulations - 1)
  variance_matrix <- variance_matrix / (num_matrices - 1)
  
  return(variance_matrix)
}
# Function to calculate element-wise MSE matrix for a given matrix type
calculate_elementwise_MSE_matrix <- function(dataframe, matrix_name, true_matrix_name) {
  # Extract the number of matrices and the dimensions of one matrix
  num_matrices <- nrow(dataframe)
  matrix_dim <- dim(dataframe[[matrix_name]][[1]])
  
  # Initialize a matrix to store the sum of squared differences
  MSE_matrix <- matrix(0, nrow = matrix_dim[1], ncol = matrix_dim[2])
  
  # Retrieve the true matrix
  true_matrix <- dataframe[[true_matrix_name]][[1]]
  
  # Loop through each matrix to compute the sum of squared differences
  for (i in 1:num_matrices) {
    matrix <- dataframe[[matrix_name]][[i]]
    MSE_matrix <- MSE_matrix + (matrix - true_matrix)^2
  }
  
  # The MSE matrix is the average of the squared differences
  MSE_matrix <- MSE_matrix / num_matrices
  
  return(MSE_matrix)
}

stop()
# Table ####
# Updated function to calculate average MSE
calculate_average_MSE <- function(dataframe, matrix_name, true_matrix_name) {
  # Calculate the MSE matrix
  MSE_matrix <- calculate_elementwise_MSE_matrix(dataframe, matrix_name, true_matrix_name)
  
  # Return the average of all elements in the MSE matrix
  return(mean(MSE_matrix))
}

# Updated function to create the summary table
create_summary_table <- function(subdir_paths, matrix_names) {
  # Initialize an empty list to store the results
  results <- list()
  
  # Loop through each condition
  for (path in subdir_paths) {
    # Extract the structure name from the path
    structure_name <- gsub("results_pxl/", "", path)
    
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
      
      # Calculate the average MSE for this matrix
      average_MSE <- calculate_average_MSE(all_analysis_results[[structure_name]], matrix_name, true_matrix_name)
      
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

# Define the matrix names you're interested in
matrix_names <- c("B_permuted", "B_permuted_beta", "B_permuted_pxl", "Covariance_matrix", "Covariance_matrix_beta", "Covariance_matrix_pxl")

# Create the summary table
summary_table <- create_summary_table(subdir_paths, matrix_names)

# Print the summary table
print(summary_table)

# table 2

library(dplyr)

# Function as given
compare_GAMMA_to_B_true <- function(dataframe, type = "em") {
  if (type == "em") {
    GAMMA_permuted_list <- dataframe$GAMMA_permuted
  } else if (type == "em_beta") {
    GAMMA_permuted_list <- dataframe$GAMMA_permuted_beta
  } else if (type == "em_pxl") {
    GAMMA_permuted_list <- dataframe$GAMMA_permuted_pxl
  } else {
    stop("Invalid type specified. Choose 'em', 'em_beta', or 'em_pxl'.")
  }
  
  B_true <- dataframe$B_True[[1]]  # Assuming B_true is the same across simulations
  
  # Compare each GAMMA_permuted matrix with B_true and check if they match
  match_results <- sapply(GAMMA_permuted_list, function(GAMMA_permuted) {
    isTRUE(all.equal(GAMMA_permuted, B_true))
  })
  
  # Calculate the number of exact matches
  exact_matches <- sum(match_results)
  
  return(exact_matches)
}

# Initialize an empty list to store results
comparison_results <- list()

# Loop over each dataset and type
for (subdir_path in subdir_paths) {
  # Extract dataset name
  dataset_name <- gsub("results_pxl/", "", subdir_path)
  
  # Perform analysis
  dataframe <- all_analysis_results[[dataset_name]]
  
  # Store the results in a named list
  comparison_results[[dataset_name]] <- list(
    times_correct = compare_GAMMA_to_B_true(dataframe, "em"),
    times_correct_beta = compare_GAMMA_to_B_true(dataframe, "em_beta"),
    times_correct_pxl = compare_GAMMA_to_B_true(dataframe, "em_pxl")
  )
}

# Convert the list to a data frame for easier manipulation and visualization
comparison_df <- do.call(rbind, lapply(names(comparison_results), function(name) {
  data.frame(
    Dataset = name,
    times_correct = comparison_results[[name]]$times_correct,
    times_correct_beta = comparison_results[[name]]$times_correct_beta,
    times_correct_pxl = comparison_results[[name]]$times_correct_pxl
  )
}))

# Ensure the columns are named correctly
rownames(comparison_df) <- NULL  # Remove row names
comparison_df <- comparison_df %>% 
  select(Dataset, times_correct, times_correct_beta, times_correct_pxl)  # Order the columns

# Print the table
print(comparison_df)


# Loadings box-plots ####
# Function to calculate and plot element-wise metrics (Bias, Variance, MSE)
calculate_and_plot_metric <- function(calc_function, metric_name, matrix_name, true_matrix_name = NULL) {
  all_metrics <- list()
  
  for (path in subdir_paths) {
    # Extract the structure name from the path
    structure_name <- gsub("results_pxl/", "", path)
    
    # Split the structure name into parts
    parts <- strsplit(structure_name, "-")[[1]]
    
    if (length(parts) == 2) {
      # For names like "3x3-weak"
      structure <- parts[1]
      condition <- parts[2]
    } else if (length(parts) == 3) {
      # For names like "overlap2-small-weak"
      structure <- paste(parts[1], parts[2], sep = "-")
      condition <- parts[3]
    } else {
      stop("Unexpected structure name format")
    }
    
    # Shorten the structure names
    structure <- gsub("overlap2-small", "o2-s", structure)
    structure <- gsub("overlap2-large", "o2-l", structure)
    structure <- gsub("overlap3-small", "o3-s", structure)
    structure <- gsub("overlap3-large", "o3-l", structure)
    
    # Calculate the metric for the current structure
    if (!is.null(true_matrix_name)) {
      metrics_matrix <- calc_function(all_analysis_results[[structure_name]], matrix_name, true_matrix_name)
    } else {
      metrics_matrix <- calc_function(all_analysis_results[[structure_name]], matrix_name)
    }
    
    # Flatten the matrix to a vector and convert to a data frame
    metrics_df <- data.frame(
      Value = as.vector(metrics_matrix)
    )
    
    # Add columns for the structure and condition
    metrics_df$Structure <- structure
    metrics_df$Condition <- condition
    
    # Store the metrics in the list
    all_metrics[[structure_name]] <- metrics_df
  }
  
  # Combine all metrics into a single dataframe
  metrics_combined <- do.call(rbind, all_metrics)
  
  # Ensure the structure and condition columns are treated as factors
  metrics_combined$Structure <- factor(metrics_combined$Structure, 
                                       levels = c("3x3", "5x5", "o2-s", "o2-l", "o3-s", "o3-l"))
  metrics_combined$Condition <- factor(metrics_combined$Condition, levels = c("weak", "moderate", "strong"))
  
  # Generate the title using the exact matrix name
  title_text <- paste(metric_name, "of", matrix_name, "for All Structures")
  
  # Generate the combined boxplot for the metric
  p <- ggplot(metrics_combined, aes(x = Condition, y = Value, fill = Condition)) +
    geom_boxplot() +
    labs(title = title_text, y = metric_name, x = "Condition") +
    theme_minimal() +
    scale_fill_manual(values = c("weak" = "green", "moderate" = "blue", "strong" = "red")) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"  # Remove the legend entirely
    ) +
    facet_wrap(~ Structure, nrow = 1, scales = "free_x")  # Unified facet order
  
  return(p)
}

# MSE B
plot_mse <- calculate_and_plot_metric(calculate_elementwise_MSE_matrix, "MSE", "B_permuted", "B_True")
plot_mse_beta <- calculate_and_plot_metric(calculate_elementwise_MSE_matrix, "MSE", "B_permuted_beta", "B_True")
print(plot_mse)
print(plot_mse_beta)

# MSE Covariance Matrix
plot_mse_cov <- calculate_and_plot_metric(calculate_elementwise_MSE_matrix, "MSE", "Covariance_matrix", "Covariance_matrix_true")
plot_mse_cov_beta <- calculate_and_plot_metric(calculate_elementwise_MSE_matrix, "MSE", "Covariance_matrix_beta", "Covariance_matrix_true")
print(plot_mse_cov)
print(plot_mse_cov_beta)

# Heatmaps Loadings ####
heatmap_dataframe <- all_analysis_results$`overlap2-large-strong`
# Calculate the element-wise matrices using the functions you provided
true_matrix <- heatmap_dataframe$B_True[[1]]

average_matrix <- calculate_average_matrix(heatmap_dataframe$B_permuted)
bias_matrix <- calculate_elementwise_bias_matrix(heatmap_dataframe, matrix_name = "B_permuted", true_matrix_name = "B_True")
variance_matrix <- calculate_elementwise_variance_matrix(heatmap_dataframe, matrix_name = "B_permuted")
mse_matrix <- calculate_elementwise_MSE_matrix(heatmap_dataframe, matrix_name = "B_permuted", true_matrix_name = "B_True")

average_matrix_beta <- calculate_average_matrix(heatmap_dataframe$B_permuted_beta)
bias_matrix_beta <- calculate_elementwise_bias_matrix(heatmap_dataframe, "B_permuted_beta", "B_True")
mse_matrix_beta <- calculate_elementwise_MSE_matrix(heatmap_dataframe, "B_permuted_beta", "B_True")

# Create heatmaps for each of these matrices
heatmap_true <-  create_heatmap(true_matrix, title = "Heatmap of True Matrix")
heatmap_average <- create_heatmap(average_matrix, title = "Heatmap of Average Matrix")
heatmap_bias <- create_heatmap(bias_matrix, title = "Heatmap of Bias Matrix")
heatmap_variance <- create_heatmap(variance_matrix, title = "Heatmap of Variance Matrix")
heatmap_mse <- create_heatmap(mse_matrix, title = "Heatmap of MSE Matrix")

# Print the heatmaps
print(heatmap_true)
print(heatmap_average)
print(heatmap_bias)
print(heatmap_variance)
print(heatmap_mse)
# Heatmaps factors ####
# Example usage
source("Scripts/Analysis/functions.R")
create_factor_heatmap(true_matrix, title = "True factors")
create_factor_heatmap(average_matrix, title = "Heatmap of average factors")
create_factor_heatmap(average_matrix_beta, title = "Heatmap of average factors - beta")
create_factor_heatmap(bias_matrix, title = "Heatmap of bias factors")
create_factor_heatmap(bias_matrix_beta, title = "Heatmap of bias factors - beta")
create_factor_heatmap(mse_matrix, title = "Heatmap of MSE factors")
create_factor_heatmap(mse_matrix_beta, title = "Heatmap of MSE factors - beta")


# Heatmaps Covariances ####
# Calculate the element-wise matrices using the functions you provided
average_covariance_matrix <- calculate_average_matrix(heatmap_dataframe$Covariance_matrix)
bias_covariance_matrix <- calculate_elementwise_bias_matrix(heatmap_dataframe, matrix_name = "Covariance_matrix", true_matrix_name = "Covariance_matrix_true")
variance_covariance_matrix <- calculate_elementwise_variance_matrix(heatmap_dataframe, matrix_name = "Covariance_matrix")
mse_covariance_matrix <- calculate_elementwise_MSE_matrix(heatmap_dataframe, matrix_name = "Covariance_matrix", true_matrix_name = "Covariance_matrix_true")

# Create heatmaps for each of these matrices
heatmap_true_covariance <- create_heatmap(heatmap_dataframe$Covariance_matrix_true[[1]], title = "Heatmap of True Covariance Matrix")
heatmap_average_covariance <- create_heatmap(average_covariance_matrix, title = "Heatmap of Average Covariance Matrix")
heatmap_bias_covariance <- create_heatmap(bias_covariance_matrix, title = "Heatmap of Bias Covariance Matrix")
# heatmap_variance_covariance <- create_heatmap(variance_covariance_matrix, title = "Heatmap of Variance Covariance Matrix")
heatmap_mse_covariance <- create_heatmap(mse_covariance_matrix, title = "Heatmap of MSE Covariance Matrix")

# Print the heatmaps
print(heatmap_true_covariance)
print(heatmap_average_covariance)
print(heatmap_bias_covariance)
# print(heatmap_variance_covariance)
print(heatmap_mse_covariance)

# Beta
average_covariance_matrix_beta <- calculate_average_matrix(heatmap_dataframe$Covariance_matrix_beta)
bias_covariance_matrix_beta <- calculate_elementwise_bias_matrix(heatmap_dataframe, matrix_name = "Covariance_matrix_beta", true_matrix_name = "Covariance_matrix_true")
variance_covariance_matrix_beta <- calculate_elementwise_variance_matrix(heatmap_dataframe, matrix_name = "Covariance_matrix_beta")
mse_covariance_matrix_beta <- calculate_elementwise_MSE_matrix(heatmap_dataframe, matrix_name = "Covariance_matrix_beta", true_matrix_name = "Covariance_matrix_true")

heatmap_average_covariance_beta <- create_heatmap(average_covariance_matrix, title = "Heatmap of Average Covariance Matrix")
heatmap_bias_covariance_beta <- create_heatmap(bias_covariance_matrix, title = "Heatmap of Bias Covariance Matrix")
heatmap_mse_covariance_beta <- create_heatmap(mse_covariance_matrix, title = "Heatmap of MSE Covariance Matrix")

print(heatmap_average_covariance_beta)
print(heatmap_bias_covariance_beta)
print(heatmap_mse_covariance_beta)
