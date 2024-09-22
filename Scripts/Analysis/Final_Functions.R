library(ggplot2)
library(reshape2)
library(cowplot)

# Function to calculate element-wise average matrix for a given matrix type
calculate_elementwise_average_matrix <- function(matrix_list) {
  abs_matrix_list <- lapply(matrix_list, abs)
  Reduce(`+`, abs_matrix_list) / length(matrix_list)
}
# Function to calculate element-wise bias matrix for a given matrix type
calculate_elementwise_bias_matrix <- function(matrix_list, true_matrix) {
  # Calculate the mean matrix (average of all matrices) 
  mean_matrix <- calculate_elementwise_average_matrix(matrix_list)
  
  # Calculate the bias matrix as the difference between the mean matrix and the true matrix
  bias_matrix <- mean_matrix - true_matrix
  
  return(bias_matrix)
}
# Function to calculate variance for a given matrix type
calculate_elementwise_variance_matrix <- function(matrix_list) {
  # Calculate the mean matrix using the calculate_average_matrix function
  mean_matrix <- calculate_elementwise_average_matrix(matrix_list)
  
  # Extract the number of matrices and the dimensions of one matrix
  num_matrices <- length(matrix_list)
  matrix_dim <- dim(matrix_list[[1]])
  
  # Initialize the variance matrix
  variance_matrix <- matrix(0, nrow = matrix_dim[1], ncol = matrix_dim[2])
  
  # Loop through each matrix to calculate the sum of squared differences
  for (i in 1:num_matrices) {
    variance_matrix <- variance_matrix + (matrix_list[[i]] - mean_matrix)^2
  }
  
  # The variance matrix is the sum of squared differences divided by (number of matrices - 1)
  variance_matrix <- variance_matrix / (num_matrices - 1)
  
  return(variance_matrix)
}
# Function to calculate element-wise MSE matrix for a given matrix type
calculate_elementwise_MSE_matrix <- function(matrix_list, true_matrix) {
  # Extract the number of matrices and the dimensions of one matrix
  num_matrices <- length(matrix_list)
  matrix_dim <- dim(matrix_list[[1]])
  
  # Initialize a matrix to store the sum of squared differences
  MSE_matrix <- matrix(0, nrow = matrix_dim[1], ncol = matrix_dim[2])
  
  # Loop through each matrix to compute the sum of squared differences
  for (i in 1:num_matrices) {
    MSE_matrix <- MSE_matrix + (abs(matrix_list[[i]]) - true_matrix)^2
  }
  
  # The MSE matrix is the average of the squared differences
  MSE_matrix <- MSE_matrix / num_matrices
  
  return(MSE_matrix)
}

# Calculate all statistic matrices for model
calculate_statistics <- function(dataframe, type = "em") {
  # Retrieve B_true and Covariance_true from the dataframe
  B_true <- dataframe$B_True[[1]]
  Covariance_true <- dataframe$Covariance_matrix_true[[1]]
  
  # Select appropriate matrices based on the type
  if (type == "em") {
    permuted_matrices <- dataframe$B_permuted
    covariance_matrices <- dataframe$Covariance_matrix
  } else if (type == "em_beta") {
    permuted_matrices <- dataframe$B_permuted_beta
    covariance_matrices <- dataframe$Covariance_matrix_beta
  } else if (type == "em_pxl") {
    permuted_matrices <- dataframe$B_permuted_pxl
    covariance_matrices <- dataframe$Covariance_matrix_pxl
  } else if (type == "em_beta_pxl") {
    permuted_matrices <- dataframe$B_permuted_beta_pxl
    covariance_matrices <- dataframe$Covariance_matrix_beta_pxl
  } else {
    stop("Invalid type specified. Choose 'em', 'em_beta', or 'em_pxl' or 'em_beta_pxl'.")
  }
  
  # For B matrices
  average_matrix <- calculate_elementwise_average_matrix(permuted_matrices)
  bias_matrix <- calculate_elementwise_bias_matrix(permuted_matrices, B_true)
  variance_matrix <- calculate_elementwise_variance_matrix(permuted_matrices)
  MSE_matrix <- calculate_elementwise_MSE_matrix(permuted_matrices, B_true)
  
  # For Covariance matrices
  average_covariance <- calculate_elementwise_average_matrix(covariance_matrices)
  bias_covariance <- calculate_elementwise_bias_matrix(covariance_matrices, Covariance_true)
  variance_covariance <- calculate_elementwise_variance_matrix(covariance_matrices)
  MSE_covariance <- calculate_elementwise_MSE_matrix(covariance_matrices, Covariance_true)
  
  # Return all calculated statistics
  return(list(
    B_true = B_true,
    Covariance_true = Covariance_true,
    average_matrix = average_matrix,
    bias_matrix = bias_matrix,
    variance_matrix = variance_matrix,
    MSE_matrix = MSE_matrix,
    average_covariance = average_covariance,
    bias_covariance = bias_covariance,
    variance_covariance = variance_covariance,
    MSE_covariance = MSE_covariance
  ))
}

# Split the loadings matrix into submatrices based on its dimensions
split_loadings_to_matrices <- function(loadings_matrix) {
  n_rows <- nrow(loadings_matrix)
  n_cols <- ncol(loadings_matrix)
  
  # Check if the number of rows is a perfect square
  if (sqrt(n_rows) %% 1 != 0) {
    stop("The number of rows in the matrix must be a perfect square (e.g., 100, 400).")
  }
  
  # Calculate the dimension of the square submatrices
  submatrix_size <- sqrt(n_rows)
  
  # Split the matrix into a list of submatrices
  matrices_list <- list()
  for (i in 1:n_cols) {
    factor_column <- loadings_matrix[, i]
    matrices_list[[i]] <- matrix(factor_column, nrow = submatrix_size, ncol = submatrix_size, byrow = FALSE)
  }
  
  return(matrices_list)
}

# Create a custom heatmap for one of the matrices
create_custom_heatmap <- function(matrix_data, show_legend = TRUE, common_scale = NULL, show_grid = TRUE, title = NULL) {
  ggplot(melt(matrix_data), aes(x = Var2, y = Var1, fill = value)) +
    geom_tile(color = if (show_grid) "gray" else NA) +  # Optionally show grid lines
    scale_y_reverse() +  
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, 
                         limits = common_scale) +  # Use the common scale for the fill
    theme_minimal() +
    theme(
      legend.position = if (show_legend) "right" else "none",  # Toggle legend
      legend.justification = "center",
      aspect.ratio = 1,  # Ensure square tiles
      axis.title = element_blank(),  # Remove axis titles
      axis.text = element_blank(),   # Remove axis text
      axis.ticks = element_blank(),  # Remove axis ticks
      panel.grid = element_blank(),  # Remove panel grid lines
      plot.margin = margin(0, 0, 0, 0),  # Remove plot margin
      plot.title = element_text(hjust = 0.5, margin = margin(b = 1))  # Center the title
    ) +
    coord_fixed() +  # Fix aspect ratio to make tiles square
    annotate("rect", xmin = 0.5, xmax = ncol(matrix_data) + 0.5, 
             ymin = 0.5, ymax = nrow(matrix_data) + 0.5, 
             color = "black", fill = NA, size = 1) +  # Add border around the matrix
    ggtitle(title)  # Add optional title
}

# Not used
create_factor_heatmap <- function(loadings_matrix, title = "True factors") {
  # Function to extract the legend from a ggplot object
  get_legend <- function(ggplot_obj) {
    legend <- cowplot::get_legend(ggplot_obj + theme(legend.position = "right"))
    return(legend)
  }
  
  # Split the loadings matrix into submatrices
  split_matrices <- split_loadings_to_matrices(loadings_matrix)
  
  # Calculate the common scale (min and max values across all matrices)
  common_scale <- range(unlist(split_matrices))
  
  # Create heatmaps for each matrix using the common scale
  real_factor1 <- create_custom_heatmap(split_matrices[[1]], show_legend = FALSE, common_scale = common_scale)
  real_factor2 <- create_custom_heatmap(split_matrices[[2]], show_legend = FALSE, common_scale = common_scale)
  real_factor3 <- create_custom_heatmap(split_matrices[[3]], show_legend = TRUE, common_scale = common_scale)
  
  # Extract the legend from the third plot
  legend <- get_legend(real_factor3)
  
  # Remove the legend from the third plot
  real_factor3 <- real_factor3 + theme(legend.position = "none")
  
  # Combine the heatmaps side by side with the extracted legend
  final_plot <- plot_grid(
    plot_grid(real_factor1, real_factor2, real_factor3, ncol = 3, align = "h", axis = "tb"),
    legend,
    ncol = 2,
    rel_widths = c(3, 0.3)
  )
  
  # Print the final plot
  print(final_plot)
}


