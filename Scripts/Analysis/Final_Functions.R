# Function to calculate element-wise average matrix for a given matrix type
calculate_elementwise_average_matrix <- function(matrix_list) {
  Reduce(`+`, matrix_list) / length(matrix_list)
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
    MSE_matrix <- MSE_matrix + (matrix_list[[i]] - true_matrix)^2
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