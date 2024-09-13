rm(list = ls())
source("Scripts/data_generation.R")

# initialisation
n <- 500
dim1 <- 20
dim2 <- 20
q <- 3
set.seed(13)
data <- generate_data_medium(n, dim1, dim2, q, "moderate", TRUE)

find_overlap_indices <- function(B_factors) {
  # Get dimensions
  dim1 <- dim(B_factors)[1]
  dim2 <- dim(B_factors)[2]
  
  # Initialize an array to store the sum across the factor dimensions
  overlap_matrix <- apply(B_factors, c(1, 2), sum)
  
  # Find indices where the sum is greater than 1 (indicating overlap)
  overlap_indices <- which(overlap_matrix > 1, arr.ind = TRUE)
  
  # Convert (row, column) indices to single index
  single_indices <- (overlap_indices[,1] - 1) * dim2 + overlap_indices[,2]
  
  return(single_indices)
}

# Example usage
set.seed(123)

# Find overlapping indices
overlapping_indices <- find_overlap_indices(data$B_factors)
print(overlapping_indices)

# Example: assuming df is your 400x400 dataframe
# Function to get the flat indices of the 100 maximum values
get_top_100_indices <- function(df) {
  # Get the dimensions of the dataframe
  dim1 <- nrow(df)
  dim2 <- ncol(df)
  
  # Flatten the dataframe into a vector
  flattened_values <- as.vector(as.matrix(df))
  
  # Get the indices of the top 100 largest absolute differences from 0
  top_100_indices <- order(abs(flattened_values), decreasing = TRUE)[1:100]
  
  # Convert the flat indices to row indices (disregarding columns)
  row_indices <- ((top_100_indices - 1) %% dim1) + 1
  
  # Return the unique row indices corresponding to the top 100 values
  return(unique(row_indices))
}

# Example usage with a 400x400 dataframe df
# df <- matrix(rnorm(400*400), nrow = 400, ncol = 400)  # Generate a random dataframe for testing
top_100_indices <- get_top_100_indices(statistics_em$bias_matrix)
print(top_100_indices)


# View(data$Covariance_matrix_true)
# View(data$B_true)
stop()
# Remove diagonal elements (which are 1) and zero elements
non_zero_values <- data$cor_matrix[data$cor_matrix != 0 & data$cor_matrix != 1]

# Print the results
print(range(non_zero_values))


View(data$B_true)
View(data$cor_matrix)
# checking if PSI remains same ####
set.seed(13)
data <- generate_data_medium(n, dim1, dim2, q, "weak", FALSE)
set.seed(14)
data2 <- generate_data_medium(n, dim1, dim2, q, "weak", FALSE)
identical(data$PSI_true, data2$PSI_true)
identical(data$Y, data2$Y)

View(data$PSI_true)
View(data2$PSI_true)
View(data$Y)
View(data2$Y)

# data <- generate_data_2overlap(n, dim1, dim2, q, overlap = "big",
#                          print_factors = TRUE)
# 
# data <- generate_data_3overlap(n, dim1, dim2, q, overlap = "big",
#                                  print_factors = TRUE)
# 
# data <- generate_data_psi(n, dim1, dim2, q, square_size = 4, scale = 10,
#                          print_factors = TRUE)
# 
# # Vertical gradient
# result_vertical <- generate_data_vertical_gradient(n = 100, dim1 = 10,
#                                                    dim2 = 10, q = 3,
#                                                    print_factors = TRUE)

