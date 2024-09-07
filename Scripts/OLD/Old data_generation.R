library(mvtnorm)

# Function to generate B, PSI, and Y for 2D data 
generate_data <- function(n, dim1, dim2, q, square_size = 4, 
                          print_factors = FALSE){
  # Number of parameters
  p <- dim1 * dim2
  
  # Set dimensions and initialize the B_factors array
  B_factors <- array(0, dim = c(dim1, dim2, q))
  
  # Define square positions based on square_size
  if (square_size == 2) {
    B_factors[1:2, 1:2, 1] <- 1        # Top left
    B_factors[9:10, 5:6, 2] <- 1       # Bottom middle
    B_factors[1:2, 9:10, 3] <- 1       # Top right
  } else if (square_size == 3) {
    B_factors[1:3, 1:3, 1] <- 1        # Top left
    B_factors[8:10, 4:6, 2] <- 1       # Bottom middle
    B_factors[1:3, 8:10, 3] <- 1       # Top right
  } else if (square_size == 4) {
    B_factors[1:4, 1:4, 1] <- 1        # Top left
    B_factors[7:10, 4:7, 2] <- 1       # Bottom middle
    B_factors[1:4, 7:10, 3] <- 1       # Top right
  } else if (square_size == 5) {
    B_factors[1:5, 1:5, 1] <- 1        # Top left
    B_factors[6:10, 3:7, 2] <- 1       # Bottom middle
    B_factors[1:5, 6:10, 3] <- 1       # Top right
  } else {
    stop("Invalid square_size. Choose either 2, 3, 4, or 5.")
  }
  
  if (print_factors == TRUE) {
    print(B_factors[, , 1])
    print(B_factors[, , 2])
    print(B_factors[, , 3])
  }
  
  # Convert slices to column vectors and combine into a matrix
  B_true <- do.call(cbind, lapply(1:q, function(i) 
    as.vector(B_factors[, , i])))
  
  # Generate noise covariance matrix and synthetic data
  
  PSI_true <- diag(runif(p, 2.5, 7.5))
  Sigma_true <- B_true %*% t(B_true) + PSI_true
  Y <- rmvnorm(n, mean = rep(0, p), sigma = Sigma_true)
  
  cor_matrix <- cov2cor(Sigma_true)
  
  return(list(Y = Y, B_true = B_true, PSI_true = PSI_true, 
              B_factors = B_factors, Sigma_true = Sigma_true, 
              cor_matrix = cor_matrix))
}

# Function to generate B, PSI, and Y for 2D data with slightly overlapping squares
generate_data_2overlap <- function(n, dim1, dim2, q, overlap = "small", 
                                   print_factors = FALSE){
  # Number of parameters
  p <- dim1 * dim2
  
  # Set dimensions and initialize the B_factors array
  B_factors <- array(0, dim = c(dim1, dim2, q))
  
  # Define overlapping squares based on square_size
  if (overlap == "small") {
    B_factors[1:4, 1:4, 1] <- 1        # Top left
    B_factors[3:6, 3:6, 2] <- 1        # Overlapping with first square
    B_factors[1:4, 7:10, 3] <- 1       # Top right
  } else if (overlap == "big") {
    B_factors[1:10, 1:4, 1] <- 1       # Top left
    B_factors[6:10, 1:10, 2] <- 1      # Overlapping with first square
    B_factors[1:4, 7:10, 3] <- 1       # Top right
  } else {
    stop("Invalid square_size. Choose either small, or big.")
  }
  
  if (print_factors == TRUE) {
    print(B_factors[, , 1])
    print(B_factors[, , 2])
    print(B_factors[, , 3])
  }
  
  # Convert slices to column vectors and combine into a matrix
  B_true <- do.call(cbind, lapply(1:q, function(i) 
    as.vector(B_factors[, , i])))
  
  # Generate noise covariance matrix and synthetic data
  PSI_true <- diag(runif(p, 0.05, 0.15))
  Sigma_true <- B_true %*% t(B_true) + PSI_true
  Y <- rmvnorm(n, mean = rep(0, p), sigma = Sigma_true)
  
  cor_matrix <- cov2cor(Sigma_true)
  
  return(list(Y = Y, B_true = B_true, PSI_true = PSI_true, 
              B_factors = B_factors, Sigma_true = Sigma_true, 
              cor_matrix = cor_matrix))
}

# Function to generate B, PSI, and Y for 2D data with slightly overlapping squares
generate_data_3overlap <- function(n, dim1, dim2, q, overlap = "small", 
                                   print_factors = FALSE){
  # Number of parameters
  p <- dim1 * dim2
  
  # Set dimensions and initialize the B_factors array
  B_factors <- array(0, dim = c(dim1, dim2, q))
  
  # Define overlapping squares based on square_size
  if (overlap == "small") {
    B_factors[1:5, 1:5, 1] <- 1        # Top left
    B_factors[4:7, 4:7, 2] <- 1        # Overlapping with first square
    B_factors[6:10, 6:10, 3] <- 1       # Top right
  } else if (overlap == "big") {
    B_factors[1:5, 1:5, 1] <- 1       # Top left
    B_factors[3:9, 3:5, 2] <- 1      # Overlapping with first square
    B_factors[1:3, 3:9, 3] <- 1       # Top right
  } else {
    stop("Invalid square_size. Choose either small, or big.")
  }
  
  if (print_factors == TRUE) {
    print(B_factors[, , 1])
    print(B_factors[, , 2])
    print(B_factors[, , 3])
  }
  
  # Convert slices to column vectors and combine into a matrix
  B_true <- do.call(cbind, lapply(1:q, function(i) 
    as.vector(B_factors[, , i])))
  
  # Generate noise covariance matrix and synthetic data
  PSI_true <- diag(runif(p, 0.05, 0.15))
  Sigma_true <- B_true %*% t(B_true) + PSI_true
  Y <- rmvnorm(n, mean = rep(0, p), sigma = Sigma_true)
  
  cor_matrix <- cov2cor(Sigma_true)
  
  return(list(Y = Y, B_true = B_true, PSI_true = PSI_true, 
              B_factors = B_factors, Sigma_true = Sigma_true, 
              cor_matrix = cor_matrix))
}

# Function to generate B, PSI, and Y for 2D data 
generate_data_psi <- function(n, dim1, dim2, q, square_size = 4, scale = 1, 
                              print_factors = FALSE){
  # Number of parameters
  p <- dim1 * dim2
  
  # Set dimensions and initialize the B_factors array
  B_factors <- array(0, dim = c(dim1, dim2, q))
  
  # Define square positions based on square_size
  if (square_size == 2) {
    B_factors[1:2, 1:2, 1] <- 1        # Top left
    B_factors[9:10, 5:6, 2] <- 1       # Bottom middle
    B_factors[1:2, 9:10, 3] <- 1       # Top right
  } else if (square_size == 3) {
    B_factors[1:3, 1:3, 1] <- 1        # Top left
    B_factors[8:10, 4:6, 2] <- 1       # Bottom middle
    B_factors[1:3, 8:10, 3] <- 1       # Top right
  } else if (square_size == 4) {
    B_factors[1:4, 1:4, 1] <- 1        # Top left
    B_factors[7:10, 4:7, 2] <- 1       # Bottom middle
    B_factors[1:4, 7:10, 3] <- 1       # Top right
  } else if (square_size == 5) {
    B_factors[1:5, 1:5, 1] <- 1        # Top left
    B_factors[6:10, 3:7, 2] <- 1       # Bottom middle
    B_factors[1:5, 6:10, 3] <- 1       # Top right
  } else {
    stop("Invalid square_size. Choose either 2, 3, 4, or 5.")
  }
  
  if (print_factors == TRUE) {
    print(B_factors[, , 1])
    print(B_factors[, , 2])
    print(B_factors[, , 3])
  }
  
  # Convert slices to column vectors and combine into a matrix
  B_true <- do.call(cbind, lapply(1:q, function(i) 
    as.vector(B_factors[, , i])))
  
  # Generate noise covariance matrix and synthetic data
  
  PSI_true <- diag(runif(p, 0.05*scale, 0.15*scale))
  Sigma_true <- B_true %*% t(B_true) + PSI_true
  Y <- rmvnorm(n, mean = rep(0, p), sigma = Sigma_true)
  
  return(list(Y = Y, B_true = B_true, PSI_true = PSI_true, 
              B_factors = B_factors, Sigma_true = Sigma_true))
}

# Function to generate vertical gradient patterns
generate_data_vertical_gradient <- function(n, dim1, dim2, q, print_factors = FALSE) {
  p <- dim1 * dim2
  
  # Initialize the B_factors array
  B_factors <- array(0, dim = c(dim1, dim2, q))
  
  # Define distinct vertical gradient patterns for each factor
  B_factors[, , 1] <- matrix(seq(0.1, 1, length.out = dim1), nrow = dim1, ncol = dim2)  # Increasing from top to bottom
  B_factors[, , 2] <- matrix(seq(1, 0.1, length.out = dim1), nrow = dim1, ncol = dim2)  # Decreasing from top to bottom
  B_factors[, , 3] <- matrix(rep(c(seq(0.1, 1, length.out = dim1/2), 
                                   rev(seq(0.1, 1, length.out = dim1/2))), each = dim2), 
                             nrow = dim1, ncol = dim2, byrow = TRUE)
  
  if (print_factors == TRUE) {
    print(B_factors[, , 1])
    print(B_factors[, , 2])
    print(B_factors[, , 3])
  }
  
  # Convert slices to column vectors and combine into a matrix
  B_true <- do.call(cbind, lapply(1:q, function(i) as.vector(B_factors[, , i])))
  
  # Generate noise covariance matrix and synthetic data
  PSI_true <- diag(runif(p, 0.05, 0.15))
  Sigma_true <- B_true %*% t(B_true) + PSI_true
  Y <- rmvnorm(n, mean = rep(0, p), sigma = Sigma_true)
  
  return(list(Y = Y, B_true = B_true, PSI_true = PSI_true, B_factors = B_factors))
}

# Function to generate horizontal gradient patterns
generate_data_horizontal_gradient <- function(n, dim1, dim2, q, print_factors = FALSE) {
  p <- dim1 * dim2
  
  # Initialize the B_factors array
  B_factors <- array(0, dim = c(dim1, dim2, q))
  
  # Define distinct horizontal gradient patterns for each factor
  B_factors[, , 1] <- t(matrix(seq(0.1, 1, length.out = dim2), nrow = dim2, ncol = dim1))  # Increasing from left to right
  B_factors[, , 2] <- t(matrix(seq(1, 0.1, length.out = dim2), nrow = dim2, ncol = dim1))  # Decreasing from left to right
  B_factors[, , 3] <- matrix(c(rep(seq(0.1, 1, length.out = dim1/2), each=dim2), rep(seq(1, 0.1, length.out = dim1/2), each=dim2)), nrow = dim1, ncol = dim2) # Peak in middle
  
  if (print_factors == TRUE) {
    print(B_factors[, , 1])
    print(B_factors[, , 2])
    print(B_factors[, , 3])
  }
  
  # Convert slices to column vectors and combine into a matrix
  B_true <- do.call(cbind, lapply(1:q, function(i) as.vector(B_factors[, , i])))
  
  # Generate noise covariance matrix and synthetic data
  PSI_true <- diag(runif(p, 0.05, 0.15))
  Sigma_true <- B_true %*% t(B_true) + PSI_true
  Y <- rmvnorm(n, mean = rep(0, p), sigma = Sigma_true)
  
  return(list(Y = Y, B_true = B_true, PSI_true = PSI_true, B_factors = B_factors))
}

# Function to generate diagonal gradient pattern
generate_data_diagonal_gradient <- function(n, dim1, dim2, q, print_factors = FALSE) {
  p <- dim1 * dim2
  
  # Initialize the B_factors array
  B_factors <- array(0, dim = c(dim1, dim2, q))
  
  # Define distinct diagonal gradient patterns for each factor
  B_factors[, , 1] <- outer(1:dim1, 1:dim2, function(x, y) (x + y) / (dim1 + dim2))  # Top-left to bottom-right (increasing)
  B_factors[, , 2] <- outer(1:dim1, 1:dim2, function(x, y) ((dim1 + dim2) - (x + y) + 2) / (dim1 + dim2))  # Bottom-left to top-right (decreasing)
  
  # For the third factor, set the middle to 1 and decrease as you move away
  center_x <- (dim1 + 1) / 2
  center_y <- (dim2 + 1) / 2
  max_distance <- sqrt((center_x - 1)^2 + (center_y - 1)^2)
  
  B_factors[, , 3] <- outer(1:dim1, 1:dim2, function(x, y) 1 - sqrt((x - center_x)^2 + (y - center_y)^2) / max_distance)
  
  # Set the middle 4 cells to exactly 1
  B_factors[5:6, 5:6, 3] <- 1
  
  if (print_factors == TRUE) {
    print(B_factors[, , 1])
    print(B_factors[, , 2])
    print(B_factors[, , 3])
  }
  
  # Convert slices to column vectors and combine into a matrix
  B_true <- do.call(cbind, lapply(1:q, function(i) as.vector(B_factors[, , i])))
  
  # Generate noise covariance matrix and synthetic data
  PSI_true <- diag(runif(p, 0.05, 0.15))
  Sigma_true <- B_true %*% t(B_true) + PSI_true
  Y <- rmvnorm(n, mean = rep(0, p), sigma = Sigma_true)
  
  return(list(Y = Y, B_true = B_true, PSI_true = PSI_true, B_factors = B_factors))
}


# # Example usage:
# set.seed(42)
# 
# # Vertical gradient
# result_vertical <- generate_data_vertical_gradient(n = 100, dim1 = 10,
#                                                    dim2 = 10, q = 3,
#                                                    print_factors = TRUE)
# 
# # Horizontal gradient
# result_horizontal <- generate_data_horizontal_gradient(n = 100, dim1 = 10,
#                                                        dim2 = 10, q = 3,
#                                                        print_factors = TRUE)
# 
# # Diagonal gradient
# result_diagonal <- generate_data_diagonal_gradient(n = 100, dim1 = 10,
#                                                    dim2 = 10, q = 3,
#                                                    print_factors = TRUE)



