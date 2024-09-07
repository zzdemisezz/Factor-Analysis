# Check if the 'mvtnorm' package is installed
if (!requireNamespace("mvtnorm", quietly = TRUE)) {
  install.packages("Scripts/mvtnorm_1.2-6.tar.gz", repos = NULL, type="source")
}

library(mvtnorm)
# Function to generate data based on the specific 3-factor structure
generate_data_simple <- function(n, dim1 = 20, dim2 = 20, q = 3, 
                                         corr = c("weak", "moderate", "strong"), 
                                         print_factors = FALSE) {
  
  # Number of parameters (features in the data)
  p <- dim1 * dim2
  
  # Match the correlation argument
  corr <- match.arg(corr)
  
  # Set the PSI value range based on the correlation type
  psi_range <- switch(corr,
                      "weak" = c(2.5, 7.5),
                      "moderate" = c(0.5, 1.5),
                      "strong" = c(0.05, 0.15))
  
  # Create the B_factors array based on the pre-defined matrices
  B_factors <- array(0, dim = c(dim1, dim2, q))
  
  # Using the given block structures
  # 7x7 block in the top-left for factor 1
  B_factors[1:7, 1:7, 1] <- 1        # Top-left block
  
  # 6x6 block in the center for factor 2
  B_factors[8:13, 8:13, 2] <- 1      # Center block
  
  # 7x7 block in the bottom-right for factor 3
  B_factors[14:20, 14:20, 3] <- 1    # Bottom-right block
  
  # Optionally print the factor matrices
  if (print_factors) {
    print(B_factors[, , 1])
    print(B_factors[, , 2])
    print(B_factors[, , 3])
  }
  
  # Convert slices to column vectors and combine into a matrix
  B_true <- do.call(cbind, lapply(1:q, function(i) as.vector(B_factors[, , i])))
  
  # Set a specific seed for generating PSI_true
  old_seed <- .Random.seed  # Save the current random seed
  set.seed(12)  # Set a seed specifically for generating PSI_true
  PSI_true <- diag(runif(p, min = psi_range[1], max = psi_range[2]))
  .Random.seed <<- old_seed  # Restore the previous random seed
  
  # Generate noise covariance matrix and synthetic data
  Covariance_matrix_true <- B_true %*% t(B_true) + PSI_true
  Y <- rmvnorm(n, mean = rep(0, p), sigma = Covariance_matrix_true)
  
  # Convert covariance matrix to correlation matrix
  cor_matrix <- cov2cor(Covariance_matrix_true)
  
  return(list(Y = Y, B_true = B_true, PSI_true = PSI_true, 
              B_factors = B_factors, Covariance_matrix_true = Covariance_matrix_true, 
              cor_matrix = cor_matrix))
}

# Function to generate the medium structure
generate_data_medium <- function(n, dim1 = 20, dim2 = 20, q = 3, 
                                 corr = c("weak", "moderate", "strong"), 
                                 print_factors = FALSE) {
  
  # Number of parameters (features in the data)
  p <- dim1 * dim2
  
  # Match the correlation argument
  corr <- match.arg(corr)
  
  # Set the PSI value range based on the correlation type
  psi_range <- switch(corr,
                      "weak" = c(2.5, 7.5),
                      "moderate" = c(0.5, 1.5),
                      "strong" = c(0.05, 0.15))
  
  # Create the B_factors array based on the provided structures
  B_factors <- array(0, dim = c(dim1, dim2, q))
  
  ## Factor 1: Circle, moved 2 rows down and 1 column to the right
  for (i in 1:dim1) {
    for (j in 1:dim2) {
      if (sqrt((i - 11)^2 + (j - 7)^2) <= 6) {  # Circle with radius 6, center at (11, 7)
        B_factors[i, j, 1] <- 1
      }
    }
  }
  
  ## Factor 2: Equilateral triangle, moved 2 columns to the right
  height <- 8
  center <- 12
  for (i in 18:(18 - height)) {
    left_bound <- center - (18 - i)
    right_bound <- center + (18 - i)
    B_factors[i, left_bound:right_bound, 2] <- 1  # Equilateral triangle
  }
  
  ## Factor 3: Nested L-shape
  B_factors[2:13, 6:9, 3] <- 1  # Vertical part of the L
  B_factors[2:6, 7:19, 3] <- 1  # Horizontal part of the L
  
  # Optionally print the factor matrices
  if (print_factors) {
    print(B_factors[, , 1])
    print(B_factors[, , 2])
    print(B_factors[, , 3])
  }
  
  # Convert slices to column vectors and combine into a matrix
  B_true <- do.call(cbind, lapply(1:q, function(i) as.vector(B_factors[, , i])))
  
  # Set a specific seed for generating PSI_true
  old_seed <- .Random.seed  # Save the current random seed
  set.seed(12)  # Set a seed specifically for generating PSI_true
  PSI_true <- diag(runif(p, min = psi_range[1], max = psi_range[2]))
  .Random.seed <<- old_seed  # Restore the previous random seed
  
  # Generate noise covariance matrix and synthetic data
  Covariance_matrix_true <- B_true %*% t(B_true) + PSI_true
  Y <- rmvnorm(n, mean = rep(0, p), sigma = Covariance_matrix_true)
  
  # Convert covariance matrix to correlation matrix
  cor_matrix <- cov2cor(Covariance_matrix_true)
  
  return(list(Y = Y, B_true = B_true, PSI_true = PSI_true, 
              B_factors = B_factors, Covariance_matrix_true = Covariance_matrix_true, 
              cor_matrix = cor_matrix))
}

generate_data_hard <- function(n, dim1 = 20, dim2 = 20, q = 3, 
                               corr = c("weak", "moderate", "strong"), 
                               print_factors = FALSE) {
  
  # Number of parameters (features in the data)
  p <- dim1 * dim2
  
  # Match the correlation argument
  corr <- match.arg(corr)
  
  # Set the PSI value range based on the correlation type
  psi_range <- switch(corr,
                      "weak" = c(2.5, 7.5),
                      "moderate" = c(0.5, 1.5),
                      "strong" = c(0.05, 0.15))
  
  # Create the B_factors array based on the provided structures
  B_factors <- array(0, dim = c(dim1, dim2, q))
  
  ## Factor 1: Circle in top-left, larger triangle in bottom-right (wider)
  # Draw a circle in the top-left
  for (i in 1:dim1) {
    for (j in 1:dim2) {
      if (sqrt((i-6)^2 + (j-6)^2) <= 5) {  # Circle with radius 5, center at (6, 6)
        B_factors[i, j, 1] <- 1
      }
    }
  }
  
  # Draw a larger triangle in the bottom-right (make it wider by extending the left bound)
  height <- 8
  for (i in 12:20) {
    left_bound <- 12  # Moved 1 column to the left (was 13)
    right_bound <- 20 - (20 - i)
    B_factors[i, left_bound:right_bound, 1] <- 1
  }
  
  ## Factor 2: Enlarged square in top-center, larger diamond in bottom-left, extended vertical rectangle
  # Draw an enlarged square (rectangle) in the top center
  B_factors[2:7, 7:13, 2] <- 1  # Enlarged top-center square
  
  # Draw a larger diamond in the bottom-left
  for (i in 13:20) {
    offset <- abs(16 - i)
    B_factors[i, (5 - offset):(5 + offset), 2] <- 1
  }
  
  # Draw a longer vertical rectangle on the right side
  B_factors[3:18, 16:18, 2] <- 1  # Extended vertical rectangle
  
  ## Factor 3: Rotated T-shape and a heart
  # Move the T-shape 1 column to the left and 1 row up
  B_factors[1:16, 4:7, 3] <- 1  # Vertical part of the T
  B_factors[7:10, 4:17, 3] <- 1  # Horizontal part of the T
  
  # Draw a larger heart, moved up by 1 row
  for (i in 12:19) {
    for (j in 11:20) {
      if (((i-16)^2)/2 + ((j-12)^2)/3 <= 1 || ((i-16)^2)/2 + ((j-19)^2)/3 <= 1 || 
          (i >= 16 && (j-16)^2 + (i-16)^2 <= 10)) {  # Heart shape equation
        B_factors[i, j, 3] <- 1
      }
    }
  }
  B_factors[14, c(12, 19), 3] <- 1  # Extra points for the heart
  B_factors[20, 16, 3] <- 1  # Another extra point
  
  # Optionally print the factor matrices
  if (print_factors) {
    print(B_factors[, , 1])
    print(B_factors[, , 2])
    print(B_factors[, , 3])
  }
  
  # Convert slices to column vectors and combine into a matrix
  B_true <- do.call(cbind, lapply(1:q, function(i) as.vector(B_factors[, , i])))
  
  # Set a specific seed for generating PSI_true
  old_seed <- .Random.seed  # Save the current random seed
  set.seed(12)  # Set a seed specifically for generating PSI_true
  PSI_true <- diag(runif(p, min = psi_range[1], max = psi_range[2]))
  .Random.seed <<- old_seed  # Restore the previous random seed
  
  # Generate noise covariance matrix and synthetic data
  Covariance_matrix_true <- B_true %*% t(B_true) + PSI_true
  Y <- rmvnorm(n, mean = rep(0, p), sigma = Covariance_matrix_true)
  
  # Convert covariance matrix to correlation matrix
  cor_matrix <- cov2cor(Covariance_matrix_true)
  
  return(list(Y = Y, B_true = B_true, PSI_true = PSI_true, 
              B_factors = B_factors, Covariance_matrix_true = Covariance_matrix_true, 
              cor_matrix = cor_matrix))
}

# Function to generate B, PSI, and Y for 2D data 
generate_data <- function(n, dim1, dim2, q, square_size = 5, 
                          corr = c("weak", "moderate", "strong"), 
                          print_factors = FALSE) {
  # Number of parameters
  p <- dim1 * dim2
  
  # Match the correlation argument
  corr <- match.arg(corr)
  
  # Set the PSI value range based on the correlation type
  psi_range <- switch(corr,
                      "weak" = c(2.5, 7.5),
                      "moderate" = c(0.5, 1.5),
                      "strong" = c(0.05, 0.15))
  
  # Set dimensions and initialize the B_factors array
  B_factors <- array(0, dim = c(dim1, dim2, q))
  
  # Define square positions based on square_size
    if (square_size == 3) {
    B_factors[1:3, 1:3, 1] <- 1        # Top left
    B_factors[8:10, 4:6, 2] <- 1       # Bottom middle
    B_factors[1:3, 8:10, 3] <- 1       # Top right
  } else if (square_size == 5) {
    B_factors[1:5, 1:5, 1] <- 1        # Top left
    B_factors[6:10, 3:7, 2] <- 1       # Bottom middle
    B_factors[1:5, 6:10, 3] <- 1       # Top right
  } else {
    stop("Invalid square_size. Choose either 3 or 5.")
  }
  
  if (print_factors == TRUE) {
    print(B_factors[, , 1])
    print(B_factors[, , 2])
    print(B_factors[, , 3])
  }
  
  # Convert slices to column vectors and combine into a matrix
  B_true <- do.call(cbind, lapply(1:q, function(i) 
    as.vector(B_factors[, , i])))
  
  # Set a specific seed for generating PSI_true
  old_seed <- .Random.seed  # Save the current random seed
  set.seed(12)  # Set a seed specifically for generating PSI_true
  PSI_true <- diag(runif(p, min = psi_range[1], max = psi_range[2]))
  .Random.seed <<- old_seed  # Restore the previous random seed
  
  # Generate noise covariance matrix and synthetic data
  Covariance_matrix_true <- B_true %*% t(B_true) + PSI_true
  Y <- rmvnorm(n, mean = rep(0, p), sigma = Covariance_matrix_true)
  
  cor_matrix <- cov2cor(Covariance_matrix_true)
  
  return(list(Y = Y, B_true = B_true, PSI_true = PSI_true, 
              B_factors = B_factors, Covariance_matrix_true = Covariance_matrix_true, 
              cor_matrix = cor_matrix))
}

# Function to generate B, PSI, and Y for 2D data with slightly overlapping squares
generate_data_2overlap <- function(n, dim1, dim2, q, overlap = "small", 
                                   corr = c("weak", "moderate", "strong"), 
                                   print_factors = FALSE) {
  # Number of parameters
  p <- dim1 * dim2
  
  # Match the correlation argument
  corr <- match.arg(corr)
  
  # Set the PSI value range based on the correlation type
  psi_range <- switch(corr,
                      "weak" = c(2.5, 7.5),
                      "moderate" = c(0.5, 1.5),
                      "strong" = c(0.05, 0.15))
  
  # Set dimensions and initialize the B_factors array
  B_factors <- array(0, dim = c(dim1, dim2, q))
  
  # Define overlapping squares based on overlap
  if (overlap == "small") {
    B_factors[1:4, 1:4, 1] <- 1        # Top left
    B_factors[3:6, 3:6, 2] <- 1        # Overlapping with first square
    B_factors[1:4, 7:10, 3] <- 1       # Top right
  } else if (overlap == "big") {
    B_factors[1:10, 1:4, 1] <- 1       # Top left
    B_factors[6:10, 1:10, 2] <- 1      # Overlapping with first square
    B_factors[1:4, 7:10, 3] <- 1       # Top right
  } else {
    stop("Invalid overlap size. Choose either 'small' or 'big'.")
  }
  
  if (print_factors == TRUE) {
    print(B_factors[, , 1])
    print(B_factors[, , 2])
    print(B_factors[, , 3])
  }
  
  # Convert slices to column vectors and combine into a matrix
  B_true <- do.call(cbind, lapply(1:q, function(i) 
    as.vector(B_factors[, , i])))
  
  # Set a specific seed for generating PSI_true
  old_seed <- .Random.seed  # Save the current random seed
  set.seed(12)  # Set a seed specifically for generating PSI_true
  PSI_true <- diag(runif(p, min = psi_range[1], max = psi_range[2]))
  .Random.seed <<- old_seed  # Restore the previous random seed
  
  # Generate noise covariance matrix and synthetic data
  Covariance_matrix_true <- B_true %*% t(B_true) + PSI_true
  Y <- rmvnorm(n, mean = rep(0, p), sigma = Covariance_matrix_true)
  
  cor_matrix <- cov2cor(Covariance_matrix_true)
  
  return(list(Y = Y, B_true = B_true, PSI_true = PSI_true, 
              B_factors = B_factors, Covariance_matrix_true = Covariance_matrix_true, 
              cor_matrix = cor_matrix))
}


# Function to generate B, PSI, and Y for 2D data with slightly overlapping squares
generate_data_3overlap <- function(n, dim1, dim2, q, overlap = "small", 
                                   corr = c("weak", "moderate", "strong"), 
                                   print_factors = FALSE) {
  # Number of parameters
  p <- dim1 * dim2
  
  # Match the correlation argument
  corr <- match.arg(corr)
  
  # Set the PSI value range based on the correlation type
  psi_range <- switch(corr,
                      "weak" = c(2.5, 7.5),
                      "moderate" = c(0.5, 1.5),
                      "strong" = c(0.05, 0.15))
  
  # Set dimensions and initialize the B_factors array
  B_factors <- array(0, dim = c(dim1, dim2, q))
  
  # Define overlapping squares based on overlap
  if (overlap == "small") {
    B_factors[1:5, 1:5, 1] <- 1        # Top left
    B_factors[4:7, 4:7, 2] <- 1        # Overlapping with first square
    B_factors[6:10, 6:10, 3] <- 1      # Top right
  } else if (overlap == "big") {
    B_factors[1:5, 1:5, 1] <- 1        # Top left
    B_factors[3:9, 3:5, 2] <- 1        # Overlapping with first square
    B_factors[1:3, 3:9, 3] <- 1        # Top right
  } else {
    stop("Invalid overlap size. Choose either 'small' or 'big'.")
  }
  
  if (print_factors == TRUE) {
    print(B_factors[, , 1])
    print(B_factors[, , 2])
    print(B_factors[, , 3])
  }
  
  # Convert slices to column vectors and combine into a matrix
  B_true <- do.call(cbind, lapply(1:q, function(i) 
    as.vector(B_factors[, , i])))
  
  # Set a specific seed for generating PSI_true
  old_seed <- .Random.seed  # Save the current random seed
  set.seed(12)  # Set a seed specifically for generating PSI_true
  PSI_true <- diag(runif(p, min = psi_range[1], max = psi_range[2]))
  .Random.seed <<- old_seed  # Restore the previous random seed
  
  # Generate noise covariance matrix and synthetic data
  Covariance_matrix_true <- B_true %*% t(B_true) + PSI_true
  Y <- rmvnorm(n, mean = rep(0, p), sigma = Covariance_matrix_true)
  
  cor_matrix <- cov2cor(Covariance_matrix_true)
  
  return(list(Y = Y, B_true = B_true, PSI_true = PSI_true, 
              B_factors = B_factors, Covariance_matrix_true = Covariance_matrix_true, 
              cor_matrix = cor_matrix))
}


