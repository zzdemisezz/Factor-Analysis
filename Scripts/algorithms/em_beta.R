# EM algorithm for a beta prior on theta
source("Scripts/likelihood.R")

em_beta <- function(Y, q, tol = 1e-3, max_iter = 50, simplified = TRUE, 
                    ll = TRUE, PCA = FALSE){
  n <- nrow(Y)
  p <- ncol(Y)
  
  # initialise hyper parameters
  v0 <- 0.0025
  v1 <- 10
  reg <- 1e-6
  alpha <- 0.5
  beta <- 0.5
  
  # Initialise parameters
  if (PCA) {
    # PCA initialisation for loadings
    data_scaled <- scale(Y)
    pca_result <- prcomp(data_scaled, scale. = TRUE)
    B <- pca_result$rotation[, 1:q]
  } else {
    # Random initialization
    B <- matrix(rnorm(p * q), p, q)
  }
  
  PSI <- diag(p)
  THETA <- rep(0.5, q)
  
  # Log-likelihood to check convergence
  ll_old <- 0
  iter <- 0
  convergence <- FALSE
  while (iter <= max_iter && !convergence) {
    iter <- iter + 1
    
    # E-step
    # Update Z
    PSI_inverse <- solve(PSI + reg * diag(nrow(PSI)))
    var_zi <- solve(t(B) %*% PSI_inverse %*% B + diag(q) + reg * diag(q))
    E_zt <- var_zi %*% t(B) %*% PSI_inverse %*% t(Y)
    
    # Calculate sum_E_zzt without looping
    sum_E_zzt <- var_zi * n + E_zt %*% t(E_zt)
    
    # Update GAMMA
    sqrt_v1 <- sqrt(v1)
    sqrt_v0 <- sqrt(v0)
    dnorm_B_v1 <- dnorm(B, mean = 0, sd = sqrt_v1, log = FALSE)
    dnorm_B_v0 <- dnorm(B, mean = 0, sd = sqrt_v0, log = FALSE)
    
    term1 <- dnorm_B_v1 * THETA
    term2 <- dnorm_B_v0 * (1 - THETA)
    GAMMA <- term1 / (term1 + term2)
    
    # M-step
    # Update B
    PSI_diag <- diag(PSI)
    values <- ((1 - GAMMA) / v0)  + (GAMMA / v1)  # pxq matrix
    sum_yE_z <- E_zt %*% Y  # qxp matrix
    B <- t(apply(matrix(1:p), 1, function(j) {
      D_j <- diag(values[j, ])  # qxq matrix for the j-th row
      solve(sum_E_zzt + PSI_diag[j] * D_j + reg * diag(q)) %*% sum_yE_z[, j]
    }))
    
    # Update PSI
    term1 <- colSums(Y^2)
    
    E_zt_Y <- E_zt %*% Y  # q x p matrix
    term2 <- -2 * rowSums(B * t(E_zt_Y))
    
    B_sum_E_zzt_B <- B %*% sum_E_zzt %*% t(B)
    term3 <- diag(B_sum_E_zzt_B)
    
    PSI <- diag((term1 + term2 + term3 + 1) / (n + 3))
    
    # Update THETA
    sum_gammas <- colSums(GAMMA)  # Vectorized sum for each column
    THETA <- (sum_gammas + alpha - 1) / (p + alpha + beta - 2)
    
    # Check convergence
    ll_new <- likelihood(Y, B, PSI, GAMMA, v0, v1)
    
    if (iter > 1 && ll_new < ll_old) {
      warning(paste("Log-likelihood decreased at iteration", iter, "!"))
    }
    if (abs(ll_new - ll_old) < tol) {
      convergence <- TRUE
    }
    
    ll_old <- ll_new
    
    if(ll){
      cat("Iteration:", iter, "Log-Likelihood:", ll_old, "\n")
    }
  }
  B_truncated <- B
  B_truncated[GAMMA < 0.5] <- 0
  
  GAMMA_truncated <- GAMMA
  GAMMA_truncated[GAMMA < 0.5] <- 0
  
  Covariance_matrix <- B %*% t(B) + PSI
  Covariance_matrix_truncated <- B_truncated %*% t(B_truncated) + PSI
  
  return(list(B = B, B_truncated = B_truncated, PSI = PSI, GAMMA = GAMMA, 
              GAMMA_truncated = GAMMA_truncated, 
              Covariance_matrix = Covariance_matrix, 
              Covariance_matrix_truncated = Covariance_matrix_truncated,
              Z = t(E_zt), THETA = THETA, iter = iter, converged = convergence, 
              likelihood = ll_old))
}
