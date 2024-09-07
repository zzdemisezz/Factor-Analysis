# EM algorithm 
source("Scripts/ll.R")
source("Scripts/MCAR_Functions.R")
library("Matrix")

em_matrix <- function(Y, q, dim1 = 10, dim2 = 10, dim3 = NULL, tol = 1e-3, 
               max_iter = 50, ll = TRUE){
  
  n <- nrow(Y)
  p <- ncol(Y)
  
  # initialise hyperparameters
  v0 <- 0.0025
  v1 <- 10
  reg <- 1e-6
  
  # Initialise parameters using Matrix package
  B <- Matrix(rnorm(p * q), p, q)
  PSI <- Diagonal(x = rep(1, p))
  THETA <- Matrix(0.5, p, q)
  SIGMA_inverse <- Matrix(rnorm(q * q), q, q)
  
  # MCAR functions
  if (is.null(dim3)) {
    Adj <- adjacency_matrix(dim1, dim2)
    n_sj <- n_neighbors(dim1, dim2)
    triangle <- upper_triangular(Adj, dim1 * dim2)
  } else {
    Adj <- adjacency_matrix(dim1, dim2, dim3)
    n_sj <- n_neighbors(dim1, dim2, dim3)
    triangle <- upper_triangular(Adj, dim1 * dim2 * dim3)
  }
  
  list_ind <- vector("list", p)
  for(j in 1:p){
    list_ind[[j]] <- c(triangle$y[which(triangle$x == j, arr.ind = TRUE)], 
                       triangle$x[which(triangle$y == j, arr.ind = TRUE)])
  }
  
  ll_old <- 0
  iter <- 0
  convergence <- FALSE
  while (iter < max_iter && !convergence) {
    iter <- iter + 1
    
    # E-step
    PSI_inverse <- solve(PSI + reg * Diagonal(p))
    var_zi <- solve(t(B) %*% PSI_inverse %*% B + Diagonal(q) + reg * Diagonal(q))
    E_zt <- var_zi %*% t(B) %*% PSI_inverse %*% t(Y)
    
    # Calculate sum_E_zzt without looping
    sum_E_zzt <- var_zi * n + E_zt %*% t(E_zt)
    
    # Update GAMMA and OMEGA
    sqrt_v1 <- sqrt(v1)
    sqrt_v0 <- sqrt(v0)
    
    sigmoid_THETA <- sigmoid(as.matrix(THETA))
    dnorm_B_v1 <- dnorm(as.matrix(B), mean = 0, sd = sqrt_v1, log = FALSE)
    dnorm_B_v0 <- dnorm(as.matrix(B), mean = 0, sd = sqrt_v0, log = FALSE)
    
    term1 <- dnorm_B_v1 * sigmoid_THETA
    term2 <- dnorm_B_v0 * (1 - sigmoid_THETA)
    
    GAMMA <- term1 / (term1 + term2)
    OMEGA <- 0.5 * as.matrix(THETA) * tanh(as.matrix(THETA) * 0.5)
    
    # M-step
    # Update B
    PSI_diag <- diag(PSI)
    values <- ((1 - GAMMA) / v0)  + (GAMMA / v1)
    sum_yE_z <- E_zt %*% Y
    
    B <- t(apply(matrix(1:p), 1, function(j) {
      D_j <- Diagonal(x = values[j, ])
      solve(sum_E_zzt + PSI_diag[j] * D_j + reg * Diagonal(q)) %*% sum_yE_z[, j]
    }))
    
    # Update PSI
    term1 <- colSums(Y^2)
    E_zt_Y <- E_zt %*% Y
    term2 <- -2 * diag(B %*% E_zt_Y)
    B_sum_E_zzt_B <- B %*% sum_E_zzt %*% t(B)
    term3 <- diag(B_sum_E_zzt_B)
    
    PSI <- Diagonal(x = (term1 + term2 + term3 + 1) / (n + 3))
    
    # Update THETA
    for (j in 1:p) {
      diag_omega <- Diagonal(x = OMEGA[j, ])
      term1 <- solve(diag_omega + (n_sj[j] * SIGMA_inverse) + reg * Diagonal(q))
      neighbor_sum <- colSums(THETA[list_ind[[j]], ])
      THETA[j, ] <- t(term1 %*% ((GAMMA[j, ] - 1/2) + SIGMA_inverse %*% neighbor_sum))
    }
    
    # Update SIGMA_inverse
    difference_neighbours <- THETA[triangle$x, ] - THETA[triangle$y, ]
    sum_matrix <- t(difference_neighbours) %*% difference_neighbours
    SIGMA_inverse <- solve((Diagonal(q) + sum_matrix + reg * Diagonal(q)) / (p - 1))
    
    # Check convergence
    ll_new <- ll(Y, as.matrix(B), diag(PSI), GAMMA, v0, v1)
    
    if (iter > 1 && ll_new < ll_old) {
      warning("Log-likelihood decreased!")
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
  return(list(B = B, B_truncated = B_truncated, PSI = as.matrix(PSI), GAMMA = GAMMA, 
              GAMMA_truncated = GAMMA_truncated, Z = t(E_zt), THETA = THETA, 
              iter = iter, converged = convergence, 
              likelihood = ll_old))
}
