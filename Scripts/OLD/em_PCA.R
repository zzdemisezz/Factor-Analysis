# EM algorithm 
library("elasticnet")
source("Scripts/ll.R")
source("Scripts/MCAR_Functions.R")

em <- function(Y, q, dim1 = 10, dim2 = 10, dim3 = NULL, tol = 1e-3, 
               max_iter = 50, ll = TRUE){
  
  n <- nrow(Y)
  p <- ncol(Y)
  
  # initialise hyper parameters
  v0 <- 0.0025
  v1 <- 10
  reg <- 1e-6
  
  # Initialise parameters
  # PCA initialation for loadings
  data_scaled <- scale(Y)
  pca_result <- prcomp(data_scaled, scale. = TRUE)
  B <- pca_result$rotation[, 1:q]
  
  PSI <- diag(p)
  THETA <- matrix(0.5, p, q)
  SIGMA_inverse <- matrix(rnorm(q * q), q, q)
  
  # MCAR functions
  if (is.null(dim3)) {
    # 2D case
    Adj <- adjacency_matrix(dim1, dim2)
    n_sj <- n_neighbors(dim1, dim2)
    triangle <- upper_triangular(Adj, dim1 * dim2)
  } else {
    # 3D case
    Adj <- adjacency_matrix(dim1, dim2, dim3)
    n_sj <- n_neighbors(dim1, dim2, dim3)
    triangle <- upper_triangular(Adj, dim1 * dim2 * dim3)
  }
  list_ind = list()
  for(j in 1:p){
    list_ind[[j]] = c(triangle$y[which(triangle$x == j, arr.ind = T)], 
                      triangle$x[which(triangle$y == j, arr.ind = T)])
  }
  
  ll_old <- 0
  iter <- 0
  convergence <- FALSE
  while (iter < max_iter && !convergence) {
    iter <- iter + 1
    
    # E-step
    # Update Z
    PSI_inverse <- solve(PSI + reg * diag(nrow(PSI)))
    var_zi <- solve(t(B) %*% PSI_inverse %*% B + diag(q) + reg * diag(q))
    E_zt <- var_zi %*% t(B) %*% PSI_inverse %*% t(Y)
    
    # Calculate sum_E_zzt without looping
    sum_E_zzt <- var_zi * n + E_zt %*% t(E_zt)
    
    # Update GAMMA and OMEGA
    sqrt_v1 <- sqrt(v1)
    sqrt_v0 <- sqrt(v0)
    
    sigmoid_THETA <- sigmoid(THETA)
    dnorm_B_v1 <- dnorm(B, mean = 0, sd = sqrt_v1, log = FALSE)
    dnorm_B_v0 <- dnorm(B, mean = 0, sd = sqrt_v0, log = FALSE)
    
    term1 <- dnorm_B_v1 * sigmoid_THETA
    term2 <- dnorm_B_v0 * (1 - sigmoid_THETA)
    
    GAMMA <- term1 / (term1 + term2)
    OMEGA <- 0.5 * THETA * tanh(THETA * 0.5)
    
    # M-step
    # Update B
    # Precompute the diagonal of PSI
    PSI_diag <- diag(PSI)
    
    # Compute values for D
    values <- ((1 - GAMMA) / v0)  + (GAMMA / v1)  # pxq matrix
    
    # Update B without the explicit loop
    sum_yE_z <- E_zt %*% Y  # qxp matrix
    
    # Efficiently update B by solving the linear systems in a vectorized manner
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
    for (j in 1:p) {
      diag_omega <- diag(OMEGA[j, ])  # qxq
      term1 <- solve(diag_omega + (n_sj[j] * SIGMA_inverse) + reg * diag(q))  # qxq
      
      neighbor_sum <- colSums(THETA[list_ind[[j]], ])
      
      THETA[j, ] <- t(term1 %*% ((GAMMA[j, ] - 1/2) + SIGMA_inverse %*% neighbor_sum))
    }
    
    # Update SIGMA_inverse
    difference_neighbours <- THETA[triangle$x, ] - THETA[triangle$y, ]
    sum_matrix <- t(difference_neighbours) %*% difference_neighbours
    SIGMA_inverse <- solve((diag(q) + sum_matrix + reg * diag(q)) / (p - 1))
    
    # Check convergence
    ll_new <- ll(Y, B, PSI, GAMMA, v0, v1)
    
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
  return(list(B = B, B_truncated = B_truncated, PSI = PSI, GAMMA = GAMMA, 
              GAMMA_truncated = GAMMA_truncated, Z = t(E_zt), THETA = THETA, 
              iter = iter, converged = convergence, 
              likelihood = ll_old))
}