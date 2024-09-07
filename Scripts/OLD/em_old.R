# EM algorithm 
source("Scripts/ll.R")
source("Scripts/MCAR_Functions.R")

em_old <- function(Y, q, dim1 = 10, dim2 = 10, dim3 = NULL, tol = 1e-3, 
               max_iter = 50, ll = TRUE){
  
  n <- nrow(Y)
  p <- ncol(Y)
  
  # initialise hyper parameters
  v0 <- 0.0025
  v1 <- 10
  
  # Initialise parameters
  B <- matrix(rnorm(p * q), p, q)
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
    A <- t(B) %*% solve(B %*% t(B) + PSI)  
    var_zi <- diag(q) - A %*% B            
    E_zt <- A %*% t(Y)
    
    E_zzt <- array(0, c(q, q, n))  
    for (i in 1:n) {
      E_zzt[, , i] <- var_zi + tcrossprod(E_zt[, i])
    }
    sum_E_zzt <- apply(E_zzt, 1:2, sum)  
    
    # Update GAMMA and OMEGA
    sigmoid_THETA <- sigmoid(THETA)
    dnorm_B_v1 <- dnorm(B, mean = 0, sd = sqrt(v1), log = FALSE)
    dnorm_B_v0 <- dnorm(B, mean = 0, sd = sqrt(v0), log = FALSE)
    
    term1 <- dnorm_B_v1 * sigmoid_THETA
    term2 <- dnorm_B_v0 * (1 - sigmoid_THETA)
    
    GAMMA <- term1 / (term1 + term2)
    OMEGA <- 0.5 * THETA * tanh(THETA * 0.5)
    
    # M-step
    # Update B
    for (j in 1:p) {
      values <- ((1 - GAMMA) / v0)  + (GAMMA / v1) # pxq matrix
      D_j <- diag(values[j, ]) # qxq diagonal matrix for the j-th row
      
      sum_yE_z <- E_zt %*% Y[, j] # qx1 vector
      B[j, ] <- t(solve(sum_E_zzt + PSI[j, j] * D_j) %*% sum_yE_z) # qx1 vector
    }
    
    # Update PSI
    for (j in 1:p) {
      term1 <- t(Y[, j]) %*% Y[, j]
      term2 <- -2 * t(B[j, ]) %*% E_zt %*% Y[, j]
      term3 <- t(B[j, ]) %*% sum_E_zzt %*% B[j, ]
      PSI[j,j] <- (term1 + term2 + term3 + 1) / (n + 3)
    }
    
    # Update THETA
    for (j in 1:p) {
      diag_omega <- diag(OMEGA[j, ])  # qxq
      term1 <- solve(diag_omega + (n_sj[j] * SIGMA_inverse))  # qxq
      
      neighbor_sum <- colSums(THETA[list_ind[[j]], ])
      
      THETA[j, ] <- t(term1 %*% ((GAMMA[j, ] - 1/2) + SIGMA_inverse %*% neighbor_sum))
    }
    
    # Update SIGMA_inverse
    difference_neighbours <- THETA[triangle$x, ] - THETA[triangle$y, ]
    sum <- matrix(0, q, q)
    for (i in 1:nrow(difference_neighbours)) {
      element <- tcrossprod(difference_neighbours[i, ])
      sum <- sum + element
    }
    SIGMA_inverse <- solve((diag(q) + sum) / (p-1))
    
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