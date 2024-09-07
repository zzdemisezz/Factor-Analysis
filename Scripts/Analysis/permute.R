library(clue)
permute_B <- function(B_true, GAMMA_truncated, B_truncated) {
  
  q <- ncol(B_true)  # Assuming B_true and GAMMA_truncated are both q x p matrices
  
  # Compute the cost matrix for the assignment problem
  cost_matrix <- as.matrix(dist(rbind(t(B_true), t(GAMMA_truncated))))[1:q, (q+1):(2*q)]
  
  # Solve the assignment problem to find the best permutation
  perm <- solve_LSAP(cost_matrix)
  
  # Permute the columns of B_truncated according to the optimal permutation
  B_est_permuted <- B_truncated[, perm]
  
  # Adjust signs to match B_true
  for (i in 1:ncol(B_est_permuted)) {
    if (cor(B_true[, i], B_est_permuted[, i]) < 0) {
      B_est_permuted[, i] <- -B_est_permuted[, i]
    }
  }
  
  return(B_est_permuted)
}

# # Example usage:
# # Assuming data$B_true, best_result2$GAMMA_truncated, and best_result2$B_truncated are available
# B_est_permuted <- permute_B(true_data$B_true,
#                             LargeFactors_5x5_Moderate_dataframe$GAMMA[[1]],
#                             LargeFactors_5x5_Moderate_dataframe$B[[1]])
# 
