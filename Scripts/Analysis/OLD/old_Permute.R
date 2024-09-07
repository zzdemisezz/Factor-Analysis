library(clue)
# Assuming B_true and GAMMA_truncated are both q x p matrices (same dimensions)
cost_matrix <- as.matrix(dist(rbind(t(data$B_true), t(best_result$GAMMA_truncated))))[1:q, (q+1):(2*q)]

# Solve the assignment problem to find the best permutation
perm <- solve_LSAP(cost_matrix)

# Permute the columns of B_est according to the optimal permutation
B_est_permuted <- best_result$B_truncated[, perm]

# Adjust signs
for (i in 1:ncol(B_est_permuted)) {
  if (cor(data$B_true[, i], B_est_permuted[, i]) < 0) {
    B_est_permuted[, i] <- -B_est_permuted[, i]
  }
}

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

# Example usage:
# Assuming data$B_true, best_result2$GAMMA_truncated, and best_result2$B_truncated are available
B_est_permuted <- permute_B(true_data$B_true, 
                            LargeFactors_5x5_Moderate_dataframe$GAMMA, 
                            LargeFactors_5x5_Moderate_dataframe$B)



View(B_est_permuted)
View(data$B_true)

# Now compare the aligned and sign-adjusted B_est_permuted with B_true
rmse <- sqrt(mean((data$B_true - B_est_permuted)^2))
cat("RMSE after permutation and sign adjustment:", rmse, "\n")

#GAMMA ####
cost_matrix <- as.matrix(dist(rbind(t(data$B_true), t(best_result$GAMMA_truncated))))[1:q, (q+1):(2*q)]

# Solve the assignment problem to find the best permutation
perm <- solve_LSAP(cost_matrix)

# Permute the columns of B_est according to the optimal permutation
GAMMA_est_permuted <- best_result$GAMMA_truncated[, perm]
View(GAMMA_est_permuted)
identical(GAMMA_est_permuted, data$B_true)
