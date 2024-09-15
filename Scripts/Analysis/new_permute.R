results_old <- readRDS("results_old.rds")

test <- abs(results_old$`3x3-strong`$B[[1]])

library(clue)

cosine_similarity <- function(x, y) {
  return(sum(x * y) / (sqrt(sum(x^2)) * sqrt(sum(y^2))))
}

permute_B <- function(B_true, GAMMA_truncated, B_truncated) {
  
  q <- ncol(B_true)  # Assuming B_true and GAMMA_truncated are both q x p matrices
  
  # Initialize the cost matrix
  cost_matrix <- matrix(NA, nrow = q, ncol = q)
  
  # Compute the negative cosine similarity for the assignment problem
  for (i in 1:q) {
    for (j in 1:q) {
      cost_matrix[i, j] <- -cosine_similarity(B_true[, i], GAMMA_truncated[, j])
    }
  }
  
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