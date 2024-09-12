rm(list = ls())
results_old <- readRDS("results_old.rds")

model <- results_old$`3x3-strong`

# Get the size of the covariance matrix (assuming square)
p <- nrow(model$Covariance_matrix[[1]])

# Number of simulations
n_sim <- 103

# Initialize matrices to store p-values and t-statistics for all elements
p_values <- matrix(NA, nrow = p, ncol = p)
t_statistics <- matrix(NA, nrow = p, ncol = p)

# Loop through each element (i, j) of the covariance matrix
for (i in 1:p) {
  for (j in 1:p) {
    
    # Extract 100 estimates for element (i, j) from both models
    model1_estimates <- sapply(1:n_sim, function(k) model$Covariance_matrix[[k]][i, j])
    model2_estimates <- sapply(1:n_sim, function(k) model$Covariance_matrix_beta[[k]][i, j])
    
    # Check if both estimates are all zeros
    if (all(model1_estimates == 0) && all(model2_estimates == 0)) {
      p_values[i, j] <- 1  # Assign p-value = 1, indicating no difference
      t_statistics[i, j] <- 0  # t-statistic is 0
    } else {
      # Perform paired t-test using model1 and model2 estimates directly
      t_test_result <- t.test(model1_estimates, model2_estimates, paired = TRUE)
      
      # Store the p-value and t-statistic in the matrices
      p_values[i, j] <- t_test_result$p.value
      t_statistics[i, j] <- t_test_result$statistic
    }
  }
}

# Print the p-values and t-statistics matrices
print("P-values matrix:")
print(p_values)

print("T-statistics matrix:")
print(t_statistics)

# # Optionally, save the results for further analysis
# saveRDS(list(p_values = p_values, t_statistics = t_statistics), "t_test_results.rds")


# rm(list = ls())
# results_old <- readRDS("results_old.rds")
# 
# model <- results_old$`3x3-strong`
# 
# # Specify the element (i, j) of the covariance matrix that you want to test
# i <- 1  # Row index of the element
# j <- 4  # Column index of the element
# 
# # Number of simulations
# n_sim <- 103
# 
# # Extract 100 estimates for element (i, j) from both models
# model1_estimates <- sapply(1:n_sim, function(k) model$Covariance_matrix[[k]][i, j])
# model2_estimates <- sapply(1:n_sim, function(k) model$Covariance_matrix_beta[[k]][i, j])
# 
# # Calculate the differences between the two models
# differences <- model1_estimates - model2_estimates
# 
# # Perform paired t-test using model1 and model2 estimates directly
# t_test_result <- t.test(differences, mu = 0)
# 
# # Print the results of the paired t-test
# print(t_test_result)
