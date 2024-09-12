rm(list = ls())
#results_old <- readRDS("results_old.rds")
#model <- results_old$`overlap2-large-weak`
results_old <- readRDS("new_results.rds")


model <- 

# Get the size of the covariance matrix (assuming square)
p <- nrow(model$Covariance_matrix[[1]])

# Number of simulations
n_sim <- 101

# Initialize matrices to store p-values and t-statistics for all elements
p_values <- matrix(NA, nrow = p, ncol = p)
t_statistics <- matrix(NA, nrow = p, ncol = p)

# Assume you have the true covariance matrix
true_covariance <- model$Covariance_matrix_true[[1]]  # Modify this line with the actual true matrix

# Initialize matrices to store bias for both models
bias_model1 <- matrix(NA, nrow = p, ncol = p)
bias_model2 <- matrix(NA, nrow = p, ncol = p)

# Track number of non-zero elements, statistically significant elements, and bias comparison counts
non_zero_count <- 0
statistically_significant_count <- 0
lower_bias_model1_count <- 0  # Count of parameters where Model 1 has lower bias
lower_bias_model2_count <- 0  # Count of parameters where Model 2 has lower bias

# Loop through each element (i, j) of the covariance matrix
for (i in 1:p) {
  for (j in 1:p) {
    
    # Extract 100 estimates for element (i, j) from both models
    model1_estimates <- sapply(1:n_sim, function(k) model$Covariance_matrix[[k]][i, j])
    model2_estimates <- sapply(1:n_sim, function(k) model$Covariance_matrix_beta[[k]][i, j])
    
    # Check if both estimates are all zeros
    if (!(all(model1_estimates == 0) && all(model2_estimates == 0))) {
      # Count non-zero elements
      non_zero_count <- non_zero_count + 1
      
      # Perform paired t-test using model1 and model2 estimates directly
      t_test_result <- t.test(model1_estimates, model2_estimates, paired = TRUE)
      
      # Store the p-value and t-statistic in the matrices
      p_values[i, j] <- t_test_result$p.value
      t_statistics[i, j] <- t_test_result$statistic
      
      # Check if the p-value is statistically significant (p < 0.05)
      if (p_values[i, j] < 0.05) {
        statistically_significant_count <- statistically_significant_count + 1
        
        # Calculate bias for both models
        bias1 <- mean(model1_estimates) - true_covariance[i, j]
        bias2 <- mean(model2_estimates) - true_covariance[i, j]
        
        # Store the biases in the matrices
        bias_model1[i, j] <- bias1
        bias_model2[i, j] <- bias2
        
        # Compare biases and count which model has lower bias
        if (abs(bias1) < abs(bias2)) {
          lower_bias_model1_count <- lower_bias_model1_count + 1
        } else if (abs(bias2) < abs(bias1)) {
          lower_bias_model2_count <- lower_bias_model2_count + 1
        }
      }
    }
  }
}

# Print results
num_elements_greater_than_zero <- sum(true_covariance > 0)
print(paste("Number of elements in the true covariance matrix greater than zero:", num_elements_greater_than_zero))
print(paste("Number of non-zero elements:", non_zero_count))
print(paste("Proportion of significantly different non-zero elements:", statistically_significant_count / non_zero_count))
print(paste("Total number of significantly different elements:", statistically_significant_count))
print(paste("Number of significantly different parameters where Model 1 has lower bias:", lower_bias_model1_count))
print(paste("Number of significantly different parameters where Model 2 has lower bias:", lower_bias_model2_count))

# Heatmaps ####
library(ggplot2)
library(reshape2)

# Convert matrices to data frames for plotting
p_values_df <- melt(p_values)
t_statistics_df <- melt(t_statistics)
bias_model1_df <- melt(bias_model1)
bias_model2_df <- melt(bias_model2)

# Plot heatmap of p-values
ggplot(p_values_df, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_y_reverse() +
  scale_fill_gradient(low = "white", high = "red", name = "P-value") +
  labs(title = "Heatmap of P-values", x = "Row index", y = "Column index") +
  theme_minimal()

# Plot heatmap of t-statistics
ggplot(t_statistics_df, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_y_reverse() +
  scale_fill_gradient(low = "white", high = "blue", name = "T-statistic") +
  labs(title = "Heatmap of T-statistics", x = "Row index", y = "Column index") +
  theme_minimal()

# Plot heatmap of bias for Model 1
ggplot(bias_model1_df, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_y_reverse() +
  scale_fill_gradient(low = "white", high = "blue", name = "Bias Model 1") +
  labs(title = "Heatmap of Bias for Model 1", x = "Row index", y = "Column index") +
  theme_minimal()

# Plot heatmap of bias for Model 2
ggplot(bias_model2_df, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_y_reverse() +
  scale_fill_gradient(low = "white", high = "blue", name = "Bias Model 2") +
  labs(title = "Heatmap of Bias for Model 2", x = "Row index", y = "Column index") +
  theme_minimal()

# Barplot ####
# Create a data frame for the comparison
bias_comparison_df <- data.frame(
  Model = c("Model 1", "Model 2"),
  Count = c(lower_bias_model1_count, lower_bias_model2_count)
)

# Plot the bar chart
ggplot(bias_comparison_df, aes(x = Model, y = Count, fill = Model)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Significantly Different Elements with Lower Bias", x = "Model", y = "Count") +
  theme_minimal()

# Scatterplot ####
# Create a scatter plot of non-zero elements and significance

# Get the indices of non-zero elements in the true covariance matrix
non_zero_elements <- which(true_covariance > 0, arr.ind = TRUE)

# Create a data frame from the non-zero elements and their corresponding p-values
scatter_data <- data.frame(
  row = non_zero_elements[, 1], 
  col = non_zero_elements[, 2], 
  p_value = p_values[non_zero_elements]
)

# Generate the scatter plot
ggplot(scatter_data, aes(x = row, y = col, size = -log10(p_value))) +
  geom_point(aes(color = p_value < 0.05)) +
  scale_color_manual(values = c("gray", "red"), name = "Significant") +
  labs(title = "Scatter Plot of Non-Zero Elements and Significance", x = "Row index", y = "Column index") +
  theme_minimal()


model1_estimates <- sapply(1:n_sim, function(k) model$Covariance_matrix[[k]][1, 1])
model2_estimates <- sapply(1:n_sim, function(k) model$Covariance_matrix_beta[[k]][1, 1])


# Assuming you have 100 estimates
n_simulations <- 101

# Create an empty vector to store the (1,1) element for all simulations
cov_11 <- numeric(n_simulations)

# Loop over all k to extract the (1,1) element from each covariance matrix
for (k in 1:n_simulations) {
  cov_11[k] <- model$Covariance_matrix[[k]][1, 17]
}

# Generate a histogram for the (1,1) element across all simulations
hist(cov_11, main="Histogram of (1,1) element of Covariance Matrix",
     xlab="(1,1) element", col="lightblue", border="black")


