# Load the necessary library
library("ggplot2")
library("reshape2")

# Bias matrices
# Initialize lists to store the bias matrices for each simulation
bias_list_em <- vector("list", num_simulations)
bias_list_em_beta <- vector("list", num_simulations)

# Loop through each simulation to calculate the bias for each element
for (sim in 1:num_simulations) {
  # Get the best B matrix from the EM results of the current simulation
  best_B_em <- all_results_em[[sim]]$best_result$B_truncated
  # Calculate the bias for each element in B for the em algorithm
  bias_em <- best_B_em - data$B_true
  # Store the bias matrix
  bias_list_em[[sim]] <- bias_em
  
  # Get the best B matrix from the EM Beta results of the current simulation
  best_B_beta <- all_results_em_beta[[sim]]$best_result$B_truncated
  # Calculate the bias for each element in B for the em_beta algorithm
  bias_beta <- best_B_beta - data$B_true
  # Store the bias matrix
  bias_list_em_beta[[sim]] <- bias_beta
}

# Calculate the average bias across all simulations for each element
average_bias_matrix_em <- Reduce("+", bias_list_em) / num_simulations
average_bias_matrix_em_beta <- Reduce("+", bias_list_em_beta) / num_simulations

# Print the average bias matrices for both em and em_beta
cat("Average Bias Matrix for em:\n")
print(average_bias_matrix_em)

cat("\nAverage Bias Matrix for em_beta:\n")
print(average_bias_matrix_em_beta)

# Convert the matrix to a data frame for ggplot
bias_df_em <- melt(average_bias_matrix_em)

# Create the heatmap for the em algorithm
ggplot(bias_df_em, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(min(bias_df_em$value), max(bias_df_em$value)),
                       name="Average Bias") +
  scale_y_reverse() +  # This flips the y-axis to correct the orientation
  theme_minimal() +
  labs(x = "Columns of B", y = "Rows of B",
       title = "Heatmap of Average Bias for EM Algorithm") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Repeat the same for em_beta
bias_df_em_beta <- melt(average_bias_matrix_em_beta)

ggplot(bias_df_em_beta, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(min(bias_df_em_beta$value), max(bias_df_em_beta$value)),
                       name="Average Bias") +
  scale_y_reverse() +  # This flips the y-axis to correct the orientation
  theme_minimal() +
  labs(x = "Columns of B", y = "Rows of B",
       title = "Heatmap of Average Bias for EM_Beta Algorithm") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


