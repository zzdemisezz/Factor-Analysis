rm(list = ls())
# library("viridisLite")
# library("viridis")
# library("RColorBrewer")
# library("pheatmap")
library("patchwork")
source("Scripts/Analysis/Final_Functions.R")

results <- readRDS("Results/dataframes/results_5")
structure <- results$`hard-moderate`

matrices <- calculate_statistics(structure, type = "em_pxl")
matrices_beta <- calculate_statistics(structure, type = "em_beta_pxl")

# Covariances ####
# Common scale for covariance matrices
common_scale_top <- range(c(matrices$Covariance_true, matrices$average_covariance, matrices_beta$average_covariance))
# Common scale for MSE matrices
common_scale_bottom <- range(c(matrices$MSE_covariance, matrices_beta$MSE_covariance))

# Top row color scale 
color_scale_top <- scale_fill_gradient2(low = "white", high = "black", mid = "blue", midpoint = mean(common_scale_top))
# Bottom row color scale 
color_scale_bottom <- scale_fill_gradient2(low = "white", high = "red", mid = "pink", midpoint = mean(common_scale_bottom))

# Top row (True, Estimated, Beta Estimated) - use the top scale, no legend
p1 <- create_custom_heatmap(matrices$Covariance_true, show_legend = FALSE, show_grid = FALSE, common_scale = common_scale_top, title = expression("True" ~ bold(BB^T + Psi))) + color_scale_top
p2 <- create_custom_heatmap(matrices$average_covariance, show_legend = FALSE, show_grid = FALSE, common_scale = common_scale_top, title = expression("Spatial Average Estimated" ~ bold(BB^T + Psi))) + color_scale_top
p3 <- create_custom_heatmap(matrices_beta$average_covariance, show_legend = TRUE, show_grid = FALSE, common_scale = common_scale_top, title = expression("Beta Average Estimated" ~ bold(BB^T + Psi))) + color_scale_top
# Bottom row (MSE, Beta MSE) - use the bottom scale, one legend on the right
p4 <- create_custom_heatmap(matrices$MSE_covariance, show_legend = FALSE, show_grid = FALSE, common_scale = common_scale_bottom, title = expression("Spatial MSEs" ~ bold(BB^T + Psi))) + color_scale_bottom
p5 <- create_custom_heatmap(matrices_beta$MSE_covariance, show_legend = TRUE, show_grid = FALSE, common_scale = common_scale_bottom, title = expression("Beta MSEs" ~ bold(BB^T + Psi))) + color_scale_bottom

# Combine the heatmaps using patchwork
final_plot <- (p1 | p2 | p3) / (p4 | p5)
# Display the final plot
final_plot
stop()
ggsave("~/Downloads/covariances-overlap2-large-strong.png", width=16, height = 8)

# Factors ####
# Split loadings into submatrices
true_factors <- split_loadings_to_matrices(matrices$B_true)
est_factors <- split_loadings_to_matrices(matrices$average_matrix)
est_factors_beta <- split_loadings_to_matrices(matrices_beta$average_matrix)
bias_factors <- split_loadings_to_matrices(matrices$bias_matrix)
bias_factors_beta <- split_loadings_to_matrices(matrices_beta$bias_matrix)
mse_factors <- split_loadings_to_matrices(matrices$MSE_matrix)
mse_factors_beta <- split_loadings_to_matrices(matrices_beta$MSE_matrix)

# Find common scales for each block
common_scale_left <- range(c(true_factors, est_factors, est_factors_beta))
common_scale_bias <- range(c(bias_factors, bias_factors_beta))
common_scale_mse <- range(c(mse_factors, mse_factors_beta))

# Color scale for True and Estimated - 0 is white
color_scale_true_est <- scale_fill_gradient2(low = "blue", mid = "white", high = "darkblue", midpoint = 0, limits = common_scale_left)
# Left side (True, Estimated, Estimated Beta) - sharing the same color scale
p_true <- lapply(1:3, function(i) create_custom_heatmap(true_factors[[i]], show_legend = FALSE, show_grid = TRUE, common_scale = common_scale_left, title = paste("True Factor", i)) + color_scale_true_est)

# Color scale for Bias - 0 is white
color_scale_bias <- scale_fill_gradient2(low = "red", mid = "white", high = "red", midpoint = 0, limits = common_scale_bias)
# Color scale for MSE - 0 is white
color_scale_mse <- scale_fill_gradient2(low = "lightgreen", mid = "white", high = "darkgreen", midpoint = 0, limits = common_scale_mse)

# p_est <- lapply(1:3, function(i) create_custom_heatmap(est_factors[[i]], show_legend = FALSE, show_grid = TRUE, common_scale = common_scale_left, title = paste("Spatial Estimate Factor", i)) + color_scale_true_est)
# p_est_beta <- lapply(1:3, function(i) create_custom_heatmap(est_factors_beta[[i]], show_legend = (i == 3), show_grid = TRUE, common_scale = common_scale_left, title = paste("Beta Estimate Factor", i)) + color_scale_true_est)
# 
# # Right side (Bias, Bias Beta, MSE, MSE Beta) with legends
# p_bias <- lapply(1:3, function(i) create_custom_heatmap(bias_factors[[i]], show_legend = FALSE, show_grid = TRUE, common_scale = common_scale_bias, title = paste("Spatial Biases Factor", i)) + color_scale_bias)
# p_bias_beta <- lapply(1:3, function(i) create_custom_heatmap(bias_factors_beta[[i]], show_legend = (i == 3), show_grid = TRUE, common_scale = common_scale_bias, title = paste("Beta Biases Factor", i)) + color_scale_bias)
# 
# p_mse <- lapply(1:3, function(i) create_custom_heatmap(mse_factors[[i]], show_legend = FALSE, show_grid = TRUE, common_scale = common_scale_mse, title = paste("Spatial MSEs Factor", i)) + color_scale_mse)
# p_mse_beta <- lapply(1:3, function(i) create_custom_heatmap(mse_factors_beta[[i]], show_legend = (i == 3), show_grid = TRUE, common_scale = common_scale_mse, title = paste("Beta MSEs Factor", i)) + color_scale_mse)

# True Factors
p_true <- lapply(1:3, function(i) {
  if (i == 2) {
    create_custom_heatmap(true_factors[[i]], show_legend = FALSE, show_grid = TRUE, common_scale = common_scale_left, title = "True Loading Factors") + color_scale_true_est
  } else {
    create_custom_heatmap(true_factors[[i]], show_legend = FALSE, show_grid = TRUE, common_scale = common_scale_left) + color_scale_true_est
  }
})
# Spatial Estimated Factors
p_est <- lapply(1:3, function(i) {
  if (i == 2) {
    create_custom_heatmap(est_factors[[i]], show_legend = FALSE, show_grid = TRUE, common_scale = common_scale_left, title = " Spatial Average Estimates") + color_scale_true_est
  } else {
    create_custom_heatmap(est_factors[[i]], show_legend = FALSE, show_grid = TRUE, common_scale = common_scale_left) + color_scale_true_est
  }
})
# Beta Estimated Factors
p_est_beta <- lapply(1:3, function(i) {
  if (i == 2) {
    create_custom_heatmap(est_factors_beta[[i]], show_legend = (i == 3), show_grid = TRUE, common_scale = common_scale_left, title = " Beta Average Estimates") + color_scale_true_est
  } else {
    create_custom_heatmap(est_factors_beta[[i]], show_legend = (i == 3), show_grid = TRUE, common_scale = common_scale_left) + color_scale_true_est
  }
})
# Spatial Biases Factors
p_bias <- lapply(1:3, function(i) {
  if (i == 2) {
    create_custom_heatmap(bias_factors[[i]], show_legend = FALSE, show_grid = TRUE, common_scale = common_scale_bias, title = " Spatial Average Biases") + color_scale_bias
  } else {
    create_custom_heatmap(bias_factors[[i]], show_legend = FALSE, show_grid = TRUE, common_scale = common_scale_bias) + color_scale_bias
  }
})
# Beta Biases Factors
p_bias_beta <- lapply(1:3, function(i) {
  if (i == 2) {
    create_custom_heatmap(bias_factors_beta[[i]], show_legend = (i == 3), show_grid = TRUE, common_scale = common_scale_bias, title = "Beta Average Biases") + color_scale_bias
  } else {
    create_custom_heatmap(bias_factors_beta[[i]], show_legend = (i == 3), show_grid = TRUE, common_scale = common_scale_bias) + color_scale_bias
  }
})
# Spatial MSEs Factors
p_mse <- lapply(1:3, function(i) {
  if (i == 2) {
    create_custom_heatmap(mse_factors[[i]], show_legend = FALSE, show_grid = TRUE, common_scale = common_scale_mse, title = "Spatial MSEs") + color_scale_mse
  } else {
    create_custom_heatmap(mse_factors[[i]], show_legend = FALSE, show_grid = TRUE, common_scale = common_scale_mse) + color_scale_mse
  }
})
# Beta MSEs Factors
p_mse_beta <- lapply(1:3, function(i) {
  if (i == 2) {
    create_custom_heatmap(mse_factors_beta[[i]], show_legend = (i == 3), show_grid = TRUE, common_scale = common_scale_mse, title = "Beta MSEs") + color_scale_mse
  } else {
    create_custom_heatmap(mse_factors_beta[[i]], show_legend = (i == 3), show_grid = TRUE, common_scale = common_scale_mse) + color_scale_mse
  }
})

# Arrange the plots according to your updated sketch
final_plot <- (
  # Left side: True, Estimated, Estimated Beta
  (p_true[[1]] | p_true[[2]] | p_true[[3]]) /
    (p_est[[1]] | p_est[[2]] | p_est[[3]]) /
    (p_est_beta[[1]] | p_est_beta[[2]] | p_est_beta[[3]]) |
    
    # Right side: Bias, Bias Beta, MSE, MSE Beta, with legends to the right
    (p_bias[[1]] | p_bias[[2]] | p_bias[[3]]) /
    (p_bias_beta[[1]] | p_bias_beta[[2]] | p_bias_beta[[3]]) /
    (p_mse[[1]] | p_mse[[2]] | p_mse[[3]]) /
    (p_mse_beta[[1]] | p_mse_beta[[2]] | p_mse_beta[[3]])
)

# Display the final plot
final_plot
stop()
ggsave("~/Downloads/loadings-hard-moderate.png", width = 8.27, height = 4.68, units = "in", dpi = 300)


# Find matches ####
# Function to compare GAMMA to B_True for a specific dataframe and return indices of mismatches
# compare_GAMMA_to_B_true <- function(dataframe, type = "em") {
#   # Select the appropriate GAMMA_permuted list based on the type
#   GAMMA_permuted_list <- switch(type,
#                                 em = dataframe$GAMMA_permuted,
#                                 em_beta = dataframe$GAMMA_permuted_beta,
#                                 em_pxl = dataframe$GAMMA_permuted_pxl,
#                                 em_beta_pxl = dataframe$GAMMA_permuted_beta_pxl,
#                                 stop("Invalid type specified. Choose 'em', 'em_beta', or 'em_pxl'."))
# 
#   B_true <- dataframe$B_True[[1]]  # Assuming B_True is consistent across simulations
# 
#   # Compare each GAMMA_permuted matrix with B_True and get mismatch indices
#   match_results <- sapply(GAMMA_permuted_list, function(GAMMA_permuted) {
#     isTRUE(all.equal(GAMMA_permuted, B_true))
#   })
# 
#   # Get the indices where there are no exact matches (FALSE values in match_results)
#   mismatch_indices <- which(!match_results)
# 
#   return(mismatch_indices)
# }
# test <- compare_GAMMA_to_B_true(structure, type = "em")
# test2 <- compare_GAMMA_to_B_true(structure, type = "em_beta")
# length(test)
# length(test2)
# test
# test2