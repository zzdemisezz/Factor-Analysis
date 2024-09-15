# rm(list = ls())
# library("viridisLite")
# library("viridis")
# library("RColorBrewer")
# library("pheatmap")
library("ggplot2")
library("reshape2")
library("patchwork")
library("cowplot")

source("Scripts/Analysis/Final_Functions.R")

#results <- readRDS("results_beta_pxl.rds") # has iter and converged
# structure <- results$`hard-moderate`
# results2 <- readRDS("results_old.rds")
structure <- results2$`overlap2-large-strong`

matrices <- calculate_statistics(structure, type = "em")
matrices_beta <- calculate_statistics(structure, type = "em_beta")

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

# Covariances ####
# Common scale for the top 3 heatmaps (covariance matrices)
common_scale_top <- range(c(matrices$Covariance_true, matrices$average_covariance, matrices_beta$average_covariance))

# Common scale for the bottom 2 heatmaps (MSE matrices)
common_scale_bottom <- range(c(matrices$MSE_covariance, matrices_beta$MSE_covariance))

# Define color scales for each group
# Top row color scale (e.g., blue to red)
color_scale_top <- scale_fill_gradient2(low = "white", high = "black", mid = "blue", midpoint = mean(common_scale_top))

# Bottom row color scale (e.g., white to red for MSE)
color_scale_bottom <- scale_fill_gradient2(low = "white", high = "red", mid = "pink", midpoint = mean(common_scale_bottom))

# Top row (True, Estimated, Beta Estimated) - use the top scale, no legend
p1 <- create_custom_heatmap(matrices$Covariance_true, show_legend = FALSE, show_grid = FALSE, common_scale = common_scale_top, title = expression("True" ~ Psi)) + color_scale_top
p2 <- create_custom_heatmap(matrices$average_covariance, show_legend = FALSE, show_grid = FALSE, common_scale = common_scale_top, title = expression("Average Estimated" ~ Psi)) + color_scale_top
p3 <- create_custom_heatmap(matrices_beta$average_covariance, show_legend = TRUE, show_grid = FALSE, common_scale = common_scale_top, title = expression("Beta Average Estimated" ~ Psi)) + color_scale_top

# Bottom row (MSE, Beta MSE) - use the bottom scale, one legend on the right
p4 <- create_custom_heatmap(matrices$MSE_covariance, show_legend = FALSE, show_grid = FALSE, common_scale = common_scale_bottom, title = expression("MSE" ~ Psi)) + color_scale_bottom
p5 <- create_custom_heatmap(matrices_beta$MSE_covariance, show_legend = TRUE, show_grid = FALSE, common_scale = common_scale_bottom, title = expression("Beta MSE" ~ Psi)) + color_scale_bottom

# Combine the heatmaps using patchwork
final_plot <- (p1 | p2 | p3) / (p4 | p5)

# Display the final plot
final_plot
# stop()
# ggsave("~/Downloads/covariances.png", width=16, height = 8)

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

# Define color schemes ensuring that 0 is white for each scale
# Color scale for True and Estimated - 0 is white
color_scale_true_est <- scale_fill_gradient2(low = "blue", mid = "white", high = "darkblue", midpoint = 0, limits = common_scale_left)
# Left side (True, Estimated, Estimated Beta) - sharing the same color scale
p_true <- lapply(1:3, function(i) create_custom_heatmap(true_factors[[i]], show_legend = FALSE, show_grid = TRUE, common_scale = common_scale_left, title = paste("True", i)) + color_scale_true_est)
true_plot <- (p_true[[1]] | p_true[[2]] | p_true[[3]]) 
# true_plot
# true_plot <- plot_grid(
#   p_true[[1]], p_true[[2]], p_true[[3]],
#   ncol = 3, align = "h", axis = "tb"
# )
# true_plot

# ggsave("~/Downloads/true_loadings.png")

# Color scale for Bias - 0 is white
color_scale_bias <- scale_fill_gradient2(low = "red", mid = "white", high = "red", midpoint = 0, limits = common_scale_bias)

# Color scale for MSE - 0 is white
color_scale_mse <- scale_fill_gradient2(low = "lightgreen", mid = "white", high = "darkgreen", midpoint = 0, limits = common_scale_mse)

p_est <- lapply(1:3, function(i) create_custom_heatmap(est_factors[[i]], show_legend = FALSE, show_grid = TRUE, common_scale = common_scale_left, title = paste("Estimated", i)) + color_scale_true_est)
p_est_beta <- lapply(1:3, function(i) create_custom_heatmap(est_factors_beta[[i]], show_legend = (i == 3), show_grid = TRUE, common_scale = common_scale_left, title = paste("Estimated Beta", i)) + color_scale_true_est)

# Right side (Bias, Bias Beta, MSE, MSE Beta) with legends
p_bias <- lapply(1:3, function(i) create_custom_heatmap(bias_factors[[i]], show_legend = FALSE, show_grid = TRUE, common_scale = common_scale_bias, title = paste("Bias", i)) + color_scale_bias)
p_bias_beta <- lapply(1:3, function(i) create_custom_heatmap(bias_factors_beta[[i]], show_legend = (i == 3), show_grid = TRUE, common_scale = common_scale_bias, title = paste("Bias Beta", i)) + color_scale_bias)

p_mse <- lapply(1:3, function(i) create_custom_heatmap(mse_factors[[i]], show_legend = FALSE, show_grid = TRUE, common_scale = common_scale_mse, title = paste("MSE", i)) + color_scale_mse)
p_mse_beta <- lapply(1:3, function(i) create_custom_heatmap(mse_factors_beta[[i]], show_legend = (i == 3), show_grid = TRUE, common_scale = common_scale_mse, title = paste("MSE Beta", i)) + color_scale_mse)

# Arrange the plots according to your updated sketch
final_plot <- (
  # Left side: True, Estimated, Estimated Beta
  (p_true[[1]] | p_true[[2]] | p_true[[3]]) /
    (p_est[[1]] | p_est[[2]] | p_est[[3]]) /
    (p_est_beta[[1]] | p_est_beta[[2]] | p_est_beta[[3]]) |
    
    # Right side: Bias, Bias Beta, MSE, MSE Beta, with legends at the bottom
    (p_bias[[1]] | p_bias[[2]] | p_bias[[3]]) /
    (p_bias_beta[[1]] | p_bias_beta[[2]] | p_bias_beta[[3]]) /
    (p_mse[[1]] | p_mse[[2]] | p_mse[[3]]) /
    (p_mse_beta[[1]] | p_mse_beta[[2]] | p_mse_beta[[3]])
)

# Display the final plot
final_plot

# ggsave("~/Downloads/loadings.png", width = 10, height = 8, dpi = 300)

stop()
# OLD ####

# Convert the matrix to a format suitable for ggplot2
matrix_melted <- melt(matrix)
  
ggplot(matrix_melted, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +  # No internal tile borders
  scale_y_reverse() +  # Reverse the y-axis (so the first row is at the top)
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),       # Remove axis text
    axis.title = element_blank(),      # Remove axis titles
    axis.ticks = element_blank(),      # Remove axis ticks
    panel.grid = element_blank(),      # Remove grid lines
    panel.border = element_rect(color = "black", fill = NA, size = 1)  # Add border only around the matrix
  ) +
  coord_fixed()  # Ensure that the aspect ratio of the tiles remains square

# Option 3 ####
levelplot(t(factors[[1]]), xlab = NULL, ylab = NULL)
library(lattice)

# Assuming your matrix is called 'matrix_data'
levelplot(t(factors[[1]]),
          xlab = NULL, ylab = NULL,
          col.regions = colorRampPalette(c("white", "purple"))(100),  # Color palette
          at = seq(0, 1, length.out = 101),  # Adjust 'at' for color intervals if necessary
          scales = list(draw = FALSE),  # Remove axis labels and ticks
          colorkey = FALSE,  # Remove the color legend
          aspect = "iso",  # Fix aspect ratio to make the plot square
          panel = function(...) {
            panel.levelplot(...)  # Call the panel function for levelplot
            panel.rect(xleft = 0.5, ybottom = 0.5, 
                       xright = ncol(t(factors[[1]])) + 0.5, 
                       ytop = nrow(t(factors[[1]])) + 0.5, 
                       border = "black", lwd = 2)  # Add black border around the entire matrix
          })
levelplot(t(factors[[2]]),
          xlab = NULL, ylab = NULL,
          col.regions = colorRampPalette(c("white", "purple"))(100),  # Color palette
          at = seq(0, 1, length.out = 101),  # Adjust 'at' for color intervals if necessary
          scales = list(draw = FALSE),  # Remove axis labels and ticks
          colorkey = FALSE,  # Remove the color legend
          aspect = "iso",  # Fix aspect ratio to make the plot square
          panel = function(...) {
            panel.levelplot(...)  # Call the panel function for levelplot
            panel.rect(xleft = 0.5, ybottom = 0.5, 
                       xright = ncol(t(factors[[1]])) + 0.5, 
                       ytop = nrow(t(factors[[1]])) + 0.5, 
                       border = "black", lwd = 2)  # Add black border around the entire matrix
          })
levelplot(t(factors[[3]]),
          xlab = NULL, ylab = NULL,
          col.regions = colorRampPalette(c("white", "purple"))(100),  # Color palette
          at = seq(0, 1, length.out = 101),  # Adjust 'at' for color intervals if necessary
          scales = list(draw = FALSE),  # Remove axis labels and ticks
          colorkey = FALSE,  # Remove the color legend
          aspect = "iso",  # Fix aspect ratio to make the plot square
          panel = function(...) {
            panel.levelplot(...)  # Call the panel function for levelplot
            panel.rect(xleft = 0.5, ybottom = 0.5, 
                       xright = ncol(t(factors[[1]])) + 0.5, 
                       ytop = nrow(t(factors[[1]])) + 0.5, 
                       border = "black", lwd = 2)  # Add black border around the entire matrix
          })



# Option 4 ####
# Key is legend, border is black borders, asp is square, 
plot(matrices$Covariance_true, border=NA, col = custom_palette, key=NULL, axis.col=NULL, 
     axis.row=NULL, xlab='', ylab='', main = "True Covariance Matrix")
plot(matrices$average_covariance, border=NA, col = custom_palette, key=NULL, axis.col=NULL, 
     axis.row=NULL, xlab='', ylab='', main = "Average Estimated Covariance Matrix")
plot(matrices$MSE_covariance, border=NA, col = custom_palette, key=NULL, axis.col=NULL, 
     axis.row=NULL, xlab='', ylab='', main = "Average MSE Covariance Matrix")
levelplot(t(matrices$B_true))


plot(factors[[1]], border=NA, asp = TRUE, key=NULL, axis.col=NULL, 
     axis.row=NULL, xlab='', ylab='', col = topo.colors, main = "True Factor 1")
plot(factors[[2]], border=NA, asp = TRUE, key=NULL, axis.col=NULL, 
     axis.row=NULL, xlab='', ylab='', col = topo.colors, main = "True Factor 2")
plot(factors[[3]], border=NA, asp = TRUE, key=NULL, axis.col=NULL, 
     axis.row=NULL, xlab='', ylab='', col = topo.colors, main = "True Factor 3")




