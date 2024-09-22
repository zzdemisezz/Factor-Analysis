rm(list = ls())
library("patchwork")
source("Scripts/Analysis/Final_Functions.R")

# Load results
# results2 <- readRDS("results_final.rds")
results2 <- readRDS("results_beta_pxl.rds")

# Function to get only the names of the data structure with 1 correlation structure
select_unique_structures <- function(names_list) {
  # unique_prefixes <- c("3x3", "5x5", "overlap2-small","overlap2-large", "overlap3-small", "overlap3-large")
  unique_prefixes <- c("simple", "medium", "hard")
  selected_names <- sapply(unique_prefixes, function(prefix) {
    grep(paste0("^", prefix), names_list, value = TRUE)[1]  # Select the first match for each prefix
  })
  return(selected_names)
}

# Get the data structure names
unique_structures <- select_unique_structures(names(results2))

# Function to generate a plots for a given data structure 
create_plots_for_structure_with_label <- function(structure, label) {
  matrices <- calculate_statistics(structure, type = "em")
  
  # Split loadings into submatrices
  true_factors <- split_loadings_to_matrices(matrices$B_true)
  # Find common scales for each block
  common_scale_left <- range(c(true_factors, matrices$B_true))
  
  # Color scale for True and Estimated - 0 is white
  color_scale_true_est <- scale_fill_gradient2(low = "blue", mid = "white", high = "darkblue", midpoint = 0, limits = common_scale_left)
  
  # Left side (True, Estimated, Estimated Beta) - sharing the same color scale
  p_true <- lapply(1:3, function(i) create_custom_heatmap(true_factors[[i]], show_legend = FALSE, show_grid = TRUE, common_scale = common_scale_left, title = paste("Factor", i)) + color_scale_true_est)
  
  # Full loading matrix heatmap
  heatmap_full <- ggplot(melt(matrices$B_true), aes(x = Var2, y = Var1, fill = value)) +
    geom_tile(color = NA) +  
    scale_y_reverse() +  
    scale_fill_gradient2(low = "blue", high = "darkblue", mid = "white", midpoint = 0) +  
    theme_minimal() +
    theme(
      legend.position = "none",  # Remove legends
      aspect.ratio = 1,  
      axis.title = element_blank(),  
      axis.text = element_blank(),   
      axis.ticks = element_blank(),  
      panel.grid = element_blank(),  
      plot.margin = margin(0, 0, 0, 0),
      plot.title = element_text(hjust = 0.5, margin = margin(b = 1))  # Center the title
    ) +
    coord_fixed() +  
    annotate("rect", xmin = 0.5, xmax = ncol(matrices$B_true) + 0.5, 
             ymin = 0.5, ymax = nrow(matrices$B_true) + 0.5, 
             color = "black", fill = NA, size = 1) +  
    ggtitle("B") 
  
  # Add y-axis label to the first heatmap plot and remove y-axis tick labels
  p_true[[1]] <- p_true[[1]] + 
    labs(y = label) + 
    theme(
      axis.title.y = element_text(size = 10, angle = 90),  # Force y-axis label with a specific angle
      axis.text.y = element_blank()  # Remove the y-axis numbers
    )
  
  true_plot <- (p_true[[1]] | p_true[[2]] | p_true[[3]] | heatmap_full)
  
  return(true_plot)
}

# Create plots for all the data structures
all_plots <- lapply(seq_along(unique_structures), function(i) {
  struct_name <- unique_structures[i]
  structure <- results2[[struct_name]]
  
  # Generate the plot with the structure name as the y-axis label and no legends
  create_plots_for_structure_with_label(structure, paste("Structure", i+6))
})

# Combine the plots in a grid
combined_plot <- wrap_plots(all_plots, ncol = 1)

# Display the final combined plot
combined_plot
stop()
# Save the final plot to fit a full Overleaf page (A4 size)
# ggsave("~/Downloads/data_structures_10x10.png", combined_plot, 
#        width = 8.27, height = 11.69, units = "in", dpi = 300)
# Save the figure to be half the size of an A4 page
ggsave("~/Downloads/data_structures_20x20.png", combined_plot,
       width = 8.27, height = 5.85, units = "in", dpi = 300)


