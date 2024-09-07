rm(list = ls())
source("Scripts/Analysis/functions.R")
source("Scripts/Analysis/loading-objects.R")

# Specifying the directory results
main_dir <- "Results/results_complex" 
subdir_paths <- generate_subdir_paths(main_dir)

# all_analysis_results <- readRDS("all_analysis_results.rds")

# Need to change main-dir inside function below, also need to change names of columns
# Function to calculate and plot element-wise metrics (Bias, Variance, MSE)
boxplot_structures <- function(calc_function, metric_name, matrix_name, true_matrix_name = NULL) {
  all_metrics <- list()
  
  for (path in subdir_paths) {
    # Extract the structure name from the path
    structure_name <- gsub("Results/results_complex/", "", path)
    
    # Split the structure name into parts
    parts <- strsplit(structure_name, "-")[[1]]
    
    if (length(parts) == 2) {
      # For names like "3x3-weak"
      structure <- parts[1]
      condition <- parts[2]
    } else if (length(parts) == 3) {
      # For names like "overlap2-small-weak"
      structure <- paste(parts[1], parts[2], sep = "-")
      condition <- parts[3]
    } else {
      stop("Unexpected structure name format")
    }
    
    # Shorten the structure names
    structure <- gsub("overlap2-small", "o2-s", structure)
    structure <- gsub("overlap2-large", "o2-l", structure)
    structure <- gsub("overlap3-small", "o3-s", structure)
    structure <- gsub("overlap3-large", "o3-l", structure)
    
    # Calculate the metric for the current structure
    if (!is.null(true_matrix_name)) {
      metrics_matrix <- calc_function(all_analysis_results[[structure_name]][[matrix_name]], all_analysis_results[[structure_name]][[true_matrix_name]][[1]])
    } else {
      metrics_matrix <- calc_function(all_analysis_results[[structure_name]][[matrix_name]])
    }
    
    # Flatten the matrix to a vector and convert to a data frame
    metrics_df <- data.frame(
      Value = as.vector(metrics_matrix)
    )
    
    # Add columns for the structure and condition
    metrics_df$Structure <- structure_name
    metrics_df$Condition <- condition
    
    # Store the metrics in the list
    all_metrics[[structure_name]] <- metrics_df
  }
  
  # Combine all metrics into a single dataframe
  metrics_combined <- do.call(rbind, all_metrics)
  
  # Ensure the structure and condition columns are treated as factors
  metrics_combined$Structure <- factor(metrics_combined$Structure, 
                                       levels = c("3x3", "5x5", "o2-s", "o2-l", "o3-s", "o3-l"))
  metrics_combined$Condition <- factor(metrics_combined$Condition, levels = c("weak", "moderate", "strong"))
  
  # Generate the title using the exact matrix name
  title_text <- paste(metric_name, "of", matrix_name, "for All Structures")
  
  # Generate the combined boxplot for the metric
  p <- ggplot(metrics_combined, aes(x = Condition, y = Value, fill = Condition)) +
    geom_boxplot() +
    labs(title = title_text, y = metric_name, x = "Condition") +
    theme_minimal() +
    scale_fill_manual(values = c("weak" = "green", "moderate" = "blue", "strong" = "red")) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"  # Remove the legend entirely
    ) +
    facet_wrap(~ Structure, nrow = 1, scales = "free_x")  # Unified facet order
  
  return(p)
}

# MSE B
plot_mse <- boxplot_structures(calculate_elementwise_MSE_matrix, "MSE", "B_permuted", "B_True")
plot_mse_beta <- boxplot_structures(calculate_elementwise_MSE_matrix, "MSE", "B_permuted_beta", "B_True")
print(plot_mse)
stop()
print(plot_mse_beta)

# MSE Covariance Matrix
plot_mse_cov <- boxplot_structures(calculate_elementwise_MSE_matrix, "MSE", "Covariance_matrix", "Covariance_matrix_true")
plot_mse_cov_beta <- boxplot_structures(calculate_elementwise_MSE_matrix, "MSE", "Covariance_matrix_beta", "Covariance_matrix_true")
print(plot_mse_cov)
print(plot_mse_cov_beta)
