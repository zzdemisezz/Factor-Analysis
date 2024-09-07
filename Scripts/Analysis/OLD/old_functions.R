library(ggplot2)
library(dplyr)
library(reshape2)
library(cowplot)

# generate_subdir_paths.R
generate_subdir_paths <- function(main_dir) {
  sizes <- c("3x3", "5x5", "overlap2-small", "overlap2-large", "overlap3-small", "overlap3-large")
  strengths <- c("strong", "moderate", "weak")
  
  paths <- c()
  
  for (size in sizes) {
    for (strength in strengths) {
      path <- file.path(main_dir, paste(size, strength, sep="-"))
      paths <- c(paths, path)
    }
  }
  
  return(paths)
}

# Generate factor heatmaps
create_factor_heatmap <- function(loadings_matrix, title = "True factors") {
  library(ggplot2)
  library(reshape2)
  library(cowplot)
  
  # Split the 100x3 loadings matrix into three 10x10 matrices
  split_loadings_to_matrices <- function(loadings_matrix) {
    if (nrow(loadings_matrix) != 100 || ncol(loadings_matrix) != 3) {
      stop("The input matrix must be 100x3.")
    }
    
    matrices_list <- list()
    for (i in 1:ncol(loadings_matrix)) {
      factor_column <- loadings_matrix[, i]
      matrices_list[[i]] <- matrix(factor_column, nrow = 10, ncol = 10, byrow = FALSE)
    }
    
    return(matrices_list)
  }
  
  # Create a custom heatmap for one of the matrices
  create_custom_heatmap <- function(matrix_data, show_legend = TRUE, common_scale = NULL) {
    ggplot(melt(matrix_data), aes(x = Var2, y = Var1, fill = value)) +
      geom_tile(color = "gray") +  
      scale_y_reverse() +  
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, 
                           limits = common_scale) +  # Use the common scale for the fill
      theme_minimal() +
      theme(
        legend.position = if (show_legend) "right" else "none",
        legend.justification = "center",
        aspect.ratio = 1,  
        axis.title = element_blank(),  
        axis.text = element_blank(),   
        axis.ticks = element_blank(),  
        panel.grid = element_blank(),  
        plot.margin = margin(0, 0, 0, 0) 
      ) +
      coord_fixed()  
  }
  
  # Function to extract the legend from a ggplot object
  get_legend <- function(ggplot_obj) {
    legend <- cowplot::get_legend(ggplot_obj + theme(legend.position = "right"))
    return(legend)
  }
  
  # Split the loadings matrix into three 10x10 matrices
  split_matrices <- split_loadings_to_matrices(loadings_matrix)
  
  # Calculate the common scale (min and max values across all matrices)
  common_scale <- range(unlist(split_matrices))
  
  # Create heatmaps for each matrix using the common scale
  real_factor1 <- create_custom_heatmap(split_matrices[[1]], show_legend = FALSE, common_scale = common_scale)
  real_factor2 <- create_custom_heatmap(split_matrices[[2]], show_legend = FALSE, common_scale = common_scale)
  real_factor3 <- create_custom_heatmap(split_matrices[[3]], show_legend = TRUE, common_scale = common_scale)
  
  # Extract the legend from the third plot
  legend <- get_legend(real_factor3)
  
  # Remove the legend from the third plot
  real_factor3 <- real_factor3 + theme(legend.position = "none")
  
  # Combine the heatmaps side by side with the extracted legend
  final_plot <- plot_grid(
    plot_grid(real_factor1, real_factor2, real_factor3, ncol = 3, align = "h", axis = "tb"),
    legend,
    ncol = 2,
    rel_widths = c(3, 0.3)
  )
  
  # Print the final plot
  print(final_plot)
}

# Function to create a heatmap with the correct orientation
create_heatmap <- function(matrix_data, title = "Heatmap", x_label = "Factors", y_label = "Loadings") {
  
  # Convert the matrix to a format suitable for ggplot2
  matrix_melted <- melt(matrix_data)
  
  # Check if the melt was successful
  if (!all(c("Var1", "Var2", "value") %in% colnames(matrix_melted))) {
    stop("The melted data frame does not contain the expected 'Var1', 'Var2', and 'value' columns.")
  }
  
  # Create the heatmap with the y-axis reversed
  heatmap_plot <- ggplot(matrix_melted, aes(x = Var2, y = Var1, fill = value)) +
    geom_tile() +
    scale_y_reverse() +  # Reverse the y-axis
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
    theme_minimal() +
    labs(title = title, x = x_label, y = y_label)
  
  # Return the plot object
  return(heatmap_plot)
}
# Define a function to generate and print heatmaps
generate_heatmaps <- function(true_matrix, statistics, prefix) {
  if (grepl("Covariance", prefix)) {
    heatmap_true <- create_heatmap(true_matrix, title = paste("Heatmap of true", prefix))
    heatmap_average <- create_heatmap(statistics$average_covariance, title = paste("Heatmap of average", prefix))
    heatmap_bias <- create_heatmap(statistics$bias_covariance, title = paste("Heatmap of bias", prefix))
    heatmap_variance <- create_heatmap(statistics$variance_covariance, title = paste("Heatmap of variance", prefix))
    heatmap_MSE <- create_heatmap(statistics$MSE_covariance, title = paste("Heatmap of MSE", prefix))
  } else {
    heatmap_true <- create_heatmap(true_matrix, title = paste("Heatmap of true", prefix))
    heatmap_average <- create_heatmap(statistics$average_matrix, title = paste("Heatmap of average", prefix))
    heatmap_bias <- create_heatmap(statistics$bias_matrix, title = paste("Heatmap of bias", prefix))
    heatmap_variance <- create_heatmap(statistics$variance_matrix, title = paste("Heatmap of variance", prefix))
    heatmap_MSE <- create_heatmap(statistics$MSE_matrix, title = paste("Heatmap of MSE", prefix))
  }
  
  # Print all heatmaps
  print(heatmap_true)
  print(heatmap_average)
  print(heatmap_bias)
  print(heatmap_variance)
  print(heatmap_MSE)
}

# Calculate statistic matrices
calculate_average_matrix <- function(matrix_list) {
  Reduce(`+`, matrix_list) / length(matrix_list)
}
calculate_variance <- function(matrix_list) {
  # Calculate the variance for each element across the list of matrices
  elementwise_variance <- Reduce(`+`, lapply(matrix_list, function(x) (x - calculate_average_matrix(matrix_list))^2)) / (length(matrix_list) - 1)
  return(elementwise_variance)
}
calculate_MSE <- function(true_matrix, matrix_list) {
  mse_matrix <- Reduce(`+`, lapply(matrix_list, function(matrix) {
    (true_matrix - matrix)^2
  })) / length(matrix_list)
  
  return(mse_matrix)
}
# Calculate all statistic matrices for model
calculate_statistics <- function(dataframe, type = "em") {
  # Retrieve B_true and Covariance_true from the dataframe
  B_true <- dataframe$B_True[[1]]
  Covariance_true <- dataframe$Covariance_matrix_true[[1]]
  
  # Select appropriate matrices based on the type
  if (type == "em") {
    permuted_matrices <- dataframe$B_permuted
    covariance_matrices <- dataframe$Covariance_matrix
  } else if (type == "em_beta") {
    permuted_matrices <- dataframe$B_permuted_beta
    covariance_matrices <- dataframe$Covariance_matrix_beta
  } else if (type == "em_pxl") {
    permuted_matrices <- dataframe$B_permuted_pxl
    covariance_matrices <- dataframe$Covariance_matrix_pxl
  } else {
    stop("Invalid type specified. Choose 'em', 'em_beta', or 'em_pxl'.")
  }
  
  # For B matrices
  average_matrix <- calculate_average_matrix(permuted_matrices)
  bias_matrix <- average_matrix - B_true
  variance_matrix <- calculate_variance(permuted_matrices)
  MSE_matrix <- calculate_MSE(B_true, permuted_matrices)
  
  # For Covariance matrices
  average_covariance <- calculate_average_matrix(covariance_matrices)
  bias_covariance <- average_covariance - Covariance_true
  variance_covariance <- calculate_variance(covariance_matrices)
  MSE_covariance <- calculate_MSE(Covariance_true, covariance_matrices)
  
  list(
    average_matrix = average_matrix,
    bias_matrix = bias_matrix,
    variance_matrix = variance_matrix,
    MSE_matrix = MSE_matrix,
    average_covariance = average_covariance,
    bias_covariance = bias_covariance,
    variance_covariance = variance_covariance,
    MSE_covariance = MSE_covariance
  )
}
# Calculate metrics
calculate_metrics <- function(dataframe, type = "em") {
  statistics <- calculate_statistics(dataframe, type = type)
  metrics <- c(
    mean_bias = mean(statistics$bias_matrix),
    mean_variance = mean(statistics$variance_matrix),
    mean_mse = mean(statistics$MSE_matrix)
  )
  metrics_covariance <- c(
    mean_bias = mean(statistics$bias_covariance),
    mean_variance = mean(statistics$variance_covariance),
    mean_mse = mean(statistics$MSE_covariance)
  )
  structure_check <- compare_GAMMA_to_B_true(dataframe, type = type)
  
  return(list(
    metrics_B = metrics,
    metrics_Covariance = metrics_covariance,
    exact_matches = structure_check$exact_matches,
    non_matching_indices = structure_check$non_matching_indices
  ))
}
# Function to see how often we get structure right
compare_GAMMA_to_B_true <- function(dataframe, type = "em") {
  if (type == "em") {
    GAMMA_permuted_list <- dataframe$GAMMA_permuted
  } else if (type == "em_beta") {
    GAMMA_permuted_list <- dataframe$GAMMA_permuted_beta
  } else if (type == "em_pxl") {
    GAMMA_permuted_list <- dataframe$GAMMA_permuted_pxl
  } else {
    stop("Invalid type specified. Choose 'em', 'em_beta', or 'em_pxl'.")
  }
  
  B_true <- dataframe$B_True[[1]]  # Assuming B_true is the same across simulations
  
  # Compare each GAMMA_permuted matrix with B_true and check if they match
  match_results <- sapply(GAMMA_permuted_list, function(GAMMA_permuted) {
    isTRUE(all.equal(GAMMA_permuted, B_true))
  })
  
  # Get indices where the matrices do not match
  non_matching_indices <- which(!match_results)
  
  # Calculate the number of exact matches
  exact_matches <- sum(match_results)
  
  return(list(
    exact_matches = exact_matches,
    non_matching_indices = non_matching_indices
  ))
}

# Functions to create box plots
# Function to calculate bias, variance, and MSE for a given matrix type
calculate_bias_variance_mse <- function(dataframe, true_matrix, matrix_name) {
  metrics_list <- list()
  
  for (i in 1:nrow(dataframe)) {
    permuted_matrix <- dataframe[[matrix_name]][[i]]
    
    bias <- mean(permuted_matrix - true_matrix)
    variance <- mean((permuted_matrix - mean(permuted_matrix))^2)
    mse <- mean((permuted_matrix - true_matrix)^2)
    
    metrics_list[[i]] <- data.frame(
      Bias = bias,
      Variance = variance,
      MSE = mse
    )
  }
  
  metrics_df <- bind_rows(metrics_list)
  return(metrics_df)
}
# Function to compile metrics data and generate the plot
generate_metric_plot <- function(all_analysis_results, matrix_name, true_matrix_name, metric_name) {
  metrics_data <- list()
  
  for (dataset_name in names(all_analysis_results)) {
    dataframe <- all_analysis_results[[dataset_name]]$dataframe
    true_matrix <- dataframe[[true_matrix_name]][[1]]
    
    # Calculate metrics
    metrics_df <- calculate_bias_variance_mse(dataframe, true_matrix, matrix_name)
    
    # Extract condition and base name
    condition <- gsub(".*_", "", dataset_name)
    base_name <- gsub("_Strong|_Moderate|_Weak", "", dataset_name)
    
    # Add labels to the metrics
    metrics_df$Condition <- factor(condition, levels = c("Weak", "Moderate", "Strong"))
    metrics_df$Dataset <- base_name
    
    metrics_data[[dataset_name]] <- metrics_df
  }
  
  metrics_data_combined <- bind_rows(metrics_data)
  
  metrics_data_combined$Dataset <- factor(metrics_data_combined$Dataset, levels = c("3x3", "5x5", "overlap2-small", "overlap2-large", "overlap3-small", "overlap3-large"))
  
  # Determine the title based on the type of matrix
  if (grepl("Covariance", matrix_name, ignore.case = TRUE)) {
    title_text <- paste(metric_name, "of Covariance for each structure")
  } else {
    title_text <- paste(metric_name, "of B for each structure")
  }
  
  # Generate the plot
  ggplot(metrics_data_combined %>% select(Condition, Value = !!sym(metric_name), Dataset) %>% filter(!is.na(Value)), aes(x = Condition, y = Value, fill = Condition)) +
    geom_boxplot() +
    labs(title = title_text, x = "Condition", y = metric_name) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(values = c("Weak" = "green", "Moderate" = "blue", "Strong" = "red")) +
    facet_grid(. ~ Dataset, scales = "free_x")
}
