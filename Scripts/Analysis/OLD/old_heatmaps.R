rm(list = ls())
source("Scripts/Analysis/functions.R")
source("Scripts/Analysis/common_analysis.R")  # Source the shared script

# Set subdir_path
subdir_path <- "results4/LargeFactors_5x5_Strong"

# Perform analysis and return relevant objects
analysis_results <- perform_analysis(subdir_path)

# Access true matrices
B_true <- analysis_results$dataframe$B_True[[1]]
Covariance_true <- analysis_results$dataframe$Covariance_matrix_true[[1]]

# Generate heatmaps for the EM model
generate_heatmaps(B_true, analysis_results$statistics_EM, paste(data_name, "B Matrix"))
generate_heatmaps(Covariance_true, analysis_results$statistics_EM, paste(data_name, "Covariance Matrix"))

# Generate heatmaps for the EM BETA model
generate_heatmaps(B_true, analysis_results$statistics_EM_BETA, paste(data_name, "B Matrix (Beta)"))
generate_heatmaps(Covariance_true, analysis_results$statistics_EM_BETA, paste(data_name, "Covariance Matrix (Beta)"))
