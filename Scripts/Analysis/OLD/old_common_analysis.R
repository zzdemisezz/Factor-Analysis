source("Scripts/Analysis/loading-objects.R")

perform_analysis <- function(subdir_path) {
  # Automatically extract dataset name from subdir_path
  data_name <- gsub(".*/", "", subdir_path)  # Removes everything before the last '/'
  data_name <- gsub("/", "_", data_name)      # Replaces remaining slashes with underscores
  
  # Load the data
  assign(data_name, load_data(subdir_path))
  dataframe <- get(data_name)$dataframe
  raw_data <- get(data_name)$raw_data
  
  # # Statistics matrices
  # statistics_EM <- calculate_statistics(dataframe, type = "em")
  # statistics_EM_BETA <- calculate_statistics(dataframe, type = "em_beta")
  # statistics_EM_PXL <- calculate_statistics(dataframe, type = "em_pxl")
  # 
  # # Calculate metrics for EM, EM BETA, and EM PXL
  # results_em <- calculate_metrics(dataframe, type = "em")
  # results_em_beta <- calculate_metrics(dataframe, type = "em_beta")
  # results_em_pxl <- calculate_metrics(dataframe, type = "em_pxl")
  # 
  # # Combine results into data frames with the new information
  # combined_results <- data.frame(
  #   Metric = c("Bias", "Variance", "MSE", "Number of Exact Matches"),
  #   B = c(as.numeric(results_em$metrics_B), results_em$exact_matches),
  #   B_beta = c(as.numeric(results_em_beta$metrics_B), results_em_beta$exact_matches),
  #   B_pxl = c(as.numeric(results_em_pxl$metrics_B), results_em_pxl$exact_matches),
  #   Covariance = c(as.numeric(results_em$metrics_Covariance), NA),
  #   Covariance_beta = c(as.numeric(results_em_beta$metrics_Covariance), NA),
  #   Covariance_pxl = c(as.numeric(results_em_pxl$metrics_Covariance), NA)
  # )
  # 
  # # Print the combined results
  # print(combined_results)
  # 
  # return(list(
  #   dataframe = dataframe,
  #   statistics_EM = statistics_EM,
  #   statistics_EM_BETA = statistics_EM_BETA,
  #   statistics_EM_PXL = statistics_EM_PXL,
  #   combined_results = combined_results
  # ))
  return(list(dataframe = dataframe, raw_data = raw_data))
}
