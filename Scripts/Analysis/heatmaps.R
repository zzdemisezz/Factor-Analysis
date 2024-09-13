rm(list = ls())
source("Scripts/Analysis/functions.R")
source("Scripts/Analysis/loading-objects.R")

all_analysis_results <- readRDS("results_new.rds")

# Load the relevant dataframe
# heatmap_dataframe <- all_analysis_results$`5x5-weak`
heatmap_dataframe <- all_analysis_results$`medium-moderate`

# Calculate statistics for B_permuted and B_permuted_beta 
statistics_em <- calculate_statistics(heatmap_dataframe, type = "em")
statistics_beta <- calculate_statistics(heatmap_dataframe, type = "em_beta")

# EM
heatmap_true_loadings <- create_heatmap(statistics_em$B_true, title = "Heatmap of True Loading Matrix")
heatmap_average_loadings <- create_heatmap(statistics_em$average_matrix, title = "Heatmap of Average Loading Matrix")
heatmap_bias_loadings <- create_heatmap(statistics_em$bias_matrix, title = "Heatmap of Bias Loading Matrix")
heatmap_variance_loadings <- create_heatmap(statistics_em$variance_matrix, title = "Heatmap of Variance Loading Matrix")
heatmap_mse_loadings <- create_heatmap(statistics_em$MSE_matrix, title = "Heatmap of MSE Loading Matrix")

# Beta
heatmap_average_loadings_beta <- create_heatmap(statistics_beta$average_matrix, title = "Heatmap of Average Loading Matrix - Beta")
heatmap_bias_loadings_beta <- create_heatmap(statistics_beta$bias_matrix, title = "Heatmap of Bias Loading Matrix - Beta")
heatmap_variance_loadings_beta <- create_heatmap(statistics_beta$variance_matrix, title = "Heatmap of Variance Loading Matrix - Beta")
heatmap_mse_loadings_beta <- create_heatmap(statistics_beta$MSE_matrix, title = "Heatmap of MSE Loading Matrix - Beta")

# TRUE
print(heatmap_true_loadings)
stop()

# EM
print(heatmap_average_loadings)
print(heatmap_bias_loadings)
print(heatmap_variance_loadings)
print(heatmap_mse_loadings)

# Beta
print(heatmap_average_loadings_beta)
print(heatmap_bias_loadings_beta)
print(heatmap_variance_loadings_beta)
print(heatmap_mse_loadings_beta)

# Covariances ####

# TRUE
heatmap_true_covariance <- create_heatmap(statistics_em$Covariance_true, title = "Heatmap of True Covariance Matrix")

# EM
heatmap_average_covariance <- create_heatmap(statistics_em$average_covariance, title = "Heatmap of Average Covariance Matrix")
heatmap_variance_covariance <- create_heatmap(statistics_em$variance_covariance, title = "Heatmap of Variance Covariance Matrix")
heatmap_bias_covariance <- create_heatmap(statistics_em$bias_covariance, title = "Heatmap of Bias Covariance Matrix")
heatmap_mse_covariance <- create_heatmap(statistics_em$MSE_covariance, title = "Heatmap of MSE Covariance Matrix")

# Beta
heatmap_average_covariance_beta <- create_heatmap(statistics_beta$average_covariance, title = "Heatmap of Average Covariance Matrix - beta")
heatmap_bias_covariance_beta <- create_heatmap(statistics_beta$bias_covariance, title = "Heatmap of Bias Covariance Matrix - beta")
heatmap_variance_covariance_beta <- create_heatmap(statistics_beta$variance_covariance, title = "Heatmap of Variance Covariance Matrix - beta")
heatmap_mse_covariance_beta <- create_heatmap(statistics_beta$MSE_covariance, title = "Heatmap of MSE Covariance Matrix - beta")

# EM
print(heatmap_true_covariance)
print(heatmap_average_covariance)
print(heatmap_bias_covariance)
print(heatmap_variance_covariance)
print(heatmap_mse_covariance)

# Beta
print(heatmap_average_covariance_beta)
print(heatmap_bias_covariance_beta)
print(heatmap_variance_covariance_beta)
print(heatmap_mse_covariance_beta)






