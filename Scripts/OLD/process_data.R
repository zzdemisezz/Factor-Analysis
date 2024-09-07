#!/usr/bin/env Rscript

# Load the necessary R script for the permute function
source("/users/nichols/rht383/FactorAnalysis/Scripts/Analysis/Permute.R")

# Arguments from command line (list of files and output directory)
args <- commandArgs(trailingOnly = TRUE)
rds_files <- unlist(strsplit(args[1], " "))  # The list of files
output_dir <- args[2]

# Create a new folder for processed chunks if it doesn't already exist
processed_dir <- file.path(output_dir, "processed_chunks")
if (!dir.exists(processed_dir)) {
  dir.create(processed_dir)
}

# Function to process a chunk of files
process_chunk <- function(rds_files, processed_dir) {
  simulation_data <- lapply(rds_files, readRDS)
  names(simulation_data) <- basename(rds_files)
  
  simulation_data_list <- list()
  
  for (i in 1:length(simulation_data)) {
    # Extract matrices from all_results_em
    B_truncated_matrix <- simulation_data[[i]]$all_results_em[[1]]$best_result$B_truncated
    GAMMA_truncated_matrix <- simulation_data[[i]]$all_results_em[[1]]$best_result$GAMMA_truncated
    Covariance_matrix <- simulation_data[[i]]$all_results_em[[1]]$best_result$Covariance_matrix_truncated
    
    # Extract matrices from all_results_em_beta
    B_beta <- simulation_data[[i]]$all_results_em_beta[[1]]$best_result$B_truncated
    GAMMA_beta <- simulation_data[[i]]$all_results_em_beta[[1]]$best_result$GAMMA_truncated
    Covariance_matrix_beta <- simulation_data[[i]]$all_results_em_beta[[1]]$best_result$Covariance_matrix_truncated
    
    # Extract matrices from all_results_em_pxl
    B_pxl <- simulation_data[[i]]$all_results_em_pxl[[1]]$best_result$B_truncated
    GAMMA_pxl <- simulation_data[[i]]$all_results_em_pxl[[1]]$best_result$GAMMA_truncated
    Covariance_matrix_pxl <- simulation_data[[i]]$all_results_em_pxl[[1]]$best_result$Covariance_matrix_truncated
    
    # Extract true matrices
    B_True <- simulation_data[[i]]$all_datasets[[1]]$B_true
    Covariance_matrix_true <- simulation_data[[i]]$all_datasets[[1]]$Covariance_matrix_true
    
    simulation_row <- data.frame(
      B = I(list(B_truncated_matrix)),
      GAMMA = I(list(GAMMA_truncated_matrix)),
      Covariance_matrix = I(list(Covariance_matrix)),
      
      B_beta = I(list(B_beta)),
      GAMMA_beta = I(list(GAMMA_beta)),
      Covariance_matrix_beta = I(list(Covariance_matrix_beta)),
      
      B_pxl = I(list(B_pxl)),
      GAMMA_pxl = I(list(GAMMA_pxl)),
      Covariance_matrix_pxl = I(list(Covariance_matrix_pxl)),
      
      B_True = I(list(B_True)),
      Covariance_matrix_true = I(list(Covariance_matrix_true)),
      Simulation = i
    )
    
    simulation_data_list[[i]] <- simulation_row
  }
  
  result_dataframe <- do.call(rbind, simulation_data_list)
  
  # Extract likelihoods and filter rows where likelihood == 0
  likelihoods_em <- sapply(1:nrow(result_dataframe), function(i) {
    simulation_data[[i]]$all_results_em[[1]]$best_result$likelihood
  })
  
  likelihoods_em_beta <- sapply(1:nrow(result_dataframe), function(i) {
    simulation_data[[i]]$all_results_em_beta[[1]]$best_result$likelihood
  })
  
  likelihoods_em_pxl <- sapply(1:nrow(result_dataframe), function(i) {
    simulation_data[[i]]$all_results_em_pxl[[1]]$best_result$likelihood
  })
  
  # Remove rows where any likelihood is zero
  zero_indices <- which(likelihoods_em == 0 | likelihoods_em_beta == 0 | likelihoods_em_pxl == 0)
  if (length(zero_indices) > 0) {
    result_dataframe <- result_dataframe[-zero_indices, ]
  }
  
  # Permute matrices
  B_true <- result_dataframe$B_True[[1]]  # Assuming B_true is the same across simulations
  
  add_permuted_B_GAMMA <- function(dataframe, B_true) {
    dataframe$B_permuted <- lapply(1:nrow(dataframe), function(i) {
      B_truncated <- dataframe$B[[i]]
      GAMMA_truncated <- dataframe$GAMMA[[i]]
      B_permuted <- permute_B(B_true, GAMMA_truncated, B_truncated)
      return(B_permuted)
    })
    
    dataframe$B_permuted_beta <- lapply(1:nrow(dataframe), function(i) {
      B_beta <- dataframe$B_beta[[i]]
      GAMMA_beta <- dataframe$GAMMA_beta[[i]]
      B_permuted_beta <- permute_B(B_true, GAMMA_beta, B_beta)
      return(B_permuted_beta)
    })
    
    dataframe$B_permuted_pxl <- lapply(1:nrow(dataframe), function(i) {
      B_pxl <- dataframe$B_pxl[[i]]
      GAMMA_pxl <- dataframe$GAMMA_pxl[[i]]
      B_permuted_pxl <- permute_B(B_true, GAMMA_pxl, B_pxl)
      return(B_permuted_pxl)
    })
    
    dataframe$GAMMA_permuted <- lapply(1:nrow(dataframe), function(i) {
      GAMMA_truncated <- dataframe$GAMMA[[i]]
      GAMMA_permuted <- permute_B(B_true, GAMMA_truncated, GAMMA_truncated)
      return(GAMMA_permuted)
    })
    
    dataframe$GAMMA_permuted_beta <- lapply(1:nrow(dataframe), function(i) {
      GAMMA_beta <- dataframe$GAMMA_beta[[i]]
      GAMMA_permuted_beta <- permute_B(B_true, GAMMA_beta, GAMMA_beta)
      return(GAMMA_permuted_beta)
    })
    
    dataframe$GAMMA_permuted_pxl <- lapply(1:nrow(dataframe), function(i) {
      GAMMA_pxl <- dataframe$GAMMA_pxl[[i]]
      GAMMA_permuted_pxl <- permute_B(B_true, GAMMA_pxl, GAMMA_pxl)
      return(GAMMA_permuted_pxl)
    })
    
    return(dataframe)
  }
  
  # Add permuted matrices to dataframe
  result_dataframe <- add_permuted_B_GAMMA(result_dataframe, B_true)
  
  # Save result as an RDS file, naming the file uniquely using the SLURM_ARRAY_TASK_ID
  saveRDS(result_dataframe, file = file.path(processed_dir, paste0("processed_chunk_", SLURM_ARRAY_TASK_ID, ".rds")))
}

# Process the files assigned to this job
process_chunk(rds_files, processed_dir)

cat("Data processing complete for files:", rds_files, "\n")
