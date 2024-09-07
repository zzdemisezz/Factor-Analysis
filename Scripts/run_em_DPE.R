run_em_algorithm_DPE <- function(em_function, data, q, dim1 = NULL, dim2 = NULL, 
                             dim3 = NULL, tol = 1e-2, max_iter = 5000, 
                             ll = TRUE, num_runs = 4) {
  
  # Generate 5 equispaced log values between -1 and -10 (in decreasing order), then exponentiate
  #log_v0 <- seq(-6, -10, length.out = 4)
  #v0_values <- exp(log_v0)
  
  results <- vector("list", num_runs + 1)  # To store the final result of each run (including PCA)
  log_liks <- numeric(num_runs + 1)  # To store the log-likelihood of the final result for each run (including PCA)
  total_time <- 0
  pca_best_count <- 0  # Counter to check if PCA initialization is the best
  
  for (run in 1:num_runs) {
    print(paste("Run", run))
    
    B <- NULL  # Initialize B as NULL for the first v0 of the run
    run_total_time <- 0  # Initialize time accumulator for the current run
    
    for (v0_idx in seq_along(v0_values)) {
      # Measure the time for each v0 iteration
      v0_time <- system.time({
        print(paste("  Iteration with v0 =", v0_values[v0_idx]))
        
        if (is.null(dim1) & is.null(dim2) & is.null(dim3)) {
          result <- em_function(data$Y, q, v0 = v0_values[v0_idx], B = B, max_iter = max_iter, ll = ll)
        } else if (is.null(dim3)) {
          result <- em_function(data$Y, q, dim1 = dim1, dim2 = dim2, v0 = v0_values[v0_idx], B = B, max_iter = max_iter, ll = ll)
        } else {
          result <- em_function(data$Y, q, dim1 = dim1, dim2 = dim2, dim3 = dim3, v0 = v0_values[v0_idx], B = B, max_iter = max_iter, ll = ll)
        }
        
        # Update B for the next iteration with the next v0
        B <- result$B
      })
      
      # Print the time taken for the current v0 iteration
      v0_seconds <- round(v0_time[3], 2)
      print(paste("    Time taken for v0 =", v0_values[v0_idx], ":", v0_seconds, "seconds"))
      
      # Accumulate total time for the current run
      run_total_time <- run_total_time + v0_seconds
    }
    
    # Store the final result of the run (after all v0 iterations)
    results[[run]] <- result
    log_liks[run] <- result$likelihood  # Store the final log-likelihood of the run
    
    # Print total time for the current run
    print(paste("Time taken for run", run, ":", round(run_total_time, 2), "seconds"))
    
    # Add run time to the overall total time
    total_time <- total_time + run_total_time
  }
  
  # Additional run with PCA initialization and DPE
  print("Run with PCA initialisation:")
  B <- NULL  # Ensure B is NULL initially
  pca_total_time <- 0  # Initialize time accumulator for PCA run
  
  for (v0_idx in seq_along(v0_values)) {
    # Measure the time for each PCA-based v0 iteration
    v0_time <- system.time({
      print(paste("  PCA Iteration with v0 =", v0_values[v0_idx]))
      
      if (is.null(dim1) & is.null(dim2) & is.null(dim3)) {
        result_pca <- em_function(data$Y, q, v0 = v0_values[v0_idx], B = B, max_iter = max_iter, ll = ll, PCA = TRUE)
      } else if (is.null(dim3)) {
        result_pca <- em_function(data$Y, q, dim1 = dim1, dim2 = dim2, v0 = v0_values[v0_idx], B = B, max_iter = max_iter, ll = ll, PCA = TRUE)
      } else {
        result_pca <- em_function(data$Y, q, dim1 = dim1, dim2 = dim2, dim3 = dim3, v0 = v0_values[v0_idx], B = B, max_iter = max_iter, ll = ll, PCA = TRUE)
      }
      
      # Update B for the next PCA-based iteration
      B <- result_pca$B
    })
    
    # Print the time taken for the current PCA-based v0 iteration
    v0_seconds <- round(v0_time[3], 2)
    print(paste("    Time taken for PCA v0 =", v0_values[v0_idx], ":", v0_seconds, "seconds"))
    
    # Accumulate total time for the PCA run
    pca_total_time <- pca_total_time + v0_seconds
  }
  
  # Store the final PCA result
  results[[num_runs + 1]] <- result_pca
  log_liks[num_runs + 1] <- result_pca$likelihood  # Store the PCA log-likelihood
  
  # Print total time for the PCA run
  print(paste("Time taken for PCA run:", round(pca_total_time, 2), "seconds"))
  
  # Add PCA run time to the overall total time
  total_time <- total_time + pca_total_time
  
  # Print total runtime for all runs
  print(paste("Total time for all runs:", round(total_time, 2), "seconds"))
  
  # Select the best result based on log-likelihood
  best_run <- which.max(log_liks)
  best_result <- results[[best_run]]
  
  # Check if PCA initialization gave the best result
  if (best_run == num_runs + 1) {
    pca_best_count <- 1  # Increment the PCA best count
  }
  
  print(paste("Best run:", best_run, "with log-likelihood:", log_liks[best_run]))
  
  return(list(results = results, best_result = best_result, 
              total_time = total_time, pca_best = pca_best_count))
}
