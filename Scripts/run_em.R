run_em_algorithm <- function(em_function, data, q, dim1 = NULL, dim2 = NULL, 
                             dim3 = NULL, tol = 1e-3, max_iter = 1000, 
                             ll = TRUE, num_runs = 5) {
  results <- vector("list", num_runs + 1)
  log_liks <- numeric(num_runs + 1)
  total_time <- 0
  pca_best_count <- 0  # Counter for PCA best results
  
  for (i in 1:num_runs) {
    print(paste("Run", i))
    
    # Measure the time taken for each run
    run_time <- system.time({
      if (is.null(dim1) & is.null(dim2) & is.null(dim3)) {
        result <- em_function(data$Y, q, max_iter = max_iter, ll = ll)
      } else if (is.null(dim3)) {
        result <- em_function(data$Y, q, dim1 = dim1, dim2 = dim2, 
                              max_iter = max_iter, ll = ll)
      } else {
        result <- em_function(data$Y, q, dim1 = dim1, dim2 = dim2, dim3 = dim3, 
                              max_iter = max_iter, ll = ll)
      }
    })
    
    # Store the results and log-likelihood
    results[[i]] <- result
    log_liks[i] <- result$likelihood
    
    # Print the time taken for the run
    run_seconds <- round(run_time[3], 2)
    print(paste("Time taken for run", i, ":", run_seconds, "seconds"))
    
    # Accumulate total time
    total_time <- total_time + run_seconds
  }
  
  # Additional run with PCA = TRUE
  print("Run with PCA initialisation:")
  run_time <- system.time({
    if (is.null(dim1) & is.null(dim2) & is.null(dim3)) {
      result_pca <- em_function(data$Y, q, max_iter = max_iter, ll = ll, PCA = TRUE)
    } else if (is.null(dim3)) {
      result_pca <- em_function(data$Y, q, dim1 = dim1, dim2 = dim2, 
                                max_iter = max_iter, ll = ll, PCA = TRUE)
    } else {
      result_pca <- em_function(data$Y, q, dim1 = dim1, dim2 = dim2, dim3 = dim3, 
                                max_iter = max_iter, ll = ll, PCA = TRUE)
    }
  })
  
  # Store the PCA result and log-likelihood
  results[[num_runs + 1]] <- result_pca
  log_liks[num_runs + 1] <- result_pca$likelihood
  
  # Print the time taken for the PCA run
  run_seconds <- round(run_time[3], 2)
  print(paste("Time taken for PCA run:", run_seconds, "seconds"))
  
  # Accumulate total time
  total_time <- total_time + run_seconds
  
  # Print total runtime
  print(paste("Total time for all runs:", round(total_time, 2), "seconds"))
  
  # Select the best result based on log-likelihood
  best_run <- which.max(log_liks)
  best_result <- results[[best_run]]
  
  # Check if PCA initialization gave the best result
  if (best_run == num_runs + 1) {
    pca_best_count <- 1  # Increment the PCA best count
  }
  
  return(list(results = results, best_result = best_result, 
              pca_best = pca_best_count, total_time = total_time))
}