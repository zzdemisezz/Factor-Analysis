# Function to run simulations for a specific data generator
run_simulations <- function(generator, num_simulations) {
  all_datasets <- vector("list", num_simulations)
  all_results_em <- vector("list", num_simulations)
  all_results_em_beta <- vector("list", num_simulations)
  all_results_em_pxl <- vector("list", num_simulations)  # Store results for em_pxl
  
  pca_best_count_em <- 0  # Initialize counter for PCA best results in em
  pca_best_count_em_beta <- 0  # Initialize counter for PCA best results in em_beta
  pca_best_count_em_pxl <- 0  # Initialize counter for PCA best results in em_pxl
  
  total_em_time <- 0  # Initialize total time for EM algorithm
  total_em_beta_time <- 0  # Initialize total time for EM_BETA algorithm
  total_em_pxl_time <- 0  # Initialize total time for EM_PXL algorithm
  
  total_simulation_time <- 0  # Initialize total simulation time
  
  total_simulation_time <- system.time({
    for (sim in 1:num_simulations) {
      print(paste("Simulation", sim))
      
      # Generate new data for each simulation run
      data <- generator()  # Call the generator function to get the data
      all_datasets[[sim]] <- data  # Save the generated dataset
      
      # Run and time the EM algorithm
      total_em_time <- total_em_time + system.time({
        results_em <- run_em_algorithm(em, data, q, dim1, dim2, tol = tol, 
                                       max_iter = max_iter, ll = ll, num_runs = num_runs)
      })[3]
      
      # Run and time the EM_BETA algorithm
      total_em_beta_time <- total_em_beta_time + system.time({
        results_em_beta <- run_em_algorithm(em_beta, data, q, tol = tol, max_iter = max_iter, 
                                            ll = ll, num_runs = num_runs)
      })[3]
      
      # Run and time the EM_PXL algorithm
      total_em_pxl_time <- total_em_pxl_time + system.time({
        results_em_pxl <- run_em_algorithm(em_pxl, data, q, dim1, dim2, tol = tol, 
                                           max_iter = max_iter, ll = ll, num_runs = num_runs)
      })[3]
      
      pca_best_count_em <- pca_best_count_em + results_em$pca_best
      pca_best_count_em_beta <- pca_best_count_em_beta + results_em_beta$pca_best
      pca_best_count_em_pxl <- pca_best_count_em_pxl + results_em_pxl$pca_best  
      
      # Save the results for each simulation
      all_results_em[[sim]] <- results_em
      all_results_em_beta[[sim]] <- results_em_beta
      all_results_em_pxl[[sim]] <- results_em_pxl  
    }
  })[3]  # The third element of the system.time() result is the elapsed time
  
  return(list(
    all_results_em = all_results_em,
    all_results_em_beta = all_results_em_beta,
    all_results_em_pxl = all_results_em_pxl,  
    all_datasets = all_datasets,  # Return the list of datasets
    pca_best_count_em = pca_best_count_em,
    pca_best_count_em_beta = pca_best_count_em_beta,
    pca_best_count_em_pxl = pca_best_count_em_pxl,  
    total_em_time = total_em_time,           
    total_em_beta_time = total_em_beta_time, 
    total_em_pxl_time = total_em_pxl_time,    
    total_simulation_time = total_simulation_time
  ))
}
