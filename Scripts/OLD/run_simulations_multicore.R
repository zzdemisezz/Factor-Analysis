library(parallel)

# Function to run simulations for a specific data generator
run_simulations <- function(generator, num_simulations, num_cores = detectCores()) {
  all_results_em <- vector("list", num_simulations)
  all_results_em_beta <- vector("list", num_simulations)
  
  # Function to run a single simulation
  run_single_simulation <- function(sim) {
    print(paste("Simulation", sim))
    
    # Generate new data for each simulation run
    data <- generator()  # Call the generator function to get the data
    
    # Run the EM algorithm
    results_em <- run_em_algorithm(em, data, q, dim1, dim2, tol = tol, 
                                   max_iter = max_iter, ll = ll, num_runs = num_runs)
    results_em_beta <- run_em_algorithm(em_beta, data, q, tol = tol, max_iter = max_iter, 
                                        ll = ll, num_runs = num_runs)
    
    return(list(results_em = results_em, results_em_beta = results_em_beta))
  }
  
  # Run simulations in parallel
  total_simulation_time <- system.time({
    results_list <- mclapply(1:num_simulations, run_single_simulation, mc.cores = num_cores)
  })[3]  # The third element of the system.time() result is the elapsed time
  
  # Combine the results
  for (sim in 1:num_simulations) {
    all_results_em[[sim]] <- results_list[[sim]]$results_em
    all_results_em_beta[[sim]] <- results_list[[sim]]$results_em_beta
  }
  
  # Count PCA best results
  pca_best_count_em <- sum(sapply(all_results_em, function(x) x$pca_best))
  pca_best_count_em_beta <- sum(sapply(all_results_em_beta, function(x) x$pca_best))
  
  return(list(
    all_results_em = all_results_em,
    all_results_em_beta = all_results_em_beta,
    pca_best_count_em = pca_best_count_em,
    pca_best_count_em_beta = pca_best_count_em_beta,
    total_simulation_time = total_simulation_time
  ))
}
