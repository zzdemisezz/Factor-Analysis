# Install argparse if not already installed
if (!require(argparse)) {
  install.packages("argparse")
  library(argparse)
}

source("Scripts/data_generation.R")
source("Scripts/em.R")
source("Scripts/em_beta.R")

# Initialize the ArgumentParser
parser <- ArgumentParser(description = 'Process simulation parameters.')

# Add arguments with default values
parser$add_argument('--n', type = 'integer', default = 200, help = 'Number of samples (default: 200)')
parser$add_argument('--q', type = 'integer', default = 3, help = 'Number of features (default: 3)')
parser$add_argument('--dim1', type = 'integer', default = 10, help = 'Dimension 1 (default: 10)')
parser$add_argument('--dim2', type = 'integer', default = 10, help = 'Dimension 2 (default: 10)')
parser$add_argument('--max_iter', type = 'integer', default = 2000, help = 'Maximum number of iterations (default: 2000)')
parser$add_argument('--ll', action = 'store_true', default = FALSE, help = 'Enable or disable log-likelihood computation (default: FALSE)')
parser$add_argument('--num_runs', type = 'integer', default = 5, help = 'Number of runs (default: 5)')
parser$add_argument('--num_simulations', type = 'integer', default = 5, help = 'Number of simulations (default: 5)')

# Parse the command line arguments
args <- parser$parse_args()

# Access the arguments
print(paste("n:", args$n))
print(paste("q:", args$q))
print(paste("dim1:", args$dim1))
print(paste("dim2:", args$dim2))
print(paste("max_iter:", args$max_iter))
print(paste("ll:", args$ll))
print(paste("num_runs:", args$num_runs))
print(paste("num_simulations:", args$num_simulations))

# initialisation
set.seed(12)

# Function to run algorithms
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

# Run the simulations and save results
all_results_em <- vector("list", num_simulations)
all_results_em_beta <- vector("list", num_simulations)
pca_best_count_em <- 0  # Initialize counter for PCA best results in em
pca_best_count_em_beta <- 0  # Initialize counter for PCA best results in em_beta
total_simulation_time <- 0  # Initialize total simulation time
total_simulation_time <- system.time({
  for (sim in 1:num_simulations) {
    print(paste("Simulation", sim))
    
    # Generate new data for each simulation run
    data <- generate_data(n, dim1, dim2, q, 4, print_factors = FALSE)
    
    # Run the EM algorithm
    results_em <- run_em_algorithm(em, data, q, dim1, dim2, tol = 1e-2, 
                                   max_iter = max_iter, ll = ll, num_runs = num_runs)
    results_em_beta <- run_em_algorithm(em_beta, data, q, tol = 1e-2, max_iter = max_iter, 
                                        ll = ll, num_runs = num_runs)
    
    pca_best_count_em <- pca_best_count_em + results_em$pca_best
    pca_best_count_em_beta <- pca_best_count_em_beta + results_em_beta$pca_best
    
    # Save the results for each simulation
    all_results_em[[sim]] <- results_em
    all_results_em_beta[[sim]] <- results_em_beta
  }
})[3]  # The third element of the system.time() result is the elapsed time


print(paste("Number of times PCA initialization gave the best result for EM:", pca_best_count_em))
print(paste("Number of times PCA initialization gave the best result for EM_Beta:", pca_best_count_em_beta))
print(paste("Total time for all simulations:", round(total_simulation_time, 2), "seconds"))


