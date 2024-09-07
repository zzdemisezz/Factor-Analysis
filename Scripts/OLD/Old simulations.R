source("Scripts/data_generation.R")
source("Scripts/em.R")
source("Scripts/em_beta.R")
source("Scripts/run_em.R")

# set seed
set.seed(12)

# Parameters
n <- 500 
q <- 3
dim1 <- 10
dim2 <- 10
print_factors = FALSE
max_iter <- 5000
tol = 1e-2
ll <- FALSE
num_runs <- 5
num_simulations <- 5

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
    data <- generate_data(n, dim1, dim2, q, 3, corr = "strong", 
                          print_factors = print_factors)
    
    # Run the EM algorithm
    results_em <- run_em_algorithm(em, data, q, dim1, dim2, tol = tol, 
                                   max_iter = max_iter, ll = ll, num_runs = num_runs)
    results_em_beta <- run_em_algorithm(em_beta, data, q, tol = tol, max_iter = max_iter, 
                                        ll = ll, num_runs = num_runs)
    
    pca_best_count_em <- pca_best_count_em + results_em$pca_best
    pca_best_count_em_beta <- pca_best_count_em_beta + results_em_beta$pca_best
    
    # Save the results for each simulation
    all_results_em[[sim]] <- results_em
    all_results_em_beta[[sim]] <- results_em_beta
  }
})[3]  # The third element of the system.time() result is the elapsed time

data2 <- generate_data(n, dim1, dim2, q, 5, corr = "strong",
                       print_factors = print_factors)

data3 <- generate_data_2overlap(n, dim1, dim2, q, overlap = "small", 
                                corr = "strong", print_factors = print_factors)
data4 <- generate_data_2overlap(n, dim1, dim2, q, overlap = "big", 
                                corr = "strong", print_factors = print_factors)
data7 <- generate_data_3overlap(n, dim1, dim2, q, overlap = "small", 
                                corr = "strong", print_factors = print_factors)
data8 <- generate_data_3overlap(n, dim1, dim2, q, overlap = "big", 
                                corr = "strong", print_factors = print_factors)
