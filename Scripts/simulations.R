source("Scripts/data_generation.R")
source("Scripts/em.R")
source("Scripts/em_beta.R")
source("Scripts/run_em.R")
source("Scripts/run_simulations.R")

# Set seed
set.seed(12)

# Parameters
n <- 500 
q <- 3
dim1 <- 10
dim2 <- 10
print_factors <- FALSE
max_iter <- 5000
tol <- 1e-2
ll <- FALSE
num_runs <- 5
num_simulations <- 100

# List of datasets to generate and their descriptions
data_generators <- list(
  list(name = "SmallFactors_3x3_Strong", generator = function() generate_data(n, dim1, dim2, q, 3, corr = "strong", print_factors = print_factors)),
  list(name = "SmallFactors_3x3_Moderate", generator = function() generate_data(n, dim1, dim2, q, 3, corr = "moderate", print_factors = print_factors)),
  list(name = "SmallFactors_3x3_Weak", generator = function() generate_data(n, dim1, dim2, q, 3, corr = "weak", print_factors = print_factors)),
  
  list(name = "LargeFactors_5x5_Strong", generator = function() generate_data(n, dim1, dim2, q, 5, corr = "strong", print_factors = print_factors)),
  list(name = "LargeFactors_5x5_Moderate", generator = function() generate_data(n, dim1, dim2, q, 5, corr = "moderate", print_factors = print_factors)),
  list(name = "LargeFactors_5x5_Weak", generator = function() generate_data(n, dim1, dim2, q, 5, corr = "weak", print_factors = print_factors)),
  
  list(name = "Overlap2_Small_Strong", generator = function() generate_data_2overlap(n, dim1, dim2, q, overlap = "small", corr = "strong", print_factors = print_factors)),
  list(name = "Overlap2_Small_Moderate", generator = function() generate_data_2overlap(n, dim1, dim2, q, overlap = "small", corr = "moderate", print_factors = print_factors)),
  list(name = "Overlap2_Small_Weak", generator = function() generate_data_2overlap(n, dim1, dim2, q, overlap = "small", corr = "weak", print_factors = print_factors)),
  
  list(name = "Overlap2_Large_Strong", generator = function() generate_data_2overlap(n, dim1, dim2, q, overlap = "big", corr = "strong", print_factors = print_factors)),
  list(name = "Overlap2_Large_Moderate", generator = function() generate_data_2overlap(n, dim1, dim2, q, overlap = "big", corr = "moderate", print_factors = print_factors)),
  list(name = "Overlap2_Large_Weak", generator = function() generate_data_2overlap(n, dim1, dim2, q, overlap = "big", corr = "weak", print_factors = print_factors)),
  
  list(name = "Overlap3_Small_Strong", generator = function() generate_data_3overlap(n, dim1, dim2, q, overlap = "small", corr = "strong", print_factors = print_factors)),
  list(name = "Overlap3_Small_Moderate", generator = function() generate_data_3overlap(n, dim1, dim2, q, overlap = "small", corr = "moderate", print_factors = print_factors)),
  list(name = "Overlap3_Small_Weak", generator = function() generate_data_3overlap(n, dim1, dim2, q, overlap = "small", corr = "weak", print_factors = print_factors)),
  
  list(name = "Overlap3_Large_Strong", generator = function() generate_data_3overlap(n, dim1, dim2, q, overlap = "big", corr = "strong", print_factors = print_factors)),
  list(name = "Overlap3_Large_Moderate", generator = function() generate_data_3overlap(n, dim1, dim2, q, overlap = "big", corr = "moderate", print_factors = print_factors)),
  list(name = "Overlap3_Large_Weak", generator = function() generate_data_3overlap(n, dim1, dim2, q, overlap = "big", corr = "weak", print_factors = print_factors))
)


# Run simulations for each data structure
all_simulation_results <- list()

for (gen in data_generators) {
  print(paste("Running simulations for:", gen$name))
  
  results <- run_simulations(gen$generator, num_simulations)
  
  all_simulation_results[[gen$name]] <- results
}