# Only change directory for results in this file and data generators/dimensions/iterations
# Load necessary scripts and libraries
source("Scripts/data_generation.R")
lapply(list.files("Scripts/algorithms", pattern = "\\.R$", full.names = TRUE), source)
source("Scripts/run_simulations.R")

# Command line arguments
args <- commandArgs(trailingOnly = TRUE)
data_generator_name <- args[1]  # Name of the data generator
simulation_index <- as.integer(args[2])  # Index of the simulation

# Run the simulation
set.seed(simulation_index)  # Ensure different seeds for different runs

# Parameters
n <- 500 
q <- 3
dim1 <- 20
dim2 <- 20
print_factors <- FALSE
max_iter <- 15000
tol <- 1e-2
ll <- FALSE
num_runs <- 10

# List of datasets to generate and their descriptions 
load("data_generators_new.RData")
#load("data_generators_old.RData")

# Find the right generator
generator <- NULL
for (gen in data_generators) {
  if (gen$name == data_generator_name) {
    generator <- gen$generator
    break
  }
}

if (is.null(generator)) {
  stop("Invalid data generator name.")
}

results <- run_simulations(generator, 1)

# Create subfolder if it doesn't exist
output_dir <- file.path("/well/nichols/users/rht383/results_20x20", data_generator_name)
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Save the results to a file in the subfolder
output_file <- file.path(output_dir, paste0("results_sim_", simulation_index, ".rds"))
saveRDS(results, file = output_file)
