rm(list = ls())
source("Scripts/data_generation.R")
source("Scripts/em.R")
source("Scripts/em_beta.R")
source("Scripts/em_beta_pxl.R")
source("Scripts/em_pxl.R")
source("Scripts/run_em.R")
source("Scripts/em_pxl_DPE.R")
source("Scripts/run_em_DPE.R")


# set seed
set.seed(42)

# Parameters
n <- 500 
q <- 3
dim1 <- 10
dim2 <- 10
max_iter <- 5000
tol = 1e-2
ll <- TRUE
num_runs <- 10

# Generate data
# data <- generate_data_simple(n, dim1, dim2, q, "weak", TRUE)
data <- generate_data(n, dim1, dim2, q, 5, corr = "strong", print_factors = FALSE)
# data <- generate_data_2overlap(n, dim1, dim2, q, overlap = "big",
#                                corr = "weak", print_factors = TRUE)
# data <- generate_data_3overlap(n, dim1, dim2, q, overlap = "big",
#                                corr = "weak", print_factors = FALSE)

# View(data$cor_matrix)

# output <- run_em_algorithm_DPE(em_pxl_DPE, data, q, dim1, dim2, tol = tol,
#                                 max_iter = max_iter, ll = ll, num_runs = num_runs)
# output <- run_em_algorithm(em_pxl, data, q, dim1, dim2, tol = tol,
#                                max_iter = max_iter, ll = ll, num_runs = num_runs)
output <- run_em_algorithm(em_beta, data, q, tol = tol,
                                max_iter = max_iter, ll = ll, num_runs = num_runs)

stop()
# # check specific initilialisation
best_result <- output$best_result
output$results[[11]]$GAMMA_truncated
best_result$likelihood
best_result$GAMMA_truncated
stop()
best_result$B_truncated

non_truncated <- mean((data$B_true - best_result$B)^2)
truncated <- mean((data$B_true - best_result$B_truncated)^2)
print(non_truncated)
print(truncated)

best_result$likelihood
best_result$converged
best_result$iter
best_result$B
best_result$GAMMA
best_result$GAMMA_truncated
View(best_result$Covariance_matrix)
View(best_result$Covariance_matrix_truncated)
View(cov2cor(best_result$Covariance_matrix_truncated))
View(data$Covariance_matrix_true)
View(data$cor_matrix)
data$B_true
output$total_time
output$pca_best

# output2 <- run_em_algorithm(em_beta, data, q, tol = tol,
#                                 max_iter = max_iter, ll = ll, num_runs = num_runs)
# best_result2 <- output2$best_result
# 
# 
# non_truncated <- mean((data$B_true - best_result2$B)^2)
# truncated <- mean((data$B_true - best_result2$B_truncated)^2)
# print(non_truncated)
# print(truncated)
# 
# best_result2$likelihood
# best_result2$converged
# best_result2$iter
# best_result2$B
# best_result2$B_truncated
# best_result2$GAMMA
# best_result2$GAMMA_truncated
# data$B_true

# ignore ####
# print_results <- function(data, best_result, model_name) {
#   # Compare loadings
#   compare_B <- sqrt(mean((data$B_true - best_result$B)^2))
#   cat("\nModel:", model_name)
#   cat("\nRMSE for B:", compare_B, "\n")
#   
#   # # Compare variances
#   # compare_PSI <- sqrt(mean((diag(data$PSI_true) - diag(best_result$PSI))^2))
#   # cat("RMSE for PSI:", compare_PSI, "\n")
#   
#   # # Shows loadings
#   # cat("\nBest Estimated B:\n")
#   # print(best_result$B)
#   # # Shows Gamma
#   # if (!is.null(best_result$GAMMA)) {
#   #   cat("\nBest Estimated GAMMA:\n")
#   #   print(best_result$GAMMA)
#   # }
#   # # Shows iterations and convergence
#   # cat("\nNumber of iterations in best run:\n")
#   # print(best_result$iter)
#   # cat("\nConverged in best run:\n")
#   # print(best_result$converged)
# }
