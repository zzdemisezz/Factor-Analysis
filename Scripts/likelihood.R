# Check if the 'mvtnorm' package is installed
if (!requireNamespace("mvtnorm", quietly = TRUE)) {
  install.packages("Scripts/mvtnorm_1.2-6.tar.gz", repos = NULL, type="source")
}

# Check if the 'invgamma' package is installed
if (!requireNamespace("invgamma", quietly = TRUE)) {
  install.packages("Scripts/invgamma_1.1.tar.gz", repos = NULL, type="source")
}

# Load the libraries
library(mvtnorm)
library(invgamma)

source("Scripts/MCAR_Functions.R")

# Define the sigmoid function
sigmoid <- function(x) {
  1 / (1 + exp(-x))
}

# Define finite sum
sum.finite <- function(x) {
  sum(x[is.finite(x)])
}

# Likelihood with spike-and-slab prior
likelihood <- function(Y, B, PSI, GAMMA, v0, v1){
  n <- nrow(Y)
  p <- ncol(Y)
  k <- ncol(B)
  
  # Compute the log-likelihood term
  Sigma <- B %*% t(B) + PSI
  ll <- sum.finite(dmvnorm(Y, sigma = Sigma, log = TRUE))
  
  # Log prior term for B 
  variances <- v0 * (1 - GAMMA) + v1 * GAMMA
  ll_B_prior <- sum.finite(dnorm(as.vector(B), mean = 0, sd = sqrt(as.vector(variances)), log = TRUE))
  
  # Add log prior term for PSI
  alpha <- 0.5
  beta <- 0.5
  ll_PSI_prior <- sum.finite(dinvgamma(diag(PSI), alpha, beta, log = TRUE))
  
  return(ll + ll_B_prior + ll_PSI_prior)
}


# Likelihood with standard normal prior
likelihood_normal <- function(Y, B, PSI) {
  n <- nrow(Y)
  p <- ncol(Y)
  q <- ncol(B)
  
  # Compute the log-likelihood term for Y using built-in functions
  Sigma <- B %*% t(B) + PSI
  ll_Y <- sum(dmvnorm(Y, sigma = Sigma, log = TRUE))
  # print(ll_Y)
  
  # Log prior term for B using dnorm
  ll_B_prior <- sum(dnorm(as.vector(B), mean = 0, sd = 1, log = TRUE))
  # print(ll_B_prior)
  
  # Log prior term for PSI using dinvgamma
  alpha <- 0.5
  beta <- 0.5
  ll_PSI_prior <- sum(dinvgamma(diag(PSI), shape = alpha, rate = beta, log = TRUE))
  # print(ll_PSI_prior)
  
  return(sum.finite(ll_Y + ll_B_prior + ll_PSI_prior))
}