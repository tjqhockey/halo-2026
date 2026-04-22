# Fit a Bayesian version of the RAPM model
# This file does xTA
library(rstan)
library(here)

# Read design matrix and separate into X and y
xt_matrix <- readRDS(here('data', 'datasets', 'xt_matrix.rds'))
X_xt <- xt_matrix[, -1]
y_xt <- xt_matrix[, 1]

# Construct data list
bayes_xTA_data <- list(N_shifts = nrow(X_xt),
                       N_players = ncol(X_xt),
                       y = y_xt,
                       X_players = X_xt)

# Run stan code
bayes_ridge_rapm_fit <- stan(file = "BayesianRAPM_xTA.stan", 
                             data = bayes_xTA_data, 
                             chains = 4, iter = 2500 * 2, 
                             cores = 4,
                             seed = 2024)

# Save the model fit
write_rds(bayes_ridge_rapm_fit, here("xTA_fit.rds"))