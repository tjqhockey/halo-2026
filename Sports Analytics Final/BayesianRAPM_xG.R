# Fit a Bayesian version of the RAPM model
# This file does xG
library(rstan)
library(here)
library(tidyverse)

# Read design matrix and separate into X and y
xg_matrix <- readRDS(here('data', 'datasets', 'xg_matrix.rds'))
X_xg <- xg_matrix[, -1]
y_xg <- xg_matrix[, 1]

# Construct data list
bayes_xG_data <- list(N_shifts = nrow(X_xg),
                       N_players = ncol(X_xg),
                       y = y_xg,
                       X_players = X_xg)

# Run stan code
bayes_ridge_rapm_fit <- stan(file = "BayesianRAPM_xG.stan", 
                             data = bayes_xG_data, 
                             chains = 4, iter = 2500 * 2, 
                             cores = 4,
                             seed = 2024)

# Save the model fit
write_rds(bayes_ridge_rapm_fit, here("data", "datasets", "xG_fit.rds"),
          compress = "gz")

# Save the posterior samples
rapm_posterior_samples <- as.data.frame(bayes_ridge_rapm_fit, 
                                        pars = "lp__", include = FALSE) |>
  as_tibble()

write_csv(rapm_posterior_samples, here("data", "datasets", "xG_posterior.rds"))
