# Fit RAPM, ridge regression, with cumulative net xT and xG for each stint
# I've already built the design matrices, read them in
library(tidyverse)
library(here)

xt_matrix <- readRDS(here('data', 'datasets', 'xt_matrix.rds'))
xg_matrix <- readRDS(here('data', 'datasets', 'xg_matrix.rds'))
# Player table for joining names to IDs
players <- here('data', 'datasets', 'players.parquet') |>
  arrow::read_parquet()

# Extract X and y from matrices
X_xt <- xt_matrix[, -1]
y_xt <- xt_matrix[, 1]
X_xg <- xg_matrix[, -1]
y_xg <- xg_matrix[, 1]

# Fit RAPM models
# Note that standardize = FALSE treats power plays as the same as even strength
# Intercept = TRUE gives home field advantage
library(glmnet)
set.seed(21)
xt_ridge <- cv.glmnet(X_xt, y_xt, alpha = 0, intercept = TRUE, standardize = FALSE)
set.seed(21)
xg_ridge <- cv.glmnet(X_xg, y_xg, alpha = 0, intercept = TRUE, standardize = FALSE)

# Save models
saveRDS(xt_ridge, here('data', 'datasets', 'xt_ridge.rds'))
saveRDS(xg_ridge, here('data', 'datasets', 'xg_ridge.rds'))

# Get player coefficients
library(broom)
xt_coef <- tidy(xt_ridge$glmnet.fit) |>
  filter(lambda == xt_ridge$lambda.min) |>
  rename(player_id = term) |>
  full_join(players, by = ("player_id")) |>
  select(player_id, player_name, estimate)
xg_coef <- tidy(xg_ridge$glmnet.fit) |>
  filter(lambda == xg_ridge$lambda.min) |>
  rename(player_id = term) |>
  full_join(players, by = ("player_id")) |>
  select(player_id, player_name, estimate)

# Join them together to compare
coefs <- inner_join(xt_coef, xg_coef, by = c("player_id", "player_name")) |>
  rename(xt_estimate = estimate.x,
         xg_estimate = estimate.y)

# Comparison scatter plot
plot <- coefs |>
  ggplot(aes(x = xt_estimate, y = xg_estimate)) +
  geom_point() +
  labs(
    x = "RAPM Coefficients with y = xT",
    y = "RAPM Coefficients with y = xG",
    title = "Using xT and xG as the response variable to build RAPM leads to similar results."
  ) +
  theme_bw()
  
plot

saveRDS(plot, here('data', 'datasets', 'avery_hw5.rds'))
