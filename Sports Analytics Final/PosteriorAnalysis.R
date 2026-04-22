library(here)
library(tidyverse)
library(bayesplot)

# xTA trace plot
mcmc_trace(bayes_ridge_rapm_fit, pars = c("sigma_shifts", "sigma_players"))

# Read xTA posterior samples
xt_posterior_samples <- read_csv(here("data", "datasets", "xTA_posterior.rds"))

# Add player names to posterior samples
players <- here('data', 'datasets', 'players.parquet') |>
  arrow::read_parquet()
xt_matrix <- readRDS(here('data', 'datasets', 'xt_matrix.rds'))[, -1]

player_order <- tibble(player_id = colnames(xt_matrix)) |>
  inner_join(players, by = "player_id") |>
  pull(player_name)

long_posterior_samples <- xt_posterior_samples |>
  select(starts_with("beta")) |>
  mutate() |>
  t() |>
  as.data.frame() |>
  mutate(player_name = player_order) |>
  pivot_longer(cols = -player_name,
               names_to = "draw",
               values_to = "beta")

# Summarize the posterior samples, 80% credible intervals
posterior_ratings <- long_posterior_samples |>
  group_by(player_name) |>
  summarize(mean = mean(beta),
            median = median(beta),
            lower_80 = quantile(beta, 0.1),
            upper_80 = quantile(beta, 0.9))
