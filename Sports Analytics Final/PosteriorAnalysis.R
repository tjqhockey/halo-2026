library(here)
library(tidyverse)
library(bayesplot)

# xTA trace plot
xTA_fit <- read_rds(here("data", "datasets", "xTA_fit.rds"))
mcmc_trace(xTA_fit, pars = c("sigma_shifts", "sigma_players"))
# sigma_players may not be that well-explored, Rhat is 1.01 and effective sample size is small

# xG trace plot
xG_fit <- read_rds(here("data", "datasets", "xG_fit.rds"))
mcmc_trace(xG_fit, pars = c("sigma_shifts", "sigma_players"))

# Read xTA posterior samples
xt_posterior_samples <- read_csv(here("data", "datasets", "xTA_posterior.rds"))
xg_posterior_samples <- read_csv(here("data", "datasets", "xG_posterior.rds"))

# Add player names to posterior samples
players <- here('data', 'datasets', 'players.parquet') |>
  arrow::read_parquet()
xt_matrix <- readRDS(here('data', 'datasets', 'xt_matrix.rds'))[, -1]
xg_matrix <- readRDS(here('data', 'datasets', 'xg_matrix.rds'))[, -1]

player_order_xt <- tibble(player_id = colnames(xt_matrix)) |>
  inner_join(players, by = "player_id") |>
  pull(player_name)

player_order_xg <- tibble(player_id = colnames(xg_matrix)) |>
  inner_join(players, by = "player_id") |>
  pull(player_name)

long_posterior_samples_xt <- xt_posterior_samples |>
  select(starts_with("beta")) |>
  mutate() |>
  t() |>
  as.data.frame() |>
  mutate(player_name = player_order_xt) |>
  pivot_longer(cols = -player_name,
               names_to = "draw",
               values_to = "beta")

long_posterior_samples_xg <- xg_posterior_samples |>
  select(starts_with("beta")) |>
  mutate() |>
  t() |>
  as.data.frame() |>
  mutate(player_name = player_order_xg) |>
  pivot_longer(cols = -player_name,
               names_to = "draw",
               values_to = "beta")

# Summarize the posterior samples, 80% credible intervals
posterior_ratings_xt <- long_posterior_samples_xt |>
  group_by(player_name) |>
  summarize(mean = mean(beta),
            median = median(beta),
            lower_80 = quantile(beta, 0.1),
            upper_80 = quantile(beta, 0.9))

posterior_ratings_xg <- long_posterior_samples_xg |>
  group_by(player_name) |>
  summarize(mean = mean(beta),
            median = median(beta),
            lower_80 = quantile(beta, 0.1),
            upper_80 = quantile(beta, 0.9))

# Export the posterior summaries for Tyler and Alex's models
write_csv(posterior_ratings_xg, here('data', 'datasets', 'posterior_ratings_xg.csv'))

# Top and bottom 10 xTA coefficients plot
library(ggridges)
top10_xt <- posterior_ratings_xt |>
  slice_max(mean, n = 10) |>
  rbind(posterior_ratings_xt |> slice_min(mean, n = 10))
p1 <- long_posterior_samples_xt |>
  inner_join(top10_xt, by = "player_name") |>
  select(-mean) |>
  ggplot(aes(x = beta, y = reorder(player_name, beta, FUN = mean))) +
  geom_density_ridges() +
  labs(
    x = "xT Added Coefficient",
    y = "Player Name",
    title = "Posterior Distributions of xT Added Model",
    subtitle = "Top and Bottom 10 Players by Posterior Mean"
  ) +
  theme_bw()

# Top 10 xG coefficients plot
top10_xg <- posterior_ratings_xg |>
  slice_max(mean, n = 10) |>
  rbind(posterior_ratings_xg |> slice_min(mean, n = 10))
p2 <- long_posterior_samples_xg |>
  inner_join(top10_xg, by = "player_name") |>
  select(-mean) |>
  ggplot(aes(x = beta, y = reorder(player_name, beta, FUN = mean))) +
  geom_density_ridges() +
  labs(
    x = "xG Coefficient",
    y = "Player Name",
    title = "Posterior Distributions of xG Model",
    subtitle = "Top and Bottom 10 Players by Posterior Mean"
  ) +
  theme_bw()

library(patchwork)
p1 + p2
