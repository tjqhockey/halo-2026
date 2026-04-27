library(rstan)
library(here)
library(tidyverse)
library(posterior)
players <- here('data', 'datasets', 'players.parquet') |>
  arrow::read_parquet()

design_matrix <- readRDS(here('data', 'datasets', 'special_teams_design.rds'))
X <- as.matrix(design_matrix |>
                 select(-c(game_id,period,game_stint,
                           stint_length, y_xG))
)
y <- design_matrix$y_xG
wgts <- design_matrix$stint_length
even_strength_posteriors <- read_csv(here('data', 'datasets',
                                          'posterior_ratings_xg.csv')) |>
  left_join(players)

all_cols <- colnames(X)
player_means <- rep(0, length(all_cols))
for (i in seq_along(all_cols)) {
  current_col <- all_cols[i]
  
  # Remove the _PP or _PK suffix to get just the player identifier
  # This regex removes everything from the last underscore to the end
  clean_id <- sub("(_PP|_PK)$", "", current_col)
  
  # Check if this clean_id exists in your ES posteriors
  # (Make sure 'player_id' or 'player_name' in the CSV matches 'clean_id')
  match_idx <- which(even_strength_posteriors$player_id == clean_id)
  
  if (length(match_idx) > 0) {
    # If found, assign the ES mean as the prior
    player_means[i] <- even_strength_posteriors$mean[match_idx]
  } else {
    # If not found (like n_pp_skaters), it stays 0
    player_means[i] <- 0
  }
}

bayes_xG_data <- list(N_shifts = nrow(X),
                      N_players = ncol(X),
                      y = y,
                      X_players = X,
                      wgts = wgts,
                      prior_means = player_means)

## Running stan part (PLEASE WORK)
# bayes_st_rapm_fit <- stan(file = "Sports Analytics Final//special_teams_bayesian.stan", 
#                              data = bayes_xG_data, 
#                              chains = 4, iter = 2500 * 2, 
#                              cores = 10,
#                              seed = 2024)

# Save the posterior samples
st_posterior_samples <- as.data.frame(bayes_st_rapm_fit) |>
  as_tibble()

summarized_draws <- summarize_draws(bayes_st_rapm_fit, 
                                    mean, median, 
                                    ~quantile(.x, probs = c(0.1, 0.9)))
coefs_only <- summarized_draws[-c(1,2,nrow(summarized_draws)),]
coefs_only$feature <- colnames(X)
coefs_only <- coefs_only |>
  mutate(type = case_when(
    substr(feature, nchar(feature) - 1, nchar(feature)) == "PP" ~ "PP",
    substr(feature, nchar(feature) - 1, nchar(feature)) == "PK" ~ "PK",
    TRUE ~ "Non-Player"),
    player_id = case_when(
      type == "Non-Player" ~ type,
      TRUE ~ substr(feature, 1, nchar(feature) - 3)
    )
  ) |>
  left_join(players)

write_rds(st_posterior_samples,
          here("data", "datasets", "special_teams_posterior_raw.rds"),
          compress = "gz")
write_rds(coefs_only,
          here("data", "datasets", "special_teams_posterior_clean.rds"),
          compress = "gz")
write_rds(bayes_st_rapm_fit,
          here("data","datasets","special_teams_fit_justincase"),
          compress = "gz")

test <- read_rds(here("data","datasets","special_teams_posterior_clean.rds"))
