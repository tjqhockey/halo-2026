# data loading
library(here)
library(stringr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(mgcv)
library(arrow)
library(lubridate)

tracking <- read_parquet("HALO HACKATHON/tracking.parquet")
events <- read_parquet("HALO HACKATHON/events.parquet")
stints <- read_parquet("HALO HACKATHON/stints.parquet")
games <- read_parquet("HALO HACKATHON/games.parquet")
players <- read_parquet("HALO HACKATHON/players.parquet")

players_1 <- "2023-24_NHLPlayers.txt"
players_2 <- "2024-25_NHLPlayers.txt"
players_3 <- "2025-26_NHLPlayers.txt"

df_1 <- read.csv(here('halo-2026', 'data', 'data-files', players_1),
                 header = TRUE, stringsAsFactors = FALSE)
df_2 <- read.csv(here('halo-2026', 'data', 'data-files', players_2),
                 header = TRUE, stringsAsFactors = FALSE)
df_3 <- read.csv(here('halo-2026', 'data', 'data-files', players_3),
                 header = TRUE, stringsAsFactors = FALSE)

colnames(df_1) <- df_1[1, ]
df_1 <- df_1[2:nrow(df_1), ]
# df_1["-9999"] <- df_1["Additional"]

colnames(df_2) <- df_2[1, ]
df_2 <- df_2[2:nrow(df_2), ]
# df_2["-9999"] <- df_2["Additional"]

colnames(df_3) <- df_3[1, ]
df_3 <- df_3[2:nrow(df_3), ]
# df_3["-9999"] <- df_3["Additional"]

df_1[, c(3, 6:21, 24:30)] <- lapply(df_1[, c(3, 6:21, 24:30)], as.numeric)
df_2[, c(3, 6:21, 24:30)] <- lapply(df_2[, c(3, 6:21, 24:30)], as.numeric)
df_3[, c(3, 6:21, 24:30)] <- lapply(df_3[, c(3, 6:21, 24:30)], as.numeric)

df_1 <- df_1[df_1$Team != "2TM", ]
df_2 <- df_2[df_2$Team != "2TM", ]
df_3 <- df_3[df_3$Team != "2TM", ]

# look for promoted players

# --- 1. Normalize players table ("Last, First" -> "First Last") ---
players <- players |>
  mutate(
    player_clean = str_replace(player_name, "^([^,]+),\\s*(.*)$", "\\2 \\1"),
    player_clean = str_to_lower(str_trim(player_clean))
  )

# --- 2. Normalize each season df ("First Last") ---
clean_names <- function(df) {
  df |>
    mutate(
      player_clean = str_to_lower(str_trim(Player))
    ) |>
    pull(player_clean)
}

season_names <- c(
  clean_names(df_1),
  clean_names(df_2),
  clean_names(df_3)
)

season_names <- unique(season_names)

# --- 3. Add made_nhl flag ---
players <- players |>
  mutate(
    made_nhl = if_else(player_clean %in% season_names, 1, 0)
  )

# read in relevant coefficients

even_strength_data <- read.csv(here('halo-2026', 'data', 'datasets',
                                    'posterior_ratings_xg.csv'),
                               header = TRUE, stringsAsFactors = FALSE)

even_strength_data <- even_strength_data |>
  rename(
    xG_coef = mean,
    xG_coef_median = median,
    xG_coef_lower_80 = lower_80,
    xG_coef_upper_80 = upper_80
  )

special_teams_data <- read.csv(here('halo-2026', 'data', 'datasets',
                                    'special_teams_posterior_clean.csv'),
                               header = TRUE, stringsAsFactors = FALSE)

# --- SPECIAL TEAMS POSTERIOR INTEGRATION ---

special_players <- special_teams_data |>
  filter(type %in% c("PP", "PK")) |>
  mutate(
    stint_type = str_to_lower(type),                 # "PP" → "pp"
    prefix = paste0("xG_", stint_type, "_coef")      # → "xG_pp_coef"
  ) |>
  select(
    player_id,
    prefix,
    mean,
    median,
    X10.,
    X90.
  )

special_players_wide <- special_players |>
  pivot_wider(
    names_from = prefix,
    values_from = c(mean, median, X10., X90.)
  )

special_players_wide <- special_players_wide |>
  rename(
    xG_pp_coef          = mean_xG_pp_coef,
    xG_pk_coef          = mean_xG_pk_coef,
    xG_pp_coef_median   = median_xG_pp_coef,
    xG_pk_coef_median   = median_xG_pk_coef,
    xG_pp_coef_lower_80 = X10._xG_pp_coef,
    xG_pk_coef_lower_80 = X10._xG_pk_coef,
    xG_pp_coef_upper_80 = X90._xG_pp_coef,
    xG_pk_coef_upper_80 = X90._xG_pk_coef
  )

# # Clean up the data after running ridge model in special_teams_rapm.R
# player_ridge_ratings <- ridge_ratings_df |>
#   filter(feature != "(Intercept)",
#          feature != "n_pp_skaters",
#          feature != "n_pk_skaters") |>
#   # Split the feature (eg "xyz_PP") back into ID and Role
#   separate(feature, into = c("player_id", "stint_type"), sep = "_")
# 
# cleaned_player_ridge_ratings <- player_ridge_ratings |>
#   mutate(
#     stint_type = str_to_lower(stint_type),
#     stint_type = paste0("xG_", stint_type, "_coef")
#   ) |>
#   select(player_id, stint_type, lambda.min) |>
#   pivot_wider(
#     names_from = stint_type,
#     values_from = lambda.min
#   )

players <- players |>
  left_join(even_strength_data, by = "player_name") |>
  left_join(special_players_wide, by = "player_id")

players <- players |>
  mutate(age = floor(time_length(interval(birth_date, ymd("2023-09-01")), "years")))

promoted_players <- players[players$made_nhl == 1, ] |>
  distinct(player_id, .keep_all = TRUE)

gp_long <- bind_rows(
  df_1 |> select(Player, GP),
  df_2 |> select(Player, GP),
  df_3 |> select(Player, GP)
) |>
  mutate(
    player_clean = str_to_lower(str_trim(Player))
  )

gp_totals <- gp_long |>
  group_by(player_clean) |>
  summarise(total_gp = sum(GP, na.rm = TRUE))

promoted_players <- promoted_players |>
  left_join(gp_totals, by = "player_clean") |>
  rename(
    GP = total_gp
  )

# end data setup

### Modeling
# Logistic Regression: made_nhl ~ xG_coef*age +
## xG_pp_coef*age + xG_pk_coef*age + position_group
# Given made_nhl = 1:
## GAM: Games ~ xG_coef*age +
## xG_pp_coef*age + xG_pk_coef*age + position_group

log_reg <- glm(made_nhl ~ xG_coef*age + xG_pp_coef*age
               + xG_pk_coef*age + position_group,
               family = "binomial", data = players)

summary(log_reg)

promoted_glm <- glm(GP ~ xG_coef*age + xG_pp_coef*age +
                      xG_pk_coef*age + position_group,
                    family = poisson(),
                    data = promoted_players)

summary(promoted_glm)

## Plots

# plot(log_reg)

library(ggplot2)
mf <- model.frame(promoted_glm)
mf_log <- model.frame(log_reg)

ggplot(mf, aes(y = GP, x = fitted(promoted_glm))) +
  geom_point() +
  # geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  theme_bw() + 
  labs(y = "Observed GP", x = "Fitted GP",
       title = "Observed vs. Predicted Games Played for Promoted Players")

p1 <- ggplot(mf_log, aes(x = xG_coef, y = log_reg$fitted.values)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  theme_bw() +
  labs(x = "xG Coefficient Estimate", y = "Fitted Probability of Promotion",
       title = "Player Promotion Probabilities vs. Player xG Coefficients")

p2 <- ggplot(mf_log, aes(x = age, y = log_reg$fitted.values)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  theme_bw() +
  labs(x = "Age on 9/1/23", y = "Fitted Probability of Promotion",
       title = "Player Promotion Probabilities vs. Player Age")

library(patchwork)
p1 + p2

## End Plots




























library(here)
library(dplyr)
library(tidyr)
library(mgcv)
library(glmnet)
library(tidyverse)

## Bring in data, code from setup_test.R
xt_event_data <- readRDS(here('halo-2026', 'data', 'datasets', 'xt_sc_data.rds'))

events <- here('halo-2026', 'data', 'datasets', 'events.parquet') |>
  arrow::read_parquet()

games <- here('halo-2026', 'data', 'datasets', 'games.parquet') |>
  arrow::read_parquet()

players <- here('halo-2026', 'data', 'datasets', 'players.parquet') |>
  arrow::read_parquet()

stints <- here('halo-2026', 'data', 'datasets', 'stints.parquet') |>
  arrow::read_parquet()

tracking <- here('halo-2026', 'data', 'datasets', 'tracking.parquet') |>
  arrow::read_parquet()


## Get total xG by player for each stint
sum_xg_per_stint <- events |>
  group_by(game_id, period, game_stint, player_name, player_id) |>
  summarize(sum_xG = sum(ifelse(is.na(sl_xg_all_shots) == 0,
                                sl_xg_all_shots,0)))

## Joining with stints and games tables
xg_stints <- stints |> select(-home_score,-away_score)|>
  filter(n_home_skaters != n_away_skaters) |>
  left_join(games, by = "game_id") |>
  mutate(pp_team = ifelse(n_home_skaters > n_away_skaters,home_team,away_team),
         pk_team = ifelse(n_home_skaters < n_away_skaters,home_team,away_team),
         n_pp_skaters = ifelse(n_home_skaters>n_away_skaters,n_home_skaters,n_away_skaters),
         n_pk_skaters = ifelse(n_home_skaters<n_away_skaters,n_home_skaters,n_away_skaters),
         stint_type = ifelse(team == pp_team,"PP","PK"),
         stint_length = period_time_end - period_time_start
  ) |>
  left_join(sum_xg_per_stint, by = c("game_id", "period", "game_stint", "player_id",
                                     "player_name")) |>
  mutate(matrix_value = ifelse(team == pp_team, 1, -1),
         sum_xG = coalesce(sum_xG, 0)) |>
  select(game_id, period, game_stint, player_id, matrix_value, sum_xG,
         home_team, away_team, n_pp_skaters,n_pk_skaters, stint_type,
         stint_length)


## Getting PP and PK xG for each stint
pp_xg <- xg_stints |>
  filter(matrix_value == 1) |>
  group_by(game_id, period, game_stint, matrix_value, n_pp_skaters, 
           n_pk_skaters,stint_length) |>
  summarize(xG_pp = sum(sum_xG),
            n = n())
pk_xg <- xg_stints |>
  filter(matrix_value == -1) |>
  group_by(game_id, period, game_stint, matrix_value, n_pp_skaters, 
           n_pk_skaters, stint_length) |>
  summarize(xG_pk = sum(sum_xG))

## Calculate net xG as PP - PK
st_net_xg <- inner_join(pp_xg, pk_xg, by = c("game_id", "period", "game_stint",
                                             "n_pp_skaters","n_pk_skaters",
                                             "stint_length")) |>
  mutate(y_xG = 3600*(xG_pp - xG_pk)/stint_length) |>
  select(game_id, period, game_stint, y_xG, n_pp_skaters, n_pk_skaters,
         stint_length)

## Putting net xT into the right df to make design matrices
xg_stints <- xg_stints |>
  inner_join(st_net_xg, by = c("game_id", "period", "game_stint",
                               "stint_length", "n_pp_skaters",
                               "n_pk_skaters"))


## Building design matrices
design_matrix <- xg_stints |>
  mutate(player_col = paste0(player_id, "_", stint_type)) |>
  pivot_wider(
    id_cols = c(game_id, period, game_stint,  stint_length,
                n_pp_skaters, n_pk_skaters, y_xG),
    names_from = player_col,
    values_from = matrix_value,
    values_fill = 0
  ) |>
  ungroup()

# 4. Final check: replace any remaining NAs in player columns with 0
design_matrix[is.na(design_matrix)] <- 0


## Fitting Ridge Regression (mostly just to make sure ts works)
X <- as.matrix(design_matrix |>
                 select(-c(game_id,period,game_stint,
                           stint_length, y_xG))
)
y <- design_matrix$y_xG
wgts <- design_matrix$stint_length

ridge_model <- cv.glmnet(X, y, alpha = 0, weights = wgts)
ridge_ratings <- coef(ridge_model, s = "lambda.min")
ridge_ratings_df <- ridge_ratings %>%
  as.matrix() %>%
  as.data.frame() %>%
  rownames_to_column(var = "feature")

# 2. Clean up the data
player_ridge_ratings <- ridge_ratings_df %>%
  filter(feature != "(Intercept)",
         feature != "n_pp_skaters",
         feature != "n_pk_skaters") %>%
  # Split the feature (eg "xyz_PP") back into ID and Role
  separate(feature, into = c("player_id", "stint_type"), sep = "_") |>
  left_join(players)

cleaned_player_ridge_ratings <- player_ridge_ratings |>
  mutate(
    stint_type = str_to_lower(stint_type),        # "PP" → "pp"
    stint_type = paste0("xG_", stint_type, "_coef")  # → "xG_pp_coef"
  ) |>
  select(player_id, stint_type, lambda.min) |>
  pivot_wider(
    names_from = stint_type,
    values_from = lambda.min
  )

# end manipulations

test <- stints |>
  filter(game_id == "709f514f-e471-41ba-7261-62151e8445ad",game_stint == 159) |>
  left_join(xg_stints)

test2 <- sum_xg_per_stint |>
  filter(game_id == "709f514f-e471-41ba-7261-62151e8445ad",
         game_stint == 159)

test3 <- events |>
  filter(game_id == "709f514f-e471-41ba-7261-62151e8445ad",game_stint == 159)

test4 <- games |>
  filter(game_id == "6c660c51-6fbf-aec3-1671-02e0fde52ce6")

test5 <- design_matrix[769,]

test6 <- players |>
  filter(player_id == "774bd9d6-51d4-5596-d6da-a8a7cbdece73")
