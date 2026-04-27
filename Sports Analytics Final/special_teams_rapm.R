library(here)
library(dplyr)
library(tidyr)
library(mgcv)
library(glmnet)
library(tidyverse)

## Bring in data, code from setup_test.R
xt_event_data <- readRDS(here('data', 'datasets', 'xt_sc_data.rds'))

events <- here('data', 'datasets', 'events.parquet') |>
  arrow::read_parquet()

games <- here('data', 'datasets', 'games.parquet') |>
  arrow::read_parquet()

players <- here('data', 'datasets', 'players.parquet') |>
  arrow::read_parquet()

stints <- here('data', 'datasets', 'stints.parquet') |>
  arrow::read_parquet()

tracking <- here('data', 'datasets', 'tracking.parquet') |>
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

# Saving design matrix for stan part
saveRDS(design_matrix, here('data', 'datasets', 'special_teams_design.rds'))





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
