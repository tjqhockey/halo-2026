# First, run setup_test.R to get all the data
# This will then join the avg xT for each player and stint to the stints table, then write the design matrices as RDS files

# Get average xT and xG per stint
avg_xt_per_stint <- xt_event_data |>
  group_by(game_id, period, game_stint, player_name) |>
  summarize(avg_xT_sc = mean(xT_sc),
            avg_xG_plus = mean(xG_plus))

# Join to stints table
xt_stints <- stints |>
  full_join(avg_xt_per_stint, by = c("game_id", "period", "game_stint", "player_name")) |>
  # Assume players without events have 0 xT and xG
  mutate(avg_xT_sc = replace_na(avg_xT_sc, 0),
         avg_xG_plus = replace_na(avg_xG_plus, 0)) |>
  # Join games data to determine whether home (+1) or away (-1)
  inner_join(games, by = "game_id") |>
  mutate(matrix_value = ifelse(team == home_team, 1, -1)) |>
  # Select relevant columns
  select(game_id, period, game_stint, player_id, matrix_value, avg_xT_sc, avg_xG_plus)

# Get the xT for home and away for each shift
home_xt <- xt_stints |>
  filter(matrix_value == 1) |>
  group_by(game_id, period, game_stint, matrix_value) |>
  summarize(xT_home = sum(avg_xT_sc),
            xG_home = sum(avg_xG_plus))
away_xt <- xt_stints |>
  filter(matrix_value == -1) |>
  group_by(game_id, period, game_stint, matrix_value) |>
  summarize(xT_away = sum(avg_xT_sc),
            xG_away = sum(avg_xG_plus))
# Calculate net xt as home-away
net_xt <- inner_join(home_xt, away_xt, by = c("game_id", "period", "game_stint")) |>
  mutate(y_xT = xT_home - xT_away,
         y_xG = xG_home - xG_away) |>
  select(game_id, period, game_stint, y_xT, y_xG)

# Join the net xT and select player ID, +/- 1, and y to prepare to build design matrices
xt_stints <- xt_stints |>
  inner_join(net_xt, by = c("game_id", "period", "game_stint")) |>
  select(-avg_xT_sc, -avg_xG_plus)

# Build design matrices
xt_matrix <- xt_stints |>
  pivot_wider(names_from = player_id,
              values_from = matrix_value,
              values_fill = 0) |>
  select(-game_id, -period, -game_stint, -y_xG) |>
  as.matrix()

xg_matrix <- xt_stints |>
  pivot_wider(names_from = player_id,
              values_from = matrix_value,
              values_fill = 0) |>
  select(-game_id, -period, -game_stint, -y_xT) |>
  as.matrix()

# Save design matrices as RDS files
saveRDS(xt_matrix, here('data', 'datasets', 'xt_matrix.rds'))
saveRDS(xg_matrix, here('data', 'datasets', 'xg_matrix.rds'))
