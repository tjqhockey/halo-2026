# First, run setup_test.R to get all the data
# This will then join the avg xT for each player and stint to the stints table, then write the design matrices as RDS files

# Get xT added and xG per stint
avg_xt_per_stint <- xt_event_data |>
  group_by(game_id, period, game_stint, player_name) |>
  # Fill xT_sc with xT_plus if it is NA
  mutate(xT_next_sc = coalesce(xT_next_sc, xT_next_plus),
         change_xT_sc = xT_next_sc - xT_sc) |>
  summarize(xTA = sum(change_xT_sc),
            xG = sum(sl_xg_all_shots, na.rm = T))

# Join to stints table
xt_stints <- stints |>
  # Calculate stint length
  mutate(stint_length = period_time_end - period_time_start) |>
  # Filter for even strength
  filter(n_home_skaters == n_away_skaters) |>
  # Join xTA and xG values
  full_join(avg_xt_per_stint, by = c("game_id", "period", "game_stint", "player_name")) |>
  # Assume players without events have 0 xT and xG
  mutate(xTA = replace_na(xTA, 0),
         xG = replace_na(xG, 0)) |>
  # Normalize xTA or xG / 60 mins
  mutate(xTA = xTA / stint_length * 3600,
         xG = xG / stint_length * 3600) |>
  # Join games data to determine whether home (+1) or away (-1)
  inner_join(games, by = "game_id") |>
  mutate(matrix_value = ifelse(team == home_team, 1, -1)) |>
  # Select relevant columns
  select(game_id, period, game_stint, player_id, matrix_value, xTA, xG)

# Get the xT for home and away for each shift
home_xt <- xt_stints |>
  filter(matrix_value == 1) |>
  group_by(game_id, period, game_stint, matrix_value) |>
  summarize(xTA_home = sum(xTA),
            xG_home = sum(xG))
away_xt <- xt_stints |>
  filter(matrix_value == -1) |>
  group_by(game_id, period, game_stint, matrix_value) |>
  summarize(xTA_away = sum(xTA),
            xG_away = sum(xG))
# Calculate net xt as home-away
net_xt <- inner_join(home_xt, away_xt, by = c("game_id", "period", "game_stint")) |>
  mutate(y_xTA = xTA_home - xTA_away,
         y_xG = xG_home - xG_away) |>
  select(game_id, period, game_stint, y_xTA, y_xG)

# Join the net xT and select player ID, +/- 1, and y to prepare to build design matrices
xt_stints <- xt_stints |>
  inner_join(net_xt, by = c("game_id", "period", "game_stint")) |>
  select(-xTA, -xG)

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
  select(-game_id, -period, -game_stint, -y_xTA) |>
  as.matrix()

# Save design matrices as RDS files
saveRDS(xt_matrix, here('data', 'datasets', 'xt_matrix.rds'))
saveRDS(xg_matrix, here('data', 'datasets', 'xg_matrix.rds'))
