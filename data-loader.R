library(here)
library(dplyr)
library(sportyR)
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)

source(here('docs', 'space_control.R'))

events <- here('data', 'events.parquet') |>
  arrow::read_parquet()

games <- here('data', 'games.parquet') |>
  arrow::read_parquet()

players <- here('data', 'players.parquet') |>
  arrow::read_parquet()

stints <- here('data', 'stints.parquet') |>
  arrow::read_parquet()

tracking <- here('data', 'tracking.parquet') |>
  arrow::read_parquet()
  
### space ownership

# 20 boxes across and 17 boxes long to grid the rink into 500 5x5ft boxes for ex
grid_the_rink <- function(n_boxes_x, n_boxes_y) {
  # I am going to assume nice values for box lengths and not put in any safeguards right now
  box_x_length_feet <- 200/n_boxes_x
  box_y_length_feet <- 85/n_boxes_y
  
  rink_grid <- expand.grid(c(1:n_boxes_x), c(1:n_boxes_y)) |>
    rename(x_box_id = Var1, y_box_id = Var2) |>
    mutate(x_min = -100 + box_x_length_feet*(x_box_id - 1),
           x_max = x_min + box_x_length_feet,
           y_min = -42.5 + box_y_length_feet*(y_box_id - 1),
           y_max = y_min + box_y_length_feet
    )
  return(rink_grid)
}

rink_grid <- grid_the_rink(n_boxes_x = 20, n_boxes_y = 10)
  
boxed_events <- events |>
  filter(!is.na(x_adj), !is.na(y_adj),
         # filtering out shootouts
         period <= 3,
         # filtering out penalty shots
         event_type != 'penaltyshot',
         stringr::str_sub(event_type, start = 1L, end = 2L) != 'ps',
         # filter to events with players involved
         !is.na(player_id),
         x_adj <= 100, x_adj >= -100, y_adj <= 42.5, y_adj >= -42.5) |>
  left_join(rink_grid |>
              mutate(y_max = if_else(y_max == 42.5, 42.51, y_max),
                     x_max = if_else(x_max == 100, 100.01, x_max)),
            join_by(x_adj >= x_min, x_adj < x_max,
                    y_adj >= y_min, y_adj < y_max)) |>
  mutate(box_id = stringr::str_c(x_box_id, y_box_id, sep = '-')) 

# want unique ids of uninterrupted possessions (i.e. other team does not gain
# possession, whistle not blown)
# no one has possession after a shot until a loose puck recovery
possessions <- boxed_events |>
  group_by(game_id, sequence_id) |>
  arrange(sl_event_id) |>
  # possession starts when team recovers puck after faceoff, not when faceoff is won
  mutate(possession_team_id = if_else(event_type == 'lpr' & outcome == 'successful', team_id, NA)) |>
  fill(possession_team_id, .direction = 'down') |>
  filter(!is.na(possession_team_id)) |>
  mutate(possession_id = consecutive_id(possession_team_id)) |>
  ungroup()

# full information possessions
fi_possessions <- possessions |>
  filter(has_tracking_data == 1, event_player_tracked == 1)
  
# we can use the NAs in tracking I guess since all we need is team
    
# helper function for data investigation: get a sequence slice surrounding a play
get_seq_slice <- function(event_data = events, given_game_id, given_period, given_time) {
  event_data |>
    filter(game_id == given_game_id,
           period == given_period,
           period_time >= given_time - 10,
           period_time <= given_time + 10) |>
    arrange(sl_event_id) |>
    View()
}
  
get_seq_slice('')
  
## space control + move prob model - computes space control of the point they move it
# to in the next frame of a possession using tracking data from the current frame
sc_move_data <- fi_possessions |>
  # just do one game for now
  filter(game_id == '2f293ccc-ece6-3dac-2e44-6b7f4f3c23f2') |>
  # these are events I determined to be moves (pass, carry)
  filter(event_type %in% c('controlledbreakout', 'pass',
                           'dumpin', 'dumpout',
                           'carry', 'puckprotection')) |>
  left_join(games |> 
              select(game_id, home_start_net, home_team_id),
            by = 'game_id') |>
  left_join(tracking |>
              select(game_id, sl_event_id, tracking_player_id = player_id,
                     tracking_team_id = team_id,
                     tracking_x, tracking_y, tracking_vel_x, tracking_vel_y),
            join_by(game_id, sl_event_id)) |>
  mutate(home_start_net = if_else(home_start_net == 'pos_x', 1, -1),
         home_team_net_sign = if_else(period %in% c(1, 3), home_start_net, -home_start_net),
         # if possession team = home team and home_team_net_sign is negative - do nothing
         # if possession team = home team and home_team_net_sign is positive - flip the coords
         # if possession team != home team and home_team_net_sign is positive - do nothing
         # if possession team != home team and home_team_net_sign is negative - flip coords
         needs_flipped = case_when(possession_team_id == home_team_id & home_team_net_sign > 0 ~ 1,
                                   possession_team_id != home_team_id & home_team_net_sign < 0 ~ 1,
                                   T ~ 0),
         tracking_x_adj = if_else(needs_flipped == 1, -tracking_x, tracking_x),
         tracking_y_adj = if_else(needs_flipped == 1, -tracking_y, tracking_y),
         tracking_vel_x_adj = if_else(needs_flipped == 1, -tracking_vel_x, tracking_vel_x),
         tracking_vel_y_adj = if_else(needs_flipped == 1, -tracking_vel_y, tracking_vel_y),
         puck_x_adj = x_adj,
         puck_y_adj = y_adj
  ) |>
  group_by(game_id, possession_id) |>
  mutate(to_x_adj = lead(x_adj),
         to_y_adj = lead(y_adj)) |>
  filter(!is.na(to_x_adj)) |> 
  ungroup() |>
  # fill(puck_x_adj, puck_y_adj, .by = c(game_id, sl_event_id), .direction = 'downup') |>
  # need to change this to just dataframe of full information plays - no filtering necessary
  filter(!is.na(puck_x_adj), !is.na(tracking_x_adj),
         !is.na(tracking_vel_x_adj), !is.na(tracking_player_id)) |>
  group_split(game_id, sl_event_id, tracking_player_id) |>
  purrr::map(\(df){
    mu <- get_player_influence_mu(player_x = df$tracking_x_adj, player_y = df$tracking_y_adj,
                              player_v_x = df$tracking_vel_x_adj, player_v_y = df$tracking_vel_y_adj)
    cov <- get_player_influence_cov(player_v_x = df$tracking_vel_x_adj, player_v_y = df$tracking_vel_y_adj,
                                    player_x = df$tracking_x_adj, player_y = df$tracking_y_adj,
                                    puck_x = df$puck_x_adj[1], puck_y = df$puck_y_adj[1])
    f_i_point = mvtnorm::dmvnorm(x = c(df$to_x_adj, df$to_y_adj), mean = mu, sigma = cov)
    f_i_skater = mvtnorm::dmvnorm(x = mu, mean = mu, sigma = cov)
    
    df |>
      mutate(I_i = f_i_point/f_i_skater)
  }) |>
  purrr::list_rbind() |>
  group_by(game_id, possession_id) |>
  mutate(rink_control = rje::expit(sum(if_else(tracking_team_id == possession_team_id, I_i, 0)) -
              sum(if_else(tracking_team_id != possession_team_id, I_i, 0)))) |>
  ungroup()