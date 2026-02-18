library(here)
library(dplyr)
library(sportyR)
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)

source(here('docs', 'space_control.R'))
source(here('docs', 'misc_helper_fns.R'))

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
  
rink_grid <- grid_the_rink()

# assign each event to a box based on coordinates and filter out unwanted
# events
get_boxed_events <- function() {
  
  boxed_events <- events |>
    filter( # we need events with coordinate locations
      !is.na(x_adj), !is.na(y_adj),
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
}

# want unique ids of uninterrupted possessions (i.e. other team does not gain
# possession, whistle not blown)
# no one has possession after a shot until a loose puck recovery
get_possessions <- function(){
  
  boxed_events <- get_boxed_events()
  
  possessions <- boxed_events |>
    group_by(game_id, sequence_id) |>
    arrange(sl_event_id) |>
    # possession starts when team recovers puck after faceoff, not when faceoff is won
    mutate(possession_team_id = if_else(event_type == 'lpr' &
                                          outcome == 'successful',
                                        team_id,
                                        NA)) |>
    fill(possession_team_id, .direction = 'down') |>
    filter(!is.na(possession_team_id)) |>
    mutate(possession_id = consecutive_id(possession_team_id)) |>
    ungroup()
  
}

# no need for clean tracking data here
get_move_prob_data_basic <- function() {
  possessions <- get_possessions()
  
  mp_data_simple <- possessions |>
    filter(event_type %in% c('controlledbreakout', 'pass',
                             'dumpin', 'dumpout',
                             'carry', 'puckprotection', 'reception',
                             'failedpasslocation')) 
  return(mp_data_simple)
}

# no clean tracking of other players needed here
# gets all events that are either move or shoot so that shooting prob can be
# calculated per box
get_sp_data_basic <- function() {
  
  possessions |>
    # these are events I determined to be moves (pass, carry) or shots
    filter(event_type %in% c('controlledbreakout', 'pass',
                             'dumpin', 'dumpout',
                             'carry', 'puckprotection',
                             'shot'))

}

# now we care that we have clean tracking for our shooting prob model
get_sp_data_tracking <- function() {
  
}

clean_tracking_event_ids <- function() {
  
}

# want to filter to events that have clean tracking data
# already checked that there is only one row per player id in players table
# we want to check for 5 v 5 situations, no pulled goalie, no weirdness
not_too_many_men <- tracking |>
  left_join(players |>
              select(player_id, position_group),
            join_by(player_id)) |>
  group_by(game_id, sl_event_id, position_group) |>
  summarize(n = n()) |> View()
  mutate(g_f = case_when())
  group_by(game_id, sl_event_id) |>
  summarize() 
  

  
(tracking |>
  mutate(v = sqrt(tracking_vel_x^2 + tracking_vel_y^2)) |>
  filter(tracking_vel_x > 36 | tracking_vel_y > 36 | v > 36) |>
    filter(v < 100) |>
    pull(v)|>
  hist()
  nrow())/nrow(tracking)
  
no_empty_nets <- events |>
  left_join(stints,
            join_by(game_id, game_stint))

# full information possessions
fi_possessions <- possessions |>
  filter(has_tracking_data == 1, event_player_tracked == 1) |>

tracking |>
  group_by(game_id, sl_event_id) |>
  summarize(n = n()) |>
  group_by(n) |>
  summarize(num = n()) |> View()

tracking |>
  group_by(game_id, sl_event_id) |>
  filter(n() == 19) |> View()
  
