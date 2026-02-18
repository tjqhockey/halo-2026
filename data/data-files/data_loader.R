library(here)
library(dplyr)
library(sportyR)
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)

source(here('docs', 'space_control.R'))
source(here('docs', 'misc_helper_fns.R'))

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
  
  return(boxed_events)
}

# want unique ids of uninterrupted possessions (i.e. other team does not gain
# possession, whistle not blown)
# no one has possession after a shot until a loose puck recovery
get_possessions <- function(){
  
  boxed_events <- get_boxed_events()
  
  possessions <- boxed_events |>
    group_by(game_id, sequence_id) |>
    arrange(sl_event_id, .by_group = T) |>
    # possession starts when team recovers puck after faceoff, not when faceoff is won
    mutate(possession_team_id = if_else(event_type == 'lpr' &
                                          outcome == 'successful',
                                        team_id,
                                        NA)) |>
    fill(possession_team_id, .direction = 'down') |>
    filter(!is.na(possession_team_id)) |>
    mutate(possession_id = consecutive_id(possession_team_id)) |>
    ungroup()
  
  return(possessions)
  
}

get_possessions_5v5 <- function(){
  
  possessions <- get_possessions()
  
  penalties <- readRDS(here('data', 'datasets', 'penalties.rds'))
  
  no_penalties <- events |>
    anti_join(penalties,
              join_by(game_id,
                      period,
                      period_time >= start_time,
                      period_time <= end_time))
  
  pos_filtered_n <- possessions |>
    inner_join(no_penalties,
               join_by(game_id, sl_event_id)) |>
    group_by(game_id, possession_id) |>
    summarize(n = n())
  
  pos_unfiltered_n <- possessions |>
    group_by(game_id, possession_id) |>
    summarize(n = n())
  
  possessions_clean_ids <- pos_unfiltered_n |>
    inner_join(pos_filtered_n,
               join_by(game_id, possession_id, n)) |>
    select(-n)
  
  possessions_5v5 <- possessions |> 
    inner_join(possessions_clean_ids, 
               join_by(game_id, possession_id))
  
  return(possessions_5v5)
}


# 5v5 No Clean Tracking Requirement ---------------------------------------

# no need for clean tracking data here
# data for move prob transition matrix 
get_move_prob_data_basic <- function() {
  
  possessions <- get_possessions_5v5()
  
  mp_data_basic <- possessions |>
    filter(event_type %in% c('controlledbreakout', 'pass',
                             'dumpin', 'dumpout',
                             'carry', 'puckprotection', 'reception',
                             'failedpasslocation')) 
  return(mp_data_basic)
}

# no clean tracking of other players needed here
# gets all events that are either move or shoot so that shooting prob can be
# calculated per box
get_sp_data_basic <- function() {
  
  possessions <- get_possessions_5v5()
  sp_data_basic <- possessions |>
    # these are events I determined to be moves (pass, carry) or shots
    filter(event_type %in% c('controlledbreakout', 'pass',
                             'dumpin', 'dumpout',
                             'carry', 'puckprotection',
                             'shot'))
  return(sp_data_basic)
}

# basic data for goal prob given shot, do not care about tracking quality
get_gp_data_basic <- function(){
  possessions <- get_possessions_5v5()
  
  gp_data_basic <- possessions |>
    filter(event_type == 'shot')
  
  return(gp_data_basic)
}

get_xT_data_basic <- function() {
  
  possessions <- get_possessions_5v5()
  
  xT_data_basic <- possessions |>
    filter(event_type %in% c('controlledbreakout', 'pass',
                             'dumpin', 'dumpout',
                             'carry', 'puckprotection', 'reception',
                             'failedpasslocation', 'shot')) 
  return(xT_data_basic)
}


# Clean tracking data functions -------------------------------------------

get_possessions_tracking <- function(){
  
  clean_ids <- clean_tracking_event_ids()
  
  possessions <- get_possessions()
  
  pos_filtered_n <- possessions |>
    inner_join(clean_ids,
               join_by(game_id, sl_event_id)) |>
    group_by(game_id, possession_id) |>
    summarize(n = n())
  
  pos_unfiltered_n <- possessions |>
    group_by(game_id, possession_id) |>
    summarize(n = n())
  
  possessions_clean_ids <- pos_unfiltered_n |>
    inner_join(pos_filtered_n,
               join_by(game_id, possession_id, n)) |>
    select(-n)
  
  possessions |>
    inner_join(possessions_clean, 
               join_by(game_id, possession_id)) |>
    left_join(tracking_data |>
                select(game_id, sl_event_id, tracking_player_id = player_id,
                       tracking_team_id = team_id,
                       tracking_x, tracking_y, tracking_vel_x, tracking_vel_y),
              join_by(game_id, sl_event_id)) |>
    left_join(players |>
                select(player_id, position_group),
              join_by(player_id)) |>
    mutate(skater = if_else(position_group %in% c('F', 'D'), 1, 0))
    
  ## COME BACK IF TIME
  
  # # if skaters == 5 on a team and there are players with NA id, filter out
  # filter_out_nas <- filter_out_nas()
  # 
  # # constant x velocities 
  # repaired_velocities <- 
    
    
}

clean_tracking_event_ids <- function() {
  
  # events where nets are not empty
  no_empty_nets <- events |>
    left_join(stints |>
                distinct(game_id, game_stint, is_home_net_empty, is_away_net_empty),
              join_by(game_id, game_stint)) |>
    filter(is_home_net_empty == FALSE, is_away_net_empty == FALSE) |>
    select(game_id, sl_event_id)
  
  # plays where the maximum number of identified skaters is five
  not_too_many_skaters <- tracking |>
    left_join(players |>
                select(player_id, position_group),
              join_by(player_id)) |>
    mutate(skater = if_else(position_group %in% c('F', 'D'), 1, 0)) |>
    group_by(game_id, sl_event_id, team_id) |>
    summarize(n_skater = sum(skater)) |>
    mutate(filter_out = if_else(n_skater > 5, 1, 0)) |>
    group_by(game_id, sl_event_id) |>
    summarize(filter_out = sum(filter_out)) |>
    filter(filter_out == 0) |>
    select(game_id, sl_event_id) |>
    ungroup()
    
  # intermediary df
  penalties <- readRDS(here('data', 'datasets', 'penalties.rds'))
  
  no_penalties <- events |>
    anti_join(penalties,
              join_by(game_id,
                      period,
                      period_time >= start_time,
                      period_time <= end_time))
  
  has_full_tracking <- events |>
    filter(has_tracking_data == 1, event_player_tracked == 1) |>
    select(game_id, sl_event_id)
    
  clean_ids <- no_empty_nets |>
    inner_join(not_too_many_skaters,
               join_by(game_id, sl_event_id)) |>
    inner_join(no_penalties,
               join_by(game_id, sl_event_id)) |>
    inner_join(has_full_tracking,
               join_by(game_id, sl_event_id))
  
  return(clean_ids)
}



# want to filter to events that have clean tracking data
# already checked that there is only one row per player id in players table
# we want to check for 5 v 5 situations, no pulled goalie, no weirdness

# filter_out_na_players <- tracking_pos_filter |>
#   mutate()
# #   
# # 
# #   
# (tracking |>
#   mutate(v = sqrt(tracking_vel_x^2 + tracking_vel_y^2)) |>
#   filter(tracking_vel_x > 36 | tracking_vel_y > 36 | v > 36) |> View()
#     # filter(v < 100) |>
#     pull(v)|>
#   hist()
#   nrow())/nrow(tracking)
# 
# tracking |>
#   filter(game_id == '00b0366a-95c6-5250-2dae-e3dd5c4198bc', sl_event_id == 466) |> 
#   View()
#   
  
# 
# # full information possessions
# fi_possessions <- possessions |>
#   filter(has_tracking_data == 1, event_player_tracked == 1) |>
# 
# tracking |>
#   group_by(game_id, sl_event_id) |>
#   summarize(n = n()) |>
#   group_by(n) |>
#   summarize(num = n()) |> View()
# 
# tracking |>
#   group_by(game_id, sl_event_id) |>
#   filter(n() == 19) |> View()
#   

# now we care that we have clean tracking for our shooting prob model
get_sp_data_tracking <- function() {
  
}
