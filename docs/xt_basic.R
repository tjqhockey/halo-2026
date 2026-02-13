# PURPOSE: Implement basic xT model from [insert soccer guy link when I get 
# internet access]

# Prep Environment --------------------------------------------------------

library(here)
library(dplyr)
library(tidyr)

events <- here('data', 'events.parquet') |>
  arrow::read_parquet()


# Helper functions + dataset prep -----------------------------------------

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

# assign each event and associated coordinate pair to a rink grid box
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
possessions <- boxed_events |>
  group_by(game_id, sequence_id) |>
  arrange(sl_event_id) |>
  # possession starts when team recovers puck after faceoff, not when faceoff is won
  mutate(possession_team = if_else(event_type == 'lpr' & outcome == 'successful',
                                   team, NA)) |>
  fill(possession_team, .direction = 'down') |>
  filter(!is.na(possession_team))

# initial transition matrix, with transitions missing if we did not observe them
init_trans_mat <- possessions |>
  filter(event_type %in% c('controlledbreakout', 'pass',
                           'dumpin', 'dumpout',
                           'carry', 'puckprotection', 'reception',
                           'failedpasslocation')) |>
  group_by(game_id) |>
  arrange(sl_event_id) |>
  # possession id is only unique to each sequence
  mutate(possession_id = consecutive_id(possession_team)) |>
  group_by(game_id, possession_id) |>
  mutate(from_box = lag(box_id)) |> 
  filter(!is.na(from_box)) |>
  group_by(from_box, box_id) |>
  summarize(count = n()) |>
  group_by(from_box) |> 
  mutate(total_from = sum(count),
         transition_prob = count/total_from) |>
  select(from_box, to_box = box_id, transition_prob)

# get all box ids for general use
all_box_ids <- stringr::str_c(rink_grid$x_box_id, rink_grid$y_box_id, sep = '-')

# transition matrix with transition probability = 0 for all box pairs we did not
# observe
trans_mat <- expand.grid(all_box_ids,
                         all_box_ids) |>
  rename(from_box = Var1, to_box = Var2) |>
  mutate(transition_prob_fill = 0) |>
  full_join(init_trans_mat) |>
  mutate(transition_prob = if_else(is.na(transition_prob),
                                   transition_prob_fill,
                                   transition_prob)) |>
  select(-transition_prob_fill)

# get empirical shooting probability for each box
box_shot_probs <- possessions |>
  # these are events I determined to be moves (pass, carry) or shots
  filter(event_type %in% c('controlledbreakout', 'pass',
                           'dumpin', 'dumpout',
                           'carry', 'puckprotection',
                           'shot')) |>
  group_by(box_id) |>
  summarize(total_shots = if_else(event_type == 'shot', 1, 0) |>
              sum(),
            total = n(),
            shot_prob = total_shots/total) |>
  full_join(tibble(box_id = all_box_ids, shot_prob_fill = 0),
            by = 'box_id') |>
  mutate(shot_prob = if_else(is.na(shot_prob), shot_prob_fill, shot_prob)) |>
  select(-shot_prob_fill)

# get the empirical goal probability given a shot was taken from a certain box
box_goal_probs <- possessions |>
  filter(event_type == 'shot') |>
  group_by(box_id) |>
  summarize(total_goals = if_else(stringr::str_detect(flags, 'withgoal'), 1, 0) |>
              sum(),
            total = n(),
            goal_prob = total_goals/total) |>
  full_join(tibble(box_id = all_box_ids, goal_prob_fill = 0),
            by = 'box_id') |>
  mutate(goal_prob = if_else(is.na(goal_prob), goal_prob_fill, goal_prob)) |>
  select(-goal_prob_fill)


# xT Convergence Loop -----------------------------------------------------

# evaluate iteratively until convergence:

# initialize xt of all zones to 0
xT_prev <- tibble(box_id = stringr::str_c(rink_grid$x_box_id, rink_grid$y_box_id, sep = '-'),
                  xT = 0)

xT_curr <- xT_prev

tol <- .01

max_deviation <- 1

dev_tbl <- tibble()

# this loop will update so that xT_curr is the xT of each box after EM-type
# algorithm. Data on convergence will be in dev_tbl.
for (iter in 1:100) {
  
  if (max_deviation < tol) {
    break
  }
  
  print(iter)
  
  for (box in xT_prev$box_id) {
    
    # get shot prob for box
    box_sp <- box_shot_probs |> 
      filter(box_id == box) |> 
      pull(shot_prob)
    
    # get goal prob given shot for box
    box_gp <- box_goal_probs |> 
      filter(box_id == box) |> 
      pull(goal_prob)
    
    # sum transition prob*xT of other boxes
    summed_t_xt <- trans_mat |>
      filter(from_box == box) |>
      left_join(xT_prev,
                join_by('to_box' == 'box_id')) |>
      mutate(tp_xT = transition_prob*xT) |>
      pull(tp_xT) |>
      sum()
    
    xT_curr[xT_curr[['box_id']] == box, 'xT'] <- box_sp*box_gp + summed_t_xt
    
  }
  
  # get the max deviation for any one box
  devs <- xT_curr |>
    left_join(xT_prev |>
                rename(xT_prev = xT),
              by = 'box_id') |>
    mutate(dev = xT - xT_prev,
           iteration = iter)
  
  max_deviation <- max(abs(devs$dev))
  
  dev_tbl <- dev_tbl |>
    bind_rows(devs)
  
  xT_prev <- xT_curr
  
}

