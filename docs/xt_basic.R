# PURPOSE: Implement basic xT model from [insert soccer guy link when I get 
# internet access]

# Prep Environment --------------------------------------------------------

library(here)
library(dplyr)
library(tidyr)

source(here('docs', 'misc_helper_fns.R'))
source(here('data_loader.R'))

# Helper functions + dataset prep -----------------------------------------

mp_data_basic <- get_move_prob_data_basic()
sp_data_basic <- get_sp_data_basic()
gp_data_basic <- get_gp_data_basic()

rink_grid <- grid_the_rink()

# initial transition matrix, with transitions missing if we did not observe them
init_trans_mat <- mp_data_basic |>
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
box_shot_probs <- sp_data_basic |>
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
box_goal_probs <- gp_data_basic |>
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

tol <- .005

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
    
    # move prob per box
    box_mp = 1 - box_sp
    
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
    
    xT_curr[xT_curr[['box_id']] == box, 'xT'] <- box_sp*box_gp + box_mp*summed_t_xt
    
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

