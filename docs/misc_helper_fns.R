# PURPOSE: house miscellaneous data investigation helper functions

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

grid_the_rink <- function(n_boxes_x = 20, n_boxes_y = 10) {
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