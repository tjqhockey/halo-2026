## space control + move prob model - computes space control of the point they move it
# to in the next frame of a possession using tracking data from the current frame

list_of_packages <- c("furrr", "here", "dplyr", 'tidyr', 'purrr', 'stringr')

for (p in list_of_packages) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p, dependencies = TRUE)
    library(p, character.only = TRUE)
  }
}

source(here('docs', 'misc_helper_fns.R'))
source(here('data', 'data-files', 'data_loader.R'))

# Set plan to use all available cores
plan(multisession, workers = parallelly::availableCores())

# shot prob data with tracking of other players
sp_tracking_data <- get_sp_data_tracking()

rink_grid <- grid_the_rink() |>
  mutate(box_id = str_c(x_box_id, y_box_id, sep = '-')) |>
  select(-x_box_id, -y_box_id)

# sc_move_data <- sp_tracking_data |>
#   # just do one game for now
#   filter(game_id == '2f293ccc-ece6-3dac-2e44-6b7f4f3c23f2') |>
#   # these are events I determined to be moves (pass, carry)
#   filter(event_type %in% c('controlledbreakout', 'pass',
#                            'dumpin', 'dumpout',
#                            'carry', 'puckprotection')) |>
#   left_join(games |> 
#               select(game_id, home_start_net, home_team_id),
#             by = 'game_id') |>
#   left_join(tracking |>
#               select(game_id, sl_event_id, tracking_player_id = player_id,
#                      tracking_team_id = team_id,
#                      tracking_x, tracking_y, tracking_vel_x, tracking_vel_y),
#             join_by(game_id, sl_event_id)) |>
#   mutate(home_start_net = if_else(home_start_net == 'pos_x', 1, -1),
#          home_team_net_sign = if_else(period %in% c(1, 3), home_start_net, -home_start_net),
#          # if possession team = home team and home_team_net_sign is negative - do nothing
#          # if possession team = home team and home_team_net_sign is positive - flip the coords
#          # if possession team != home team and home_team_net_sign is positive - do nothing
#          # if possession team != home team and home_team_net_sign is negative - flip coords
#          needs_flipped = case_when(possession_team_id == home_team_id & home_team_net_sign > 0 ~ 1,
#                                    possession_team_id != home_team_id & home_team_net_sign < 0 ~ 1,
#                                    T ~ 0),
#          tracking_x_adj = if_else(needs_flipped == 1, -tracking_x, tracking_x),
#          tracking_y_adj = if_else(needs_flipped == 1, -tracking_y, tracking_y),
#          tracking_vel_x_adj = if_else(needs_flipped == 1, -tracking_vel_x, tracking_vel_x),
#          tracking_vel_y_adj = if_else(needs_flipped == 1, -tracking_vel_y, tracking_vel_y),
#          puck_x_adj = x_adj,
#          puck_y_adj = y_adj
#   ) |>
#   group_by(game_id, possession_id) |>
#   mutate(to_x_adj = lead(x_adj),
#          to_y_adj = lead(y_adj),
#          to_x_min = lead(x_min),
#          to_x_max = lead(x_max),
#          to_y_min = lead(y_min),
#          to_y_max = lead(y_max)) |>
#   filter(!is.na(to_x_adj)) |> 
#   ungroup() |>
#   # fill(puck_x_adj, puck_y_adj, .by = c(game_id, sl_event_id), .direction = 'downup') |>
#   # need to change this to just dataframe of full information plays - no filtering necessary
#   filter(!is.na(puck_x_adj), !is.na(tracking_x_adj),
#          !is.na(tracking_vel_x_adj), !is.na(tracking_player_id)) |>
#   group_split(game_id, sl_event_id, tracking_player_id) |>
#   purrr::map(\(df){
#     mu <- get_player_influence_mu(player_x = df$tracking_x_adj, player_y = df$tracking_y_adj,
#                                   player_v_x = df$tracking_vel_x_adj, player_v_y = df$tracking_vel_y_adj)
#     print(mu)
#     cov <- get_player_influence_cov(player_v_x = df$tracking_vel_x_adj, player_v_y = df$tracking_vel_y_adj,
#                                     player_x = df$tracking_x_adj, player_y = df$tracking_y_adj,
#                                     puck_x = df$puck_x_adj[1], puck_y = df$puck_y_adj[1])
#     
#     integ_box <- mvtnorm::pmvnorm(upper = c(df$to_x_max, df$to_y_max),
#                                   lower = c(df$to_x_min, df$to_y_min),
#                                   mean = mu,
#                                   sigma = cov)
#     
#     # f_i_point = mvtnorm::dmvnorm(x = c(df$to_x_adj, df$to_y_adj), mean = mu, sigma = cov)
#     # f_i_skater = mvtnorm::dmvnorm(x = mu, mean = mu, sigma = cov)
#     
#     df |>
#       mutate(#I_i = f_i_point/f_i_skater
#         integ_box = integ_box
#       )
#     
#   }) |>
#   purrr::list_rbind() |>
#   group_by(game_id, possession_id) |>
#   mutate(# rink_control = rje::expit(sum(if_else(tracking_team_id == possession_team_id, I_i, 0)) -
#     # sum(if_else(tracking_team_id != possession_team_id, I_i, 0)))
#     pos_box_control = sum(if_else(tracking_team_id == possession_team_id, integ_box, 0)),
#     neg_box_control = sum(if_else(tracking_team_id != possession_team_id, integ_box, 0)),
#     tot_box_control = pos_box_control - neg_box_control,
#     box_control = rje::expit(tot_box_control)
#   ) |>
#   ungroup()

library(timeR)

timer <- timeR::createTimer()
timer$start('test')
sc_shoot_data <- sp_tracking_data |>
  # just do one game for now
  group_by(game_id) |>
  group_split() |>
  furrr::future_map(\(game_df){
  
    game_df |>
      group_by(sl_event_id) |>
      # group_split(game_id, sl_event_id, tracking_player_id) |>
      group_split() |>
      purrr::map(\(df, rink_grid){
        # timer <- timeR::createTimer()
        # timer$start('test')
        
        # get triangle bounds
        
        # right goal post (shooter's perspective)
        lower_bound_slope <- (df$puck_y_adj[1] + 3)/(df$puck_x_adj[1] - 89)
        # left goal post (shooter's perspective)
        upper_bound_slope <- (df$puck_y_adj[1] - 3)/(df$puck_x_adj[1] - 89)
        
        # intercept of line from puck to right goal post
        lower_bound_intercept <- df$puck_y_adj[1] - lower_bound_slope*(df$puck_x_adj[1])
        # intercept of line from puck to left goal post
        upper_bound_intercept <- df$puck_y_adj[1] - upper_bound_slope*(df$puck_x_adj[1])
        
        # boxes that are in the triangle at least partially
        boxes <- c()
        
        # get every box in the triangle
        for (box in rink_grid$box_id) {
          
          # print(box)
          
          # get box bounds
          y_min <- rink_grid |>
            filter(box_id == box) |>
            pull(y_min)
          y_max <- rink_grid |>
            filter(box_id == box) |>
            pull(y_max)
          x_min <- rink_grid |>
            filter(box_id == box) |>
            pull(x_min)
          x_max <- rink_grid |>
            filter(box_id == box) |>
            pull(x_max)
          
          # is any piece of box in shooting triangle?
          if ( ((x_min > df$puck_x_adj[1]) & (x_min < 89)) &
              ( (y_min < (upper_bound_intercept + upper_bound_slope*x_min)) |
               (y_min < (upper_bound_intercept + upper_bound_slope*x_max)) ) &
              (y_max > (lower_bound_intercept + lower_bound_slope*x_min) |
               y_max > (lower_bound_intercept + lower_bound_slope*x_max)) 
               ) {
            
            boxes <- append(boxes, box)
              
          }
        }
        
        df |>
          mutate(
            boxes = paste(boxes, collapse = "_"),
            n_boxes = length(boxes),
            upper_int = upper_bound_intercept,
            upper_slope = upper_bound_slope,
            lower_int = lower_bound_intercept,
            lower_slope = lower_bound_slope
          )
      
      }, rink_grid = rink_grid) |>
      purrr::list_rbind() |>
      rowwise() |>
      group_split() |>
      purrr::map(\(df, rink_grid){
        
        mu <- get_player_influence_mu(player_x = df$tracking_x_adj,
                                      player_y = df$tracking_y_adj,
                                      player_v_x = df$tracking_vel_x_adj,
                                      player_v_y = df$tracking_vel_y_adj)
        cov <- get_player_influence_cov(player_v_x = df$tracking_vel_x_adj, 
                                        player_v_y = df$tracking_vel_y_adj,
                                        player_x = df$tracking_x_adj,
                                        player_y = df$tracking_y_adj,
                                        puck_x = df$puck_x_adj,
                                        puck_y = df$puck_y_adj)
        boxes_integrated <- c()
        
        boxes <- str_split(df$boxes, '_') |>
          unlist()
        
        print(boxes)
        
        for (box in boxes) {
          
          print(box)
          
          if (box == '') {
            next
          }
          
          integ_box <- mvtnorm::pmvnorm(upper = c(rink_grid[rink_grid$box_id == box,]$x_max,
                                                  rink_grid[rink_grid$box_id == box,]$y_max),
                                        lower = c(rink_grid[rink_grid$box_id == box,]$x_min,
                                                  rink_grid[rink_grid$box_id == box,]$y_min),
                                        mean = mu,
                                        sigma = cov)
          boxes_integrated <- append(boxes_integrated, integ_box)
        }
        
        # is this person in the triangle?
        if (((df$tracking_x_adj > df$puck_x_adj) & (df$tracking_x_adj < 89)) &
            (df$tracking_y_adj > (df$lower_int + df$lower_slope*df$tracking_x_adj)) &
            (df$tracking_y_adj < (df$upper_int + df$upper_slope*df$tracking_x_adj))
        ) {
          in_triangle <- 1
        } else {
          in_triangle <- 0
        }
        
        df |>
          mutate(boxes_integrated = sum(boxes_integrated),
                 in_triangle = in_triangle)
      }, rink_grid = rink_grid) |>
      purrr::list_rbind()
    
  }) |>
  purrr::list_rbind()
timer$stop('test')

saveRDS(sc_shoot_data, here('data', 'datasets', 'sc_shoot_data.rds'))
  
# sc_shoot_data <- sc_shoot_data |>
#   group_by(game_id, sl_event_id) |>
#   mutate(# rink_control = rje::expit(sum(if_else(tracking_team_id == possession_team_id, I_i, 0)) -
#     # sum(if_else(tracking_team_id != possession_team_id, I_i, 0)))
#     pos_tri_control = sum(if_else(tracking_team_id == possession_team_id, boxes_integrated, 0)),
#     neg_tri_control = sum(if_else(tracking_team_id != possession_team_id, boxes_integrated, 0)),
#     tot_tri_control = pos_tri_control - neg_tri_control,
#     tri_control = rje::expit(tot_tri_control)
#   ) |>
#   ungroup()

# prob_shoot_model <- gam(data = sc_shoot_data |>
#                           mutate(is_shot = if_else(event_type == 'shot', 1, 0)) |>
#                           select(is_shot, x_adj, y_adj, tri_control, cone_people),
#                         formula = is_shot ~ te(x_adj,y_adj) + s(x_adj) + s() + s(),
#                         family = "binomial")
# actual <- sc_shoot_data |>
#   mutate(is_shot = if_else(event_type == 'shot', 1, 0))
# 
# calibration_data <- tibble(pred = predict(prob_shoot_model), actual)
# 
# boxes <- tibble(box_id = (sc_shoot_data$boxes[1] |> stringr::str_split(pattern = '_'))[[1]],
#                 present = 1)
# 
# grid_data <- rink_grid |> 
#   left_join(boxes, by = 'box_id') |>
#   mutate(present = if_else(is.na(present), 0, present),
#          x_center = (x_min + x_max)/2,
#          y_center = (y_min + y_max)/2)
# geom_hockey(league = "AHL",
#             display_range = "offense") +
#   geom_rect(
#     data = grid_data,
#     aes(xmin = x_min, xmax = x_max,
#         ymin = y_min, ymax = y_max,
#         fill = present),
#     alpha = .5,
#     color = "black",
#     linewidth = 0.3
#   ) +
#   # upper
#   geom_abline(intercept = 58.00127, slope = -0.6854076) +
#   # lower
#   geom_abline(intercept = 53.52542, slope = -0.5677014) +
#   geom_point(data = tibble(x = 38.0256, y = 31.93824), aes(x = x, y = y)) +
#   geom_text(data = grid_data, aes(x = x_center, y = y_center, label = box_id), 
#             color = "black", size = 3) +
#   geom_point(data = sc_shoot_data, aes(x = tracking_x_adj, y = tracking_y_adj, color = tracking_team_id))
