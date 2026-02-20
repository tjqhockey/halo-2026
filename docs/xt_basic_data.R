# get usable xT basic dataframe for plotting + showing off

library(here)
library(dplyr)
library(tidyr)

source(here('docs', 'misc_helper_fns.R'))
source(here('data', 'data-files', 'data_loader.R'))
source(here('docs', 'xt_basic.R'))


# Fit GAMS ----------------------------------------------------------------

sp_data_basic <- get_sp_data_basic()
gp_data_basic <- get_gp_data_basic()

xG_model <- gam(data = gp_data_basic |>
                  mutate(is_goal = if_else(str_detect(flags, 'withgoal'), 1, 0)),
                formula = is_goal ~ te(x_adj,y_adj),
                family = "binomial")

sp_model <- gam(data = sp_data_basic |>
                  mutate(xG_plus = predict(xG_model, newdata = sp_data_basic, type = 'response'),
                         is_shot = if_else(event_type == 'shot', 1, 0)),
                formula = is_shot ~ te(x_adj,y_adj) + s(xG_plus),
                family = "binomial")

# Fit space control models ------------------------------------------------

sc_shoot_data_raw <- readRDS(here('data', 'datasets', 'sc_shoot_data.rds'))

sc_shoot_data <- sc_shoot_data_raw |>
  group_by(game_id, sl_event_id, event_type, flags, x_adj, y_adj) |>
  summarize(# rink_control = rje::expit(sum(if_else(tracking_team_id == possession_team_id, I_i, 0)) -
    # sum(if_else(tracking_team_id != possession_team_id, I_i, 0)))
    pos_tri_control = sum(if_else(tracking_team_id == possession_team_id, boxes_integrated, 0)),
    neg_tri_control = sum(if_else(tracking_team_id != possession_team_id, boxes_integrated, 0)),
    tot_tri_control = pos_tri_control - neg_tri_control,
    tri_control = rje::expit(tot_tri_control),
    n_players_tri = sum(in_triangle)
  ) |>
  ungroup()

sc_gp_data <- sc_shoot_data |>
  filter(event_type == 'shot') |>
  mutate(is_goal = if_else(str_detect(flags, 'withgoal'), 1, 0))

xG_model_sc <- gam(data = sc_gp_data,
                formula = is_goal ~ te(x_adj,y_adj, tri_control) + s(n_players_tri, k = 5),
                family = "binomial")

sc_gp_data <- sc_gp_data |>
  mutate(xG_sc = predict(xG_model_sc, type = 'response') |> as.vector())
  
sp_model_sc <- gam(data = sc_shoot_data |>
                  mutate(is_shot = if_else(event_type == 'shot', 1, 0),
                         xG_sc = predict(xG_model, newdata = sc_shoot_data, type = 'response')),
                formula = is_shot ~ te(x_adj,y_adj, tri_control) + s(n_players_tri, k = 5) + s(xG_sc),
                family = "binomial")

sc_shoot_data <- sc_shoot_data |>
  mutate(sp_sc = predict(sp_model_sc, type = 'response') |> as.vector())

# Make dataset ------------------------------------------------------------

# xT_pos_5v5 <- get_xT_data_basic()
# pos_5v5 <- get_possessions_5v5()

# xT_basic_data <- xT_pos_5v5 |>
#   left_join(box_shot_probs |>
#               select(box_id, shot_prob),
#             join_by(box_id)) |>
#   left_join(box_goal_probs |>
#               select(box_id, goal_prob),
#             join_by(box_id)) |>
#   mutate(move_prob = 1 - shot_prob) |>
#   rowwise() |>
#   group_split() |>
#   purrr::imap(\(row, idx, trans_mat, xT_tbl){
#     print(idx)
#     tp_xT_sum <- trans_mat |>
#       filter(from_box == row$box_id) |>
#       left_join(xT_tbl, join_by(to_box == box_id)) |>
#       mutate(tp_xT = transition_prob*xT) |>
#       pull(tp_xT) |>
#       sum()
#       
#     row |>
#       mutate(tp_xT_sum = tp_xT_sum)
#       
#   }, trans_mat, xT_curr) |>
#   purrr::list_rbind() |>
#   mutate(xT = shot_prob*goal_prob + move_prob*tp_xT_sum) |>
#   group_by(game_id, possession_id) |>
#   mutate(xT_next = case_when(!is.na(lead(xT)) ~ lead(xT),
#                              is.na(lead(xT)) & event_type == 'shot' ~ goal_prob,
#                              T ~ NA),
#          change_xT = xT_next - xT)

xT_basic_data <- readRDS(here('data', 'datasets', 'xT_basic_data.rds'))

xT_basic_data <- xT_basic_data |>
  ungroup() |>
  mutate(xG_plus = predict(xG_model, newdata = xT_basic_data, type = 'response') |>
           as.vector()) |>
  left_join(sc_gp_data |> 
              select(game_id, sl_event_id, xG_sc),
            join_by(game_id, sl_event_id))

xT_basic_data <- xT_basic_data |>
  left_join(sc_shoot_data |> 
              select(game_id, sl_event_id, sp_sc),
            join_by(game_id, sl_event_id)) |>
  mutate(sp_plus = predict(sp_model, newdata = xT_basic_data, type = 'response') |>
           as.vector(),
         mp_plus = 1 - sp_plus,
         mp_sc = 1 - sp_sc,
         xT_plus = sp_plus*xG_plus + mp_plus*tp_xT_sum,
         xT_sc = sp_sc*xG_sc + mp_sc*tp_xT_sum,
         xT_sc = if_else(is.na(xT_sc), xT_plus, xT_sc)
         ) 
  
xT_basic_data <- xT_basic_data |>
  group_by(game_id, possession_id) |>
  mutate(
    xT_next_plus = case_when(
    event_type == "lpr" ~ 2*xT_plus,
    event_type == "shot" ~ xG_plus,
    !is.na(lead(xT_plus)) ~ lead(xT_plus),
    is.na(lead(xT_plus)) ~ 0,
    TRUE ~ 0),
    xT_next_sc = case_when(
      event_type == "lpr" ~ 2*xT_sc,
      event_type == "shot" ~ xG_sc,
      !is.na(lead(xT_sc)) ~ lead(xT_sc),
      is.na(lead(xT_sc)) ~ 0,
      TRUE ~ 0),
    change_xT_sc = xT_next_sc - xT_sc,
    xT_next = case_when(
      event_type == "lpr" ~ 2*xT,
      event_type == "shot" ~ goal_prob,
      !is.na(lead(xT)) ~ lead(xT),
      is.na(lead(xT)) ~ 0,
      TRUE ~ 0),
    change_xT = xT_next - xT)

# illustrative sc play

change_event_id <- sc_shoot_data |>
  filter(game_id == '18a44513-6719-9df6-3941-b5492e67ff48',
         sl_event_id == 3083) |>
  select(game_id, sl_event_id)

tracking_change_event <- sc_shoot_data_raw |>
  ungroup() |>
  inner_join(change_event_id, join_by(game_id, sl_event_id)) |>
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
    
    
    df |>
      mutate(mu = list(mu), cov = list(cov))
  }, rink_grid = rink_grid) |>
  purrr::list_rbind() |>
  left_join(events |> select(team_id, tracking_team = team) |>
              distinct(), 
            join_by(tracking_team_id == team_id))

puck_x <- tracking_change_event$x_adj[1]
puck_y <- tracking_change_event$y_adj[1]

shooter_x <- tracking_change_event |>
  filter(tracking_player_id == '7779d05b-bf33-9acd-a1a5-bb2e852f4a3f') |>
  pull(tracking_x_adj)
shooter_y <- tracking_change_event |>
  filter(tracking_player_id == '7779d05b-bf33-9acd-a1a5-bb2e852f4a3f') |>
  pull(tracking_y_adj)

boxes <- tibble(box_id = (tracking_change_event$boxes[1] |> stringr::str_split(pattern = '_'))[[1]],
                present = 1)

box_sc <- rink_grid |>
  filter(box_id %in% boxes$box_id) |>
  rowwise() |>
  group_split() |>
  purrr::map(\(box_df, tracking_change_event){
    
    box <- box_df$box_id
    
    pos_team_ids <- c()
    team_ids <- c()
    box_player_integrated <- c()
    player_ids <- c()
    
    for (i in 1:nrow(tracking_change_event)) {
      
      row <- tracking_change_event[i,]
      
      pos_team_ids <- append(pos_team_ids, row$possession_team_id)
      
      team_ids <- append(team_ids, row$tracking_team_id)
      
      player_ids <- append(player_ids, row$tracking_player_id)
      
      mu <- row$mu[[1]]
      cov <- row$cov[[1]]
      
      integ_box <- mvtnorm::pmvnorm(upper = c(box_df[box_df$box_id == box,]$x_max,
                                              box_df[box_df$box_id == box,]$y_max),
                                    lower = c(rink_grid[rink_grid$box_id == box,]$x_min,
                                              rink_grid[rink_grid$box_id == box,]$y_min),
                                    mean = mu,
                                    sigma = cov)
      box_player_integrated <- append(box_player_integrated, integ_box)
    }
    
    tibble(box_id = box,
           possession_team_id = pos_team_ids,
           tracking_team_id = team_ids,
           integ_box = box_player_integrated)
    
  }, tracking_change_event = tracking_change_event) |>
  purrr::list_rbind() |>
  group_by(box_id) |>
  summarize(pos_box_control = sum(if_else(tracking_team_id == possession_team_id, integ_box, 0)),
         neg_box_control = sum(if_else(tracking_team_id != possession_team_id, integ_box, 0)),
         tot_box_control = pos_box_control - neg_box_control,
         box_control = rje::expit(tot_box_control))

library(sportyR)

rink_grid <- grid_the_rink() |>
  mutate(box_id = str_c(x_box_id, y_box_id, sep = '-')) |>
  select(-x_box_id, -y_box_id)

grid_data <- rink_grid |>
  left_join(boxes, by = 'box_id') |>
  left_join(box_sc, by = 'box_id') |>
  mutate(present = if_else(is.na(present), 0, present),
         # alpha_val = if_else(present == 1, .5, .5),
         x_center = (x_min + x_max)/2,
         y_center = (y_min + y_max)/2)

geom_hockey(league = "AHL",
            display_range = "offense") +
  geom_rect(
    data = grid_data,
    aes(xmin = x_min, xmax = x_max,
        ymin = y_min, ymax = y_max,
        fill = box_control),
    alpha = .5,
    color = "black",
    linewidth = 0.3
  ) +
  scale_fill_gradient2(
    low = "blue",      
    mid = "white",   
    high = "red", 
    midpoint = .5,
    limits = c(.3, .7)
  ) +
  geom_point(data = tracking_change_event |> 
               distinct(x_adj, y_adj), aes(x = x_adj, y = y_adj), size = 3) +
  geom_point(data = tracking_change_event, aes(x = tracking_x_adj,
                                               y = tracking_y_adj,
                                               color = tracking_team),
             size = 8,
             alpha = 0.9) +
  # theme(legend.position = "none") +
  # upper triangle bound
  geom_segment(aes(x = puck_x, y = puck_y, xend = 89, yend = 3),
               linewidth = 1, linetype = 'dashed') +
  # lower triangle bound
  geom_segment(aes(x = puck_x, y = puck_y, xend = 89, yend = -3),
               linewidth = 1, linetype = 'dashed') +
  # stick
  geom_segment(aes(x = puck_x, y = puck_y, xend = shooter_x, yend = shooter_y),
               linewidth = 1) +
  labs(fill = 'Offensive Box Control',
       color = 'Team')

# leaderboard
  
xT_basic_data |>
  # filter(str_detect(player_name, 'Griffith')) |> View()
  group_by(player_id, player_name) |>
  summarize(total_xT = sum(change_xT, na.rm = T)) |>
  ungroup() |>
  arrange(desc(total_xT)) |>
  mutate(rank = 1:n()) |> 
  # filter(str_detect(player_name, 'Grimaldi'))
  inner_join(xT_basic_data |>
               # filter(str_detect(player_name, 'Griffith')) |> View()
               group_by(player_id, player_name) |>
               summarize(total_xT_sc = sum(change_xT_sc, na.rm = T)) |>
               ungroup() |>
               arrange(desc(total_xT_sc)) |>
               mutate(rank_sc = 1:n()),
             join_by(player_id)) |> 
  mutate(change_sc = total_xT_sc - total_xT) |>
  arrange(desc(abs(change_sc))) |>
  View()
  
# saveRDS(xT_basic_data, here('data', 'datasets', 'xT_basic_data.rds'))

# optimal xT:
# kind of a failed experiment

# we should be able to get optimal most likely move from any box (unconditional on
# positioning, of course)

# from_box <- c()
# optimal_to_box <- c()
# from_xT <- c()
# to_xT <- c()
# 
# for (cur_box in all_box_ids) {
# 
#   from_box <- append(from_box, cur_box)
# 
#   s_p <- box_shot_probs[box_shot_probs$box_id == cur_box, ]$shot_prob
#   g_p <- box_goal_probs[box_goal_probs$box_id == cur_box, ]$goal_prob
#   m_p <- 1 - s_p
# 
#   tp_xt <- trans_mat |>
#     filter(from_box == cur_box) |>
#     left_join(xT_curr,
#               join_by(to_box == box_id)) |>
#     mutate(tp_xT = transition_prob*xT)
#   
#   summed_tp_xt <- tp_xt |>
#     pull(tp_xT) |>
#     sum()
#   
#   xT_cur_box <- s_p*g_p + m_p*summed_tp_xt
#   
#   from_xT <- append(from_xT, xT_cur_box)
#   
#   print(tp_xt |>
#            arrange(desc(tp_xT)))
#     
#   optimal_pot_box <- tp_xt |>
#     arrange(desc(tp_xT)) |>
#     slice(1) |>
#     pull(to_box)
#   
#   optimal_to_box <- append(optimal_to_box, optimal_pot_box)
#     
#   s_p_pot <- box_shot_probs[box_shot_probs$box_id == optimal_pot_box, ]$shot_prob
#   g_p_pot <- box_goal_probs[box_goal_probs$box_id == optimal_pot_box, ]$goal_prob
#   m_p_pot <- 1 - s_p_pot
#   
#   summed_tp_xt_pot <- trans_mat |>
#     filter(from_box == optimal_pot_box) |>
#     left_join(xT_curr,
#               join_by(to_box == box_id)) |>
#     mutate(tp_xT = transition_prob*xT) |>
#     pull(tp_xT) |>
#     sum()
#     
#   xT_pot_box <- s_p_pot*g_p_pot + m_p_pot*summed_tp_xt_pot
#   
#   to_xT <- append(to_xT, xT_pot_box)
# 
# }
# 
# optimal_xTs <- tibble(from_box = from_box,
#                       optimal_to_box = optimal_to_box,
#                       from_xT = from_xT,
#                       to_xT = to_xT) |>
#   mutate(change_xT = to_xT - from_xT)
