# get usable xT basic dataframe for plotting + showing off

library(here)
library(dplyr)
library(tidyr)

source(here('docs', 'misc_helper_fns.R'))
source(here('data', 'data-files', 'data_loader.R'))
source(here('docs', 'xt_basic.R'))

xT_pos_5v5 <- get_xT_data_basic()
pos_5v5 <- get_possessions_5v5()

xT_basic_data <- xT_pos_5v5 |>
  left_join(box_shot_probs |>
              select(box_id, shot_prob),
            join_by(box_id)) |>
  left_join(box_goal_probs |>
              select(box_id, goal_prob),
            join_by(box_id)) |>
  mutate(move_prob = 1 - shot_prob) |>
  rowwise() |>
  group_split() |>
  purrr::imap(\(row, idx, trans_mat, xT_tbl){
    print(idx)
    tp_xT_sum <- trans_mat |>
      filter(from_box == row$box_id) |>
      left_join(xT_tbl, join_by(to_box == box_id)) |>
      mutate(tp_xT = transition_prob*xT) |>
      pull(tp_xT) |>
      sum()
      
    row |>
      mutate(tp_xT_sum = tp_xT_sum)
      
  }, trans_mat, xT_curr) |>
  purrr::list_rbind() |>
  mutate(xT = shot_prob*goal_prob + move_prob*tp_xT_sum) |>
  group_by(game_id, possession_id) |>
  mutate(xT_next = case_when(!is.na(lead(xT)) ~ lead(xT),
                             is.na(lead(xT)) & event_type == 'shot' ~ goal_prob,
                             T ~ NA),
         change_xT = xT_next - xT)
  
xT_basic_data |>
  # filter(str_detect(player_name, 'Griffith')) |> View()
  group_by(player_id, player_name) |>
  summarize(total_xT = sum(change_xT, na.rm = T),
            avg_xT = ,
            ) |>
  ungroup() |>
  arrange(desc(total_xT)) |>
  mutate(rank = 1:n()) |> 
  # filter(str_detect(player_name, 'Grimaldi'))
  View()

saveRDS(xT_basic_data, here('data', 'datasets', 'xT_basic_data.rds'))

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
