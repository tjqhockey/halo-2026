library(here)
library(dplyr)
library(sportyR)
library(ggplot2)

events <- file.path(here(), 'data', 'events.parquet') |>
  arrow::read_parquet()

games <- file.path(here(), 'data', 'games.parquet') |>
  arrow::read_parquet()

players <- file.path(here(), 'data', 'players.parquet') |>
  arrow::read_parquet()

stints <- file.path(here(), 'data', 'stints.parquet') |>
  arrow::read_parquet()

tracking <- file.path(here(), 'data', 'tracking.parquet') |>
  arrow::read_parquet()


library(dplyr)
library(ggplot2)
library(stringr)

tracking |>
  mutate(theta = atan2(tracking_vel_y, tracking_vel_x),
         speed = sqrt(tracking_vel_x^2 + tracking_vel_y^2)
         ) |>
  filter((speed < 37 & !is.na(player_id)) | (is.na(player_id) & speed < 165))
  # nrow()
  # arrange(tracking_vel_y) |>
  # slice(1:10) |>
  # left_join(games, by = 'game_id') |>
  # left_join(events, by = c('sl_event_id', 'game_id')) |>
  # View()

get_mu_player_influence <- function(player_x, player_y,
                                    player_v_x, player_v_y) {
  c(pi_x, pi_y) + .5*c(v_x, v_y)
}

get_player_dist_from_puck <- function(player_x, player_y,
                                      puck_x, puck_y) {
  sqrt((puck_x - player_x)^2 + (puck_y - player_y)^2)
}

get_player_max_radius <- function(dist_from_puck) {
  # highly arbitrary ice control surface radius function
  max(3 + (dist_from_puck/6)^2, 15)
}

get_player_angle <- function(player_v_x, player_v_y) {
  atan2(player_v_y, player_v_x)
}

get_player_speed <- function(player_v_x, player_v_y) {
  sqrt(player_v_x^2 + player_v_y^2)
}

# get scaling factors for player gaussian cov matrices
get_player_s_rat <- function(player_speed, 
                             # max possible skating speed in ft/s
                             max_speed = 36.67){
  (speed^2)/(max_speed^2)
}

get_player_s_mat <- function(R, S_rat) {
  matrix(c(.5 * R * (1 + S_rat), 0, 0, .5 * R * (1 - S_rat)), nrow = 2, byrow = T)
}

get_player_rotation_mat <- function(theta) {
  matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), nrow = 2, byrow = T)
}

get_player_cov_mat <- function(R_theta, S){
  R_theta %*% S %*% S %*% solve(R_theta)
}

get_player_influence_params <- function(player_v_x, player_v_y,
                                        player_x, player_y,
                                        puck_x, puck_y) {
  
  mu = get_mu_player_influence(player_x = player_x,
                                      player_y = player_y)
  
  dist_from_puck = get_player_dist_from_puck(player_x = player_x,
                                             player_y = player_y,
                                             puck_x = puck_x,
                                             puck_y = puck_y)
  
  R = get_player_max_radius(dist_from_puck = dist_from_puck)
  
  theta = get_player_angle(player_v_x = player_v_x,
                           player_v_y = player_v_y)
  
  player_speed = get_player_speed(player_v_x = player_v_x,
                                  player_v_y = player_v_y)
  
  S_rat = get_player_s_rat(player_speed = player_speed)
  
  S = get_player_s_mat(R = R, S_rat = S_rat)
  
  R_theta = get_player_rotation_mat(theta = theta)
  
  cov = get_player_cov_mat(R_theta = R_theta, S = S)
  
  return(list(mu = mu, cov = cov))
  
}

tracking |>
  

rink_grid <- expand.grid(seq(0, 200, length.out = 200), seq(0, 80, length.out = 200)) %>%
  rename(p_x = Var1, p_y = Var2)

parms <- get_player_influence_params(player_v_x = 4.497199,
                            player_v_y = 4.497199,
                            player_x = 15,
                            player_y = 15,
                            puck_x = 3,
                            puck_y = 15)

mu <-

get_rink_influence(rink_grid, player_x, player_y, mu, cov) {
  # influence at point of interest
  f_i_point = mvtnorm::dmvnorm(x = as.matrix(rink_grid), mean = mu, sigma = cov)
  
  # normalizing constant
  f_i_skater = mvtnorm::dmvnorm(x = c(player_x, player_y), mean = mu, sigma = cov)
  
  # normalized influence at point of interest for skater of interest
  I_i = f_i_point/f_i_skater
  
  return(I_i)
}

data.grid <- expand.grid(s.1 = seq(0, 30, length.out=200), s.2 = seq(0, 30, length.out=200))
q.samp <- cbind(data.grid, prob = mvtnorm::dmvnorm(data.grid,
                                                   mean = mu_i,
                                                   sigma = cov_i)/
                  mvtnorm::dmvnorm(c(pi_x, pi_y),
                                     mean = mu_i,
                                     sigma = cov_i)
                  )

ggplot() +
  geom_raster(data = q.samp, aes(x = s.1, y = s.2, fill = prob)) +
  geom_contour(data = q.samp, aes(x = s.1, y = s.2, z = prob)) +
  geom_point(data = tibble(x = puck_x, y = puck_y), aes(x = x, y = y)) +
  geom_segment(aes(x = pi_x, y = pi_y, xend = pi_x + v_x, yend = pi_y + v_y),
               arrow = arrow()
  ) +
  geom_point(data = tibble(x = pi_x, y = pi_y), aes(x = x, y = y), color = 'red', size = 5) +
  coord_fixed()

v_x = 4.497199
v_y = 4.497199
pi_x = 15
pi_y = 15
puck_x = 3
puck_y = 15

v_x = 0
v_y = 0
pi_x = 15
pi_y = 15
puck_x = 15
puck_y = 15


ggplot(q.samp, aes(x=s.1, y=s.2, z=prob)) + 
  geom_contour() +
  coord_fixed(xlim = c(-3, 3), ylim = c(-3, 3), ratio = 1) 

events |>
  filter(event_type == 'pass') |>
  distinct(event_type, flags) |> View()

events |> 
  filter(!is.na(sl_xg_all_shots)) |>
  mutate(pred = sl_xg_all_shots,
         actual = if_else(str_detect(flags, 'withgoal'), 1, 0)) |>
  mutate(interval = cut(pred, breaks = seq(0, 1, by = .1), include.lowest = T)) |>
  group_by(interval) |>
  summarize(int_pred = mean(pred),
            int_actual = mean(actual),
            se = sqrt((int_actual*(1-int_actual))/n()),
            n = n(),
            ci_lower = pmax(int_actual - 2*se, 0),
            ci_upper = pmin(int_actual + 2*se, 1)) |>
  ggplot(aes(x = int_pred, y = int_actual)) + 
  geom_point(alpha = 0.5, aes(size = n)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper)) +
  geom_smooth(se = FALSE, method = 'loess') +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(x = "Predicted probability", y = "Observed fraction", size = 'Count') +
  ylim(0, 1) +
  theme(text=element_text(size=15))
  
### space ownership

#### BDC 22

stick <- 6.5
time_penalty <- 1/10
max_velocity <- 36.5#30 # maximum skater velocity in ft/sec
a <- 1.3 # acceleration coefficient (not directly acceleration, but more like a speed decay)
tr <- 0.189 # reaction time (based on the article Phil sent)
t_max <- 15 # Maximum value used to use for numerically solving arrival times 
mm <- 0.1 # Coefficient of friction between puck and ice, I'll find the source for this
b <- 0.1322 # Puck air drag coefficient (actuall it's the coefficient divided by the mass so beta = k/m if k is the drag coefficient)
b2 <- 2.5 # pitch control coefficient used as beta in ice_ctrl_xyt and teamwise_ice_ctrl_xyt, taken from the Spearman paper
gg <- 32.174 # g (as in the gravity acceleration in ft/s^2)
x_decay <- 2000 #value used as decay_x
y_decay <- 500 #value used as decay_y
t_rez <- 1/100
goalie_dist <- 8 # maximum reasonable distance for goalie to go away from goal

current_track_m <- tracking |> 
  filter(game_id == '00b0366a-95c6-5250-2dae-e3dd5c4198bc', sl_event_id == 4) |>
  mutate(theta = atan2(tracking_vel_y, tracking_vel_x),
         speed = sqrt(tracking_vel_x^2 + tracking_vel_y^2)
  ) |>
  filter((speed < 37 & !is.na(player_id)) | (is.na(player_id) & speed < 165)) |>
  left_join(games |>
              select(game_id, game_date), by = 'game_id') |>
  left_join(events |>
              select(game_id, sl_event_id, event_type, outcome),
            by = c('sl_event_id', 'game_id')) |>
  full_join(events |>
              select(game_id, sl_event_id, tracking_x = x, tracking_y = y, x_adj, y_adj) |>
              mutate(player_id = 'puck'),
            by = c('sl_event_id', 'game_id', 'player_id'))


tracking |>
  mutate(theta = atan2(tracking_vel_y, tracking_vel_x),
         speed = sqrt(tracking_vel_x^2 + tracking_vel_y^2)
  ) |>
  filter((speed < 37 & !is.na(player_id)) | (is.na(player_id) & speed < 165))
# nrow()
# arrange(tracking_vel_y) |>
# slice(1:10) |>
# left_join(games, by = 'game_id') |>
# left_join(events, by = c('sl_event_id', 'game_id')) |>
# View()

## expected threat baseline model


# create transition_matrix 


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
  left_join(rink_grid, join_by(x_adj >= x_min, x_adj < x_max,
                               y_adj >= y_min, y_adj < y_max)) |>
  mutate(box_id = stringr::str_c(x_box_id, y_box_id, sep = '-')) 

possessions <- boxed_events |>
  group_by(game_id, sequence_id) |>
  arrange(sl_event_id) |>
  # possession starts when team recovers puck after faceoff, not when faceoff is won
  mutate(possession_team = if_else(event_type == 'lpr' & outcome == 'successful', team, NA)) |>
  fill(possession_team, .direction = 'down') |>
  filter(!is.na(possession_team))

# want unique ids of uninterrupted possessions (i.e. other team does not gain possession, whistle not blown)
library(tidyr)
transition_mat <- possessions |>
  group_by(game_id) |>
  arrange(sl_event_id) |>
  # possession id is only unique to each sequence
  mutate(possession_id = consecutive_id(possession_team)) |>
  group_by(game_id, possession_id) |>
  # select(game_id, period, period_time, sl_event_id, sequence_id,
  #        player_id, player_name, team,
  #        event_type, outcome, flags,
  #        description, detail, possession_team, possession_id) |>
  mutate(prev_box = lag(box_id)) |>
  # filter(game_id == '00b0366a-95c6-5250-2dae-e3dd5c4198bc') |>
  filter(!is.na(prev_box)) |>
  group_by(prev_box, box_id) |>
  summarize(count = n()) |>
  group_by(prev_box) |> 
  mutate(total_prev = sum(count),
         transition_prob = count/total_prev) |>
  select(prev_box, box_id, transition_prob) |>
  pivot_wider(names_from = box_id, values_from = transition_prob) |>
  tibble::column_to_rownames('prev_box') |>
  mutate(across(everything(), replace_na, 0))

events |>
  filter(event_type == 'icing') |> View()

# shooting and moving (pass or skate) probabilities for each box

possessions |> 
  group_by(game_id, sequence_id) |>
  
# weird things to deal with -> double faceoffs
  
  
events |>
  filter(game_id == '0f1f9e60-c5cc-30e1-f520-e92f2da2ae08', sequence_id == 1) |>
  group_by(period) |>
  summarize(n())
  

geom_hockey(league = "NHL", display_range = "full", rink_units = 'ft') +
  geom_point(data = tibble(x = -105, y = 17), aes(x = x, y = y))
  
(tracking |>
  filter(tracking_x <= 100, tracking_x >= -100, tracking_y <= 42.5, tracking_y >= -42.5,
         ) |>
  nrow())/nrow(tracking)

