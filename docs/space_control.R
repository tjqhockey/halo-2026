# PURPOSE: Define spcae control functions to implement Wide Open Spaces version
# of space control

get_player_influence_mu <- function(player_x, player_y,
                                    player_v_x, player_v_y) {
  c(player_x, player_y) + .5*c(player_v_x, player_v_y)
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
  (player_speed^2)/(max_speed^2)
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

get_player_influence_cov <- function(player_v_x, player_v_y,
                                        player_x, player_y,
                                        puck_x, puck_y) {
  
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
  
  return(cov)
  
}

get_player_rink_influence <- function(rink_point, player_x, player_y, mu, cov) {
  # influence at point of interest
  f_i_point = mvtnorm::dmvnorm(x = rink_point, mean = mu, sigma = cov)
  
  # normalizing constant
  # this is what Luke Bornn says to do but it makes no sense
  # f_i_skater = mvtnorm::dmvnorm(x = c(player_x, player_y), mean = mu, sigma = cov)
  # so instead we do this
  f_i_skater = mvtnorm::dmvnorm(x = mu, mean = mu, sigma = cov)
  
  # normalized influence at point of interest for skater of interest
  I_i = f_i_point/f_i_skater
  
  return(I_i)
}

