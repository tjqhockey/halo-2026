## Run this file before Ridge Logistic Regression Model and Ridge Poisson Regression Model files

# data loading
library(here)
library(stringr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(mgcv)
library(arrow)
library(lubridate)

tracking <- read_parquet("HALO HACKATHON/tracking.parquet")
events <- read_parquet("HALO HACKATHON/events.parquet")
stints <- read_parquet("HALO HACKATHON/stints.parquet")
games <- read_parquet("HALO HACKATHON/games.parquet")
players <- read_parquet("HALO HACKATHON/players.parquet")

players_1 <- "2023-24_NHLPlayers.txt"
players_2 <- "2024-25_NHLPlayers.txt"
players_3 <- "2025-26_NHLPlayers.txt"

df_1 <- read.csv(here('halo-2026', 'data', 'data-files', players_1),
                 header = TRUE, stringsAsFactors = FALSE)
df_2 <- read.csv(here('halo-2026', 'data', 'data-files', players_2),
                 header = TRUE, stringsAsFactors = FALSE)
df_3 <- read.csv(here('halo-2026', 'data', 'data-files', players_3),
                 header = TRUE, stringsAsFactors = FALSE)

colnames(df_1) <- df_1[1, ]
df_1 <- df_1[2:nrow(df_1), ]
# df_1["-9999"] <- df_1["Additional"]

colnames(df_2) <- df_2[1, ]
df_2 <- df_2[2:nrow(df_2), ]
# df_2["-9999"] <- df_2["Additional"]

colnames(df_3) <- df_3[1, ]
df_3 <- df_3[2:nrow(df_3), ]
# df_3["-9999"] <- df_3["Additional"]

df_1[, c(3, 6:21, 24:30)] <- lapply(df_1[, c(3, 6:21, 24:30)], as.numeric)
df_2[, c(3, 6:21, 24:30)] <- lapply(df_2[, c(3, 6:21, 24:30)], as.numeric)
df_3[, c(3, 6:21, 24:30)] <- lapply(df_3[, c(3, 6:21, 24:30)], as.numeric)

df_1 <- df_1[df_1$Team != "2TM", ]
df_2 <- df_2[df_2$Team != "2TM", ]
df_3 <- df_3[df_3$Team != "2TM", ]

# look for promoted players

# --- 1. Normalize players table ("Last, First" -> "First Last") ---
players <- players |>
  mutate(
    player_clean = str_replace(player_name, "^([^,]+),\\s*(.*)$", "\\2 \\1"),
    player_clean = str_to_lower(str_trim(player_clean))
  )

# --- 2. Normalize each season df ("First Last") ---
clean_names <- function(df) {
  df |>
    mutate(
      player_clean = str_to_lower(str_trim(Player))
    ) |>
    pull(player_clean)
}

season_names <- c(
  clean_names(df_1),
  clean_names(df_2),
  clean_names(df_3)
)

season_names <- unique(season_names)

# --- 3. Add made_nhl flag ---
players <- players |>
  mutate(
    made_nhl = if_else(player_clean %in% season_names, 1, 0)
  )

# read in relevant coefficients

even_strength_data <- read.csv(here('halo-2026', 'data', 'datasets',
                                    'posterior_ratings_xg.csv'),
                               header = TRUE, stringsAsFactors = FALSE)

even_strength_data <- even_strength_data |>
  rename(
    xG_coef = mean,
    xG_coef_median = median,
    xG_coef_lower_80 = lower_80,
    xG_coef_upper_80 = upper_80
  )

special_teams_data <- read.csv(here('halo-2026', 'data', 'datasets',
                                    'special_teams_posterior_clean.csv'),
                               header = TRUE, stringsAsFactors = FALSE)

special_players <- special_teams_data |>
  filter(type %in% c("PP", "PK")) |>
  mutate(
    stint_type = str_to_lower(type),
    prefix = paste0("xG_", stint_type, "_coef")
  ) |>
  select(
    player_id,
    prefix,
    mean,
    median,
    X10.,
    X90.
  )

special_players_wide <- special_players |>
  pivot_wider(
    names_from = prefix,
    values_from = c(mean, median, X10., X90.)
  )

special_players_wide <- special_players_wide |>
  rename(
    xG_pp_coef          = mean_xG_pp_coef,
    xG_pk_coef          = mean_xG_pk_coef,
    xG_pp_coef_median   = median_xG_pp_coef,
    xG_pk_coef_median   = median_xG_pk_coef,
    xG_pp_coef_lower_80 = X10._xG_pp_coef,
    xG_pk_coef_lower_80 = X10._xG_pk_coef,
    xG_pp_coef_upper_80 = X90._xG_pp_coef,
    xG_pk_coef_upper_80 = X90._xG_pk_coef
  )

players <- players |>
  left_join(even_strength_data, by = "player_name") |>
  left_join(special_players_wide, by = "player_id")

players <- players |>
  mutate(age = floor(time_length(interval(birth_date, ymd("2023-09-01")), "years")))

promoted_players <- players[players$made_nhl == 1, ] |>
  distinct(player_id, .keep_all = TRUE)

gp_long <- bind_rows(
  df_1 |> select(Player, GP),
  df_2 |> select(Player, GP),
  df_3 |> select(Player, GP)
) |>
  mutate(
    player_clean = str_to_lower(str_trim(Player))
  )

gp_totals <- gp_long |>
  group_by(player_clean) |>
  summarise(total_gp = sum(GP, na.rm = TRUE))

promoted_players <- promoted_players |>
  left_join(gp_totals, by = "player_clean") |>
  rename(
    GP = total_gp
  )

# end data setup


### Initial Modeling Exploration (No CV)


# Logistic Regression: made_nhl ~ xG_coef*age +
## xG_pp_coef*age + xG_pk_coef*age + position_group
# Given made_nhl = 1:
## GAM: Games ~ xG_coef*age +
## xG_pp_coef*age + xG_pk_coef*age + position_group

log_reg <- glm(made_nhl ~ xG_coef*age + xG_pp_coef*age
               + xG_pk_coef*age + position_group,
               family = "binomial", data = players)

summary(log_reg)

promoted_glm <- glm(GP ~ xG_coef*age + xG_pp_coef*age +
                      xG_pk_coef*age + position_group,
                    family = poisson(),
                    data = promoted_players)

summary(promoted_glm)

## Patchwork Plot

mf_log <- model.frame(log_reg)

p1 <- ggplot(mf_log, aes(x = xG_coef, y = log_reg$fitted.values)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  theme_bw() +
  labs(x = "xG Coefficient Estimate", y = "Fitted Probability of Promotion",
       title = "Player Promotion Probabilities vs. Player xG Coefficients")

p2 <- ggplot(mf_log, aes(x = age, y = log_reg$fitted.values)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  theme_bw() +
  labs(x = "Age on 9/1/23", y = "Fitted Probability of Promotion",
       title = "Player Promotion Probabilities vs. Player Age")

p1 + p2

## Rest in Ridge Logistic Regression Model and Ridge Poisson Regression Model files
