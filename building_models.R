library(here)
library(tidyverse)
library(mgcv)

# Loading Data
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

## Adjusting DF's as needed
set.seed(22)
shots <- events |>
  filter(event_type == "shot") |>
  mutate(is_goal = case_when(
    str_detect(flags,"withgoal") == TRUE ~ 1,
    TRUE ~ 0),
    fold = sample(1:5,n(),replace=TRUE),
    test_pred_xg = NA,
    test_prob_shoot = NA
  )

events <- events |>
  mutate(is_shot = case_when(
    event_type == "shot" ~ 1,
    TRUE ~ 0),
    fold = sample(1:5,n(),replace=TRUE),
    test_pred_xg = NA,
    test_prob_shoot = NA
    )

## Fitting models on full dataset
# xg_model <- gam(data=shots,
#                 formula= is_goal ~ te(x_adj,y_adj),
#                 family="binomial")
# prob_to_shoot_model <- gam(data=events,
#                            formula= is_shot ~ te(x_adj,y_adj),
#                            family="binomial")

## 5-fold CV to test OOS calibration
## Very simple-- no adjustment for dependent observations or anything ATM

## xG first
for (i in 1:5) {
  test <- shots |>
    filter(fold == i)
  train <- shots |>
    filter(fold != i)
  xg_model <- gam(data=train,
                  formula= is_goal ~ te(x_adj,y_adj),
                  family="binomial")
  shots$test_pred_xg[shots$fold == i] <- 
    predict(object=xg_model,
            newdata = shots[shots$fold == i, ],
            type = "response")
  print(paste("done with fold", i))
}

## Probability to shoot
for (i in 1:5) {
  test <- events |>
    filter(fold == i)
  train <- events |>
    filter(fold != i)
  prob_shoot_model <- gam(data=train,
                  formula= is_shot ~ te(x_adj,y_adj),
                  family="binomial")
  events$test_prob_shoot[events$fold == i] <- 
    predict(object=prob_shoot_model,
            newdata = events[events$fold == i, ],
            type = "response")
  print(paste("done with fold", i))
}

## Calibration plots
shots |>
  mutate(interval = cut(test_pred_xg, breaks = seq(0, 1, by = .1),
                        include.lowest = T)) |>
  group_by(interval) |>
  summarize(int_pred = mean(test_pred_xg),
            int_actual = mean(is_goal),
            se = sqrt((int_actual*(1-int_actual))/n()),
            n = n(),
            ci_lower = pmax(int_actual - 2*se, 0),
            ci_upper = pmin(int_actual + 2*se, 1)) |>
  ggplot(aes(x = int_pred, y = int_actual)) + 
  geom_point(alpha = 0.5, aes(size = n)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper)) +
  geom_smooth(se = FALSE, method = 'loess') +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(x = "Predicted probability", y = "Observed fraction",
       size = 'Count',title = "xG Calibration") +
  ylim(0, 1) +
  theme(text=element_text(size=15))


events |>
  mutate(interval = cut(test_prob_shoot, breaks = seq(0,1,b = .1),
                        include.lowest = T)) |>
  group_by(interval) |>
  summarize(int_pred = mean(test_prob_shoot),
            int_actual = mean(is_shot),
            se = sqrt((int_actual*(1-int_actual))/n()),
            n = n(),
            ci_lower = pmax(int_actual - 2*se, 0),
            ci_upper = pmin(int_actual + 2*se, 1)) |>
  ggplot(aes(x = int_pred, y = int_actual)) + 
  geom_point(alpha = 0.5, aes(size = n)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper)) +
  geom_smooth(se = FALSE, method = 'loess') +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(x = "Predicted probability", y = "Observed fraction",
       size = 'Count',title = "Probability to Shoot Calibration") +
  ylim(0, 1) +
  theme(text=element_text(size=15))
