## Ridge Poisson Games Played Model

library(glmnet)
library(purrr)
library(tidyverse)
library(ggplot2)

# Setting up data modeling

gp_dat <- promoted_players |>
  select(GP,
         xG_coef,
         xG_pp_coef,
         xG_pk_coef,
         age,
         position_group) |>
  filter(!is.na(GP),
         !is.na(xG_coef),
         !is.na(xG_pp_coef),
         !is.na(xG_pk_coef),
         !is.na(age),
         !is.na(position_group))

# Model matrix
X_gp <- model.matrix(
  GP ~ xG_coef*age +
    xG_pp_coef*age +
    xG_pk_coef*age +
    position_group,
  data = gp_dat
)[, -1]

y_gp <- gp_dat$GP

## CV

set.seed(1979)
n_gp <- nrow(gp_dat)
gp_dat$fold <- sample(rep(1:5, length.out = n_gp))

ridge_pois_cv_preds <-
  map_dfr(1:5, function(test_fold) {
    
    test_idx  <- which(gp_dat$fold == test_fold)
    train_idx <- which(gp_dat$fold != test_fold)
    
    X_train <- X_gp[train_idx, ]
    y_train <- y_gp[train_idx]
    X_test  <- X_gp[test_idx, ]
    
    # Ridge Poisson with internal lambda CV
    ridge_pois_fit <- cv.glmnet(
      x = X_train,
      y = y_train,
      family = "poisson",
      alpha = 0
    )
    
    test_pred_counts <- as.numeric(
      predict(ridge_pois_fit,
              newx = X_test,
              s = "lambda.min",
              type = "response")
    )
    
    tibble(
      pred_gp        = test_pred_counts,
      actual_gp      = y_gp[test_idx],
      fold           = test_fold,
      xG_coef        = gp_dat$xG_coef[test_idx],
      xG_pp_coef     = gp_dat$xG_pp_coef[test_idx],
      xG_pk_coef     = gp_dat$xG_pk_coef[test_idx],
      age            = gp_dat$age[test_idx],
      position_group = gp_dat$position_group[test_idx]
    )
  })

## Calibration and Evaluation

ggplot(ridge_pois_cv_preds,
       aes(x = pred_gp, y = actual_gp)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0,
              color = "red", linetype = "dashed") +
  theme_bw() +
  labs(y = "Observed GP", x = "Fitted GP",
       title = "Observed vs. Predicted Games Played for Promoted Players")

# Fixed breaks baseline ridge fit
baseline_fit <- cv.glmnet(
  x = X_gp_full,
  y = y_gp_full,
  family = "poisson",
  alpha = 0
)

baseline_pred <- as.numeric(
  predict(baseline_fit, newx = X_gp_full, s = "lambda.min", type = "response")
)

# Decile breaks
gp_breaks <- quantile(baseline_pred, probs = seq(0, 1, 0.1), na.rm = TRUE)

# Midpoints for plotting
gp_mids <- (gp_breaks[-1] + gp_breaks[-length(gp_breaks)]) / 2

## Bootstrapping

bootstrap_once_gp <- function(data, X, y, breaks) {
  
  idx <- sample(seq_len(nrow(data)), replace = TRUE)
  X_boot <- X[idx, ]
  y_boot <- y[idx]
  
  fit_boot <- cv.glmnet(
    x = X_boot,
    y = y_boot,
    family = "poisson",
    alpha = 0
  )
  
  pred_gp <- as.numeric(
    predict(fit_boot, newx = X, s = "lambda.min", type = "response")
  )
  
  tibble(
    pred_gp = pred_gp,
    actual_gp = y,
    bin = cut(pred_gp,
              breaks = breaks,
              include.lowest = TRUE,
              labels = FALSE)
  ) |>
    group_by(bin) |>
    summarize(
      mean_pred_gp = mean(pred_gp),
      mean_actual_gp = mean(actual_gp),
      .groups = "drop"
    )
}

set.seed(2026)
B <- 200

boot_list_gp <- map(1:B, ~ bootstrap_once_gp(
  gp_dat, X_gp_full, y_gp_full, gp_breaks
))

boot_df_gp <- bind_rows(
  lapply(seq_along(boot_list_gp), function(b) {
    boot_list_gp[[b]] |> mutate(boot = b)
  })
)

boot_bands_gp <- boot_df_gp |>
  group_by(bin) |>
  summarize(
    median_pred = median(mean_pred_gp),
    median_actual = median(mean_actual_gp),
    lower_95 = quantile(mean_actual_gp, 0.025),
    upper_95 = quantile(mean_actual_gp, 0.975),
    .groups = "drop"
  ) |>
  mutate(
    bin_mid = gp_mids[bin]
  )

ggplot(boot_bands_gp, aes(x = bin_mid)) +
  geom_ribbon(aes(ymin = lower_95, ymax = upper_95),
              fill = "skyblue", alpha = 0.35) +
  geom_line(aes(y = median_actual),
            color = "blue", size = 1.2) +
  geom_abline(slope = 1, intercept = 0,
              color = "black", linetype = "dashed") +
  theme_bw() +
  labs(
    x = "Predicted games played",
    y = "Observed games played",
    title = "Games Played Model Calibration Plot with Boostrap Uncertainty Bands"
  )

## Calibration plots

gp_calib_cv <-
  ridge_pois_cv_preds |>
  mutate(bin_pred_gp = cut(
    pred_gp,
    breaks = quantile(pred_gp,
                      probs = seq(0, 1, by = 0.1),
                      na.rm = TRUE),
    include.lowest = TRUE
  )) |>
  group_by(bin_pred_gp) |>
  summarize(
    mean_pred_gp = mean(pred_gp),
    mean_actual_gp = mean(actual_gp),
    n_players = n(),
    .groups = "drop"
  )

gp_calib_cv <- gp_calib_cv |>
  mutate(
    se = sqrt(mean_actual_gp / n_players),
    lower = pmax(mean_actual_gp - 2 * se, 0),
    upper = mean_actual_gp + 2 * se
  )

ggplot(gp_calib_cv,
       aes(x = mean_pred_gp, y = mean_actual_gp)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.02) +
  geom_abline(slope = 1, intercept = 0,
              color = "black", linetype = "dashed") +
  theme_bw() +
  labs(
    x = "Mean predicted GP per bin",
    y = "Mean observed GP per bin",
    title = "Games Played Calibration Plot"
  ) +
  theme(
    legend.position = "bottom",
    strip.background = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

# age

ridge_pois_cv_preds <- ridge_pois_cv_preds |>
  mutate(
    age_group = cut(
      age,
      breaks = c(-Inf, 22, 25, Inf),
      labels = c("<23", "23–25", "26+")
    )
  )

gp_calib_age <-
  ridge_pois_cv_preds |>
  mutate(
    bin_pred_gp = cut(
      pred_gp,
      breaks = quantile(pred_gp,
                        probs = seq(0, 1, by = 0.1),
                        na.rm = TRUE),
      include.lowest = TRUE
    )
  ) |>
  group_by(age_group, bin_pred_gp) |>
  summarize(
    mean_pred_gp   = mean(pred_gp),
    mean_actual_gp = mean(actual_gp),
    n_players      = n(),
    .groups = "drop"
  ) |>
  mutate(
    se    = sqrt(mean_actual_gp / n_players),
    lower = pmax(mean_actual_gp - 2 * se, 0),
    upper = mean_actual_gp + 2 * se
  )

ggplot(gp_calib_age,
       aes(x = mean_pred_gp, y = mean_actual_gp)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.02) +
  geom_abline(slope = 1, intercept = 0,
              color = "black", linetype = "dashed") +
  facet_wrap(~ age_group) +
  theme_bw() +
  labs(
    x = "Mean predicted GP per bin",
    y = "Mean observed GP per bin",
    title = "Games Played Calibration Plot by Age Group"
  ) +
  theme(
    legend.position = "bottom",
    strip.background = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

# position

gp_calib_pos <-
  ridge_pois_cv_preds |>
  mutate(
    bin_pred_gp = cut(
      pred_gp,
      breaks = quantile(pred_gp,
                        probs = seq(0, 1, by = 0.1),
                        na.rm = TRUE),
      include.lowest = TRUE
    )
  ) |>
  group_by(position_group, bin_pred_gp) |>
  summarize(
    mean_pred_gp   = mean(pred_gp),
    mean_actual_gp = mean(actual_gp),
    n_players      = n(),
    .groups = "drop"
  ) |>
  mutate(
    se    = sqrt(mean_actual_gp / n_players),
    lower = pmax(mean_actual_gp - 2 * se, 0),
    upper = mean_actual_gp + 2 * se
  )

ggplot(gp_calib_pos,
       aes(x = mean_pred_gp, y = mean_actual_gp)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.02) +
  geom_abline(slope = 1, intercept = 0,
              color = "black", linetype = "dashed") +
  facet_wrap(~ position_group) +
  theme_bw() +
  labs(
    x = "Mean predicted GP per bin",
    y = "Mean observed GP per bin",
    title = "Games Played Calibration Plot by Position Group"
  ) +
  theme(
    legend.position = "bottom",
    strip.background = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

## xG

gp_calib_by_xg <-
  ridge_pois_cv_preds |>
  mutate(
    xG_group = cut(
      xG_coef,
      breaks = quantile(xG_coef,
                        probs = c(0, 1/3, 2/3, 1),
                        na.rm = TRUE),
      include.lowest = TRUE,
      labels = c("Low xG", "Mid xG", "High xG")
    ),
    bin_pred_gp = cut(
      pred_gp,
      breaks = quantile(pred_gp,
                        probs = seq(0, 1, by = 0.1),
                        na.rm = TRUE),
      include.lowest = TRUE
    )
  ) |>
  group_by(xG_group, bin_pred_gp) |>
  summarize(
    n_players      = n(),
    mean_pred_gp   = mean(pred_gp),
    mean_actual_gp = mean(actual_gp),
    .groups = "drop"
  ) |>
  mutate(
    se    = sqrt(mean_actual_gp / n_players),
    lower = pmax(mean_actual_gp - 2 * se, 0),
    upper = mean_actual_gp + 2 * se
  )

ggplot(gp_calib_by_xg,
       aes(x = mean_pred_gp, y = mean_actual_gp)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.02) +
  geom_abline(slope = 1, intercept = 0,
              color = "black", linetype = "dashed") +
  facet_wrap(~ xG_group) +
  theme_bw() +
  labs(
    x = "Mean predicted GP per bin",
    y = "Mean observed GP per bin",
    title = "Games Played Calibration Plot by xG Bucket"
  ) +
  theme(
    legend.position = "bottom",
    strip.background = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

