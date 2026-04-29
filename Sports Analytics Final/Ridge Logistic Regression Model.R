## Ridge Logistic Regression Model

library(glmnet)
library(purrr)
library(tidyverse)
library(ggplot2)

# Setting up data for modeling

promo_dat <- players |>
  select(made_nhl,
         xG_coef,
         xG_pp_coef,
         xG_pk_coef,
         age,
         position_group) |>
  filter(!is.na(made_nhl),
         !is.na(xG_coef),
         !is.na(xG_pp_coef),
         !is.na(xG_pk_coef),
         !is.na(age),
         !is.na(position_group))

# Model matrix
X_promo <- model.matrix(
  made_nhl ~ xG_coef*age +
    xG_pp_coef*age +
    xG_pk_coef*age +
    position_group,
  data = promo_dat
)[, -1]

y_promo <- promo_dat$made_nhl

# Cross validation

set.seed(1979)
n_promo <- nrow(promo_dat)
promo_dat$fold <- sample(rep(1:5, length.out = n_promo))

ridge_logit_cv_preds <-
  map_dfr(1:5, function(test_fold) {
    
    test_idx  <- which(promo_dat$fold == test_fold)
    train_idx <- which(promo_dat$fold != test_fold)
    
    X_train <- X_promo[train_idx, ]
    y_train <- y_promo[train_idx]
    X_test  <- X_promo[test_idx, ]
    
    # Ridge logistic with internal lambda CV
    ridge_fit <- cv.glmnet(
      x = X_train,
      y = y_train,
      family = "binomial",
      alpha = 0
    )
    
    test_pred_probs <- as.numeric(
      predict(ridge_fit,
              newx = X_test,
              s = "lambda.min",
              type = "response")
    )
    
    tibble(
      pred_prob       = test_pred_probs,
      actual          = y_promo[test_idx],
      fold            = test_fold,
      xG_coef         = promo_dat$xG_coef[test_idx],
      xG_pp_coef      = promo_dat$xG_pp_coef[test_idx],
      xG_pk_coef      = promo_dat$xG_pk_coef[test_idx],
      age             = promo_dat$age[test_idx],
      position_group  = promo_dat$position_group[test_idx]
    )
  })

## Calibration Plots

ridge_calibration_data <-
  ridge_logit_cv_preds |>
  mutate(bin_pred_prob = round(pred_prob / 0.05) * 0.05) |>
  group_by(bin_pred_prob) |>
  summarize(
    n_players      = n(),
    bin_actual_prob = mean(actual),
    bin_se          = sqrt((bin_actual_prob * (1 - bin_actual_prob)) / n_players),
    .groups = "drop"
  ) |>
  mutate(
    bin_upper = pmin(bin_actual_prob + 2 * bin_se, 1),
    bin_lower = pmax(bin_actual_prob - 2 * bin_se, 0)
  )

ggplot(ridge_calibration_data,
       aes(x = bin_pred_prob, y = bin_actual_prob)) +
  geom_point() +
  geom_errorbar(aes(ymin = bin_lower, ymax = bin_upper)) +
  geom_abline(slope = 1, intercept = 0,
              color = "black", linetype = "dashed") +
  coord_equal() +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    x = "Estimated promotion probability",
    y = "Observed promotion frequency",
    title = "NHL Promotion Model Calibration Plot"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

## By position

ridge_calib_by_pos <-
  ridge_logit_cv_preds |>
  mutate(bin_pred_prob = round(pred_prob / 0.05) * 0.05) |>
  group_by(position_group, bin_pred_prob) |>
  summarize(
    n_players       = n(),
    bin_actual_prob = mean(actual),
    bin_se          = sqrt((bin_actual_prob * (1 - bin_actual_prob)) / n_players),
    .groups = "drop"
  ) |>
  mutate(
    bin_upper = pmin(bin_actual_prob + 2 * bin_se, 1),
    bin_lower = pmax(bin_actual_prob - 2 * bin_se, 0)
  )

ggplot(ridge_calib_by_pos,
       aes(x = bin_pred_prob, y = bin_actual_prob)) +
  geom_point() +
  geom_errorbar(aes(ymin = bin_lower, ymax = bin_upper)) +
  geom_abline(slope = 1, intercept = 0,
              color = "black", linetype = "dashed") +
  facet_wrap(~ position_group) +
  coord_equal() +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    x = "Estimated promotion probability",
    y = "Observed promotion frequency",
    title = "NHL Promotion Calibration Plot by Position Group"
  ) +
  theme_bw() +
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        plot.title = element_text(hjust = 0.5))

## By xG

ridge_calib_by_xg <-
  ridge_logit_cv_preds |>
  mutate(
    xG_group = cut(
      xG_coef,
      breaks = quantile(xG_coef,
                        probs = c(0, 1/3, 2/3, 1),
                        na.rm = TRUE),
      include.lowest = TRUE,
      labels = c("Lower Third xG", "Middle Third xG", "Upper Third xG")
    ),
    bin_pred_prob = round(pred_prob / 0.05) * 0.05
  ) |>
  group_by(xG_group, bin_pred_prob) |>
  summarize(
    n_players       = n(),
    bin_actual_prob = mean(actual),
    bin_se          = sqrt((bin_actual_prob * (1 - bin_actual_prob)) / n_players),
    .groups = "drop"
  ) |>
  mutate(
    bin_upper = pmin(bin_actual_prob + 2 * bin_se, 1),
    bin_lower = pmax(bin_actual_prob - 2 * bin_se, 0)
  )

ggplot(ridge_calib_by_xg,
       aes(x = bin_pred_prob, y = bin_actual_prob)) +
  geom_point() +
  geom_errorbar(aes(ymin = bin_lower, ymax = bin_upper)) +
  geom_abline(slope = 1, intercept = 0,
              color = "black", linetype = "dashed") +
  facet_wrap(~ xG_group) +
  coord_equal() +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    x = "Estimated promotion probability",
    y = "Observed promotion frequency",
    title = "NHL Promotion Calibration Plot by xG Coefficient Bucket"
  ) +
  theme_bw() +
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        plot.title = element_text(hjust = 0.5))

## By Age

ridge_calib_by_age <-
  ridge_logit_cv_preds |>
  mutate(
    age_group = cut(
      age,
      breaks = c(-Inf, 22, 25, Inf),
      labels = c("<23", "23–25", "26+")
    ),
    bin_pred_prob = round(pred_prob / 0.05) * 0.05
  ) |>
  group_by(age_group, bin_pred_prob) |>
  summarize(
    n_players       = n(),
    bin_actual_prob = mean(actual),
    bin_se          = sqrt((bin_actual_prob * (1 - bin_actual_prob)) / n_players),
    .groups = "drop"
  ) |>
  mutate(
    bin_upper = pmin(bin_actual_prob + 2 * bin_se, 1),
    bin_lower = pmax(bin_actual_prob - 2 * bin_se, 0)
  )

ggplot(ridge_calib_by_age,
       aes(x = bin_pred_prob, y = bin_actual_prob)) +
  geom_point() +
  geom_errorbar(aes(ymin = bin_lower, ymax = bin_upper)) +
  geom_abline(slope = 1, intercept = 0,
              color = "black", linetype = "dashed") +
  facet_wrap(~ age_group) +
  coord_equal() +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    x = "Estimated promotion probability",
    y = "Observed promotion frequency",
    title = "NHL Promotion Calibration Plot by Age Group"
  ) +
  theme_bw() +
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        plot.title = element_text(hjust = 0.5))

## Boostrap

bootstrap_once <- function(data, X, y) {
  
  # Resample player rows
  idx <- sample(seq_len(nrow(data)), replace = TRUE)
  X_boot <- X[idx, ]
  y_boot <- y[idx]
  
  # Fit ridge logistic with internal CV
  fit_boot <- cv.glmnet(
    x = X_boot,
    y = y_boot,
    family = "binomial",
    alpha = 0
  )
  
  # Predict on original data
  pred_prob <- as.numeric(
    predict(fit_boot, newx = X, s = "lambda.min", type = "response")
  )
  
  # Build calibration bins
  tibble(
    pred_prob = pred_prob,
    actual = y,
    bin_pred_prob = round(pred_prob / 0.05) * 0.05
  ) |>
    group_by(bin_pred_prob) |>
    summarize(
      n_players = n(),
      bin_actual_prob = mean(actual),
      .groups = "drop"
    )
}

set.seed(2026)
B <- 200

boot_list <- map(1:B, ~ bootstrap_once(promo_dat, X_full, y_full))

boot_df <- bind_rows(
  lapply(seq_along(boot_list), function(b) {
    boot_list[[b]] |> mutate(boot = b)
  })
)

boot_bands <- boot_df |>
  group_by(bin_pred_prob) |>
  summarize(
    median_prob = median(bin_actual_prob),
    lower_95 = quantile(bin_actual_prob, 0.025),
    upper_95 = quantile(bin_actual_prob, 0.975),
    .groups = "drop"
  )

# Bootstrap Calibration Plot

ggplot(boot_bands, aes(x = bin_pred_prob)) +
  geom_ribbon(aes(ymin = lower_95, ymax = upper_95),
              fill = "skyblue", alpha = 0.35) +
  geom_line(aes(y = median_prob), color = "blue", size = 1.2) +
  geom_abline(slope = 1, intercept = 0,
              color = "black", linetype = "dashed") +
  coord_equal() +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    x = "Estimated promotion probability",
    y = "Observed promotion frequency",
    title = "NHL Promotion Model Calibration Plot with Bootstrap Uncertainty Bands"
  ) +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))
