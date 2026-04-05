## Load xT data
library(here)
library(dplyr)
library(tidyr)
library(mgcv)

xt_event_data <- readRDS(here('data', 'datasets', 'xt_sc_data.rds'))
events <- here('data', 'datasets', 'events.parquet') |>
  arrow::read_parquet()

games <- here('data', 'datasets', 'games.parquet') |>
  arrow::read_parquet()

players <- here('data', 'datasets', 'players.parquet') |>
  arrow::read_parquet()

stints <- here('data', 'datasets', 'stints.parquet') |>
  arrow::read_parquet()

tracking <- here('data', 'datasets', 'tracking.parquet') |>
  arrow::read_parquet()

