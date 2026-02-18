# PURPOSE: Create penalties dataframe

library(here)
library(tidyverse)

events <- here('data', 'datasets', 'events.parquet') |>
  arrow::read_parquet()

# intermediary df
penalties <- events |>
  filter(event_type == 'penalty', detail %in% c('start', 'end')) |> 
  group_split(game_id) |>
  purrr::map(\(df){
    
    df <- df |>
      arrange(sl_event_id)
    
    start_acc <- c()
    start_period_acc <- c()
    start_period <- c()
    end_period <- c()
    start <- c()
    end <- c()
    
    for (i in 1:nrow(df)) {
      if (df[i, 'detail'] == 'start') {
        # queue in start_acc
        start_acc <- append(start_acc, df[[i, 'period_time']])
        start_period_acc <- append(start_period_acc, df[[i, 'period']])
        next
      } else if (df[i, 'detail'] == 'end') {
        start <- append(start, start_acc[1])
        start_acc <- start_acc[-1]
        start_period <- append(start_period, start_period_acc[1])
        start_period_acc <- start_period_acc[-1]
        end_period <- append(end_period, df[[i, 'period']])
        end <- append(end, df[[i, 'period_time']])
      }
    }
    
    out <- tibble(game_id = df$game_id[1],
           start_period = start_period |> as.vector(),
           end_period = end_period,
           start_time = start,
           end_time = end) |>
      mutate(penalty_id = 1:n())
    
    return(out)
  }) |>
  bind_rows()

crossover_penalties <- penalties |>
  filter(start_period != end_period)

starts <- crossover_penalties |>
  mutate(end_period = start_period,
         end_time = 1200)

ends <- crossover_penalties |>
  mutate(start_period = end_period,
         start_time = 0)

clean_penalties <- starts |>
  bind_rows(ends) |>
  bind_rows(penalties |> filter(start_period == end_period)) |>
  rename(period = start_period) |>
  select(-end_period)

saveRDS(clean_penalties, here('data', 'datasets', 'penalties.rds'))
