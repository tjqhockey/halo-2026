library(here)
library(tidyverse)
library(ggpubr)
library(sportyR)

## Not needed as I'm writing this and will take some time to run
#source(here('docs', 'xt_basic.R'))


## Right from xT_basic_data.R
top_10 <- xT_basic_data |>
  # filter(str_detect(player_name, 'Griffith')) |> View()
  group_by(player_id, player_name, team) |>
  summarize(total_xT = sum(change_xT, na.rm = T),
            num_relevant_events = n()) |> 
  ungroup() |>
  arrange(desc(total_xT)) |>
  # filter(str_detect(player_name, 'Griffith'))
  mutate(rank = 1:n()) |>
  slice_head(n = 10)

bot_10 <- xT_basic_data |>
  # filter(str_detect(player_name, 'Griffith')) |> View()
  group_by(player_id, player_name, team) |>
  summarize(total_xT = sum(change_xT, na.rm = T),
            num_relevant_events = n()) |> 
  ungroup() |>
  arrange(desc(total_xT)) |>
  # filter(str_detect(player_name, 'Griffith'))
  mutate(rank = 1:n()) |>
  slice_tail(n = 10)


## Top 10 Table
top_10 |>
  separate(player_name, into = c("Last", "First"), sep = ",\\s*") |>
  unite("player_name", First, Last, sep = " ") |>
  select(rank,player_name,team,num_relevant_events,total_xT) |>
  mutate(total_xT = round(total_xT,2)) |>
  rename(Rank = rank,
         "Player Name" = player_name,
         "Total xTA" = total_xT,
         "# Relevant Events" = num_relevant_events,
         Team = team) |>
  ggtexttable(rows = NULL,
              theme = ttheme(
                "light",
                colnames.style = colnames_style(col = "white",
                                                fontface = "bold",
                                                cex = 1.2,
                                                fill = "steelblue"),
                tbody.style = tbody_style(fill = rep(c("lightgray", "lightblue"),
                                                     length.out = nrow(top_10)),
                                          cex = 1.2)
                )
  )

## Bottom 10 table
bot_10 |>
  separate(player_name, into = c("Last", "First"), sep = ",\\s*") |>
  unite("player_name", First, Last, sep = " ") |>
  select(rank,player_name,team,num_relevant_events,total_xT) |>
  mutate(total_xT = round(total_xT,2)) |>
  rename(Rank = rank,
         "Player Name" = player_name,
         "Total xTA" = total_xT,
         "# Relevant Events" = num_relevant_events,
         Team = team) |>
  ggtexttable(rows = NULL,
              theme = ttheme(
                "light",
                colnames.style = colnames_style(col = "white",
                                                fontface = "bold",
                                                cex = 1.2,
                                                fill = "firebrick"),
                tbody.style = tbody_style(fill = rep(c("lightgray", "salmon"),
                                                     length.out = nrow(top_10)),
                                          cex = 1.2)
              )
  )


## xT Ice rink
plot_rink_grid <- xT_basic_data |>
  group_by(x_box_id,y_box_id) |>
  summarize(avg_xT = mean(xT)) |>
  left_join(rink_grid)
geom_hockey(league = "AHL",
            display_range = "offense") +
  geom_rect(
    data = plot_rink_grid,
    aes(xmin = x_min, xmax = x_max,
        ymin = y_min, ymax = y_max,
        fill = avg_xT),
    alpha = 0.5,
    color = "black",
    linewidth = 0.3
  ) +
  scale_fill_gradient(low = "blue",
                      high = "red",
                      name = "Avg xT")

## Bar graph for top 5 by event type
top_5 <- xT_basic_data |>
  # filter(str_detect(player_name, 'Griffith')) |> View()
  group_by(player_id, player_name, team) |>
  summarize(total_xT = sum(change_xT, na.rm = T),
            num_relevant_events = n()) |> 
  ungroup() |>
  arrange(desc(total_xT)) |>
  # filter(str_detect(player_name, 'Griffith'))
  mutate(rank = 1:n()) |>
  slice_head(n = 5)

top_5_with_types <- xT_basic_data |>
  filter(player_id %in% top_5$player_id) |>
  group_by(player_id,player_name,team,event_type) |>
  summarize(total_xT = sum(change_xT, na.rm = T),
            num_relevant_events = n()) |>
  ungroup() |>
  arrange(desc(total_xT)) |>
  group_by(player_id) |>
  mutate(full_sum_xT = sum(total_xT)) |>
  ungroup() |>
  arrange(desc(full_sum_xT))

top_5_with_types |>
  separate(player_name, into = c("Last", "First"), sep = ",\\s*") |>
  unite("player_name", First, Last, sep = " ") |>
  ggplot(aes(x = reorder(player_name, full_sum_xT),
             y = total_xT,
             fill = event_type)) +
  geom_bar(stat = "identity") +
  geom_text(
    data = top_5_with_types |>
      separate(player_name, into = c("Last", "First"), sep = ",\\s*") |>
      unite("player_name", First, Last, sep = " ") |>
      distinct(player_name, full_sum_xT),
    aes(x = reorder(player_name, full_sum_xT),
        y = full_sum_xT,
        label = round(full_sum_xT, 2)),
    inherit.aes = FALSE,
    vjust = -0.4,
    fontface = "bold",
    size = 4
  ) +
  labs(x = "Player Name",
       y = "Total xTA",
       fill = "Event Type") +
  theme_bw()


## Making ice rink xT added at a frame plot
geom_hockey(league = "AHL",
            display_range = "offense") +
  geom_rect(
    data = plot_rink_grid,
    aes(xmin = x_min, xmax = x_max,
        ymin = y_min, ymax = y_max),
    alpha = 0,
    color = "black",
    linewidth = 0.3
  )












## Just for testing/bugfixing
test <- xT_basic_data |>
  group_by(game_id,possession_id) |>
  arrange(sl_event_id.by_group = TRUE) |>
  select(-c(player_id,period,
            has_tracking_data,team_id,
            opp_team,opp_team_id,
            possession_team_id,
            flags)) |>
  select(event_type,description,player_name,
         team,sequence_id,outcome,xT,xT_next,change_xT)

test2 <- xT_basic_data |>
  filter(event_type == "pass") |>
  select(event_type,description,player_name,
         team,sequence_id,outcome,xT,xT_next,change_xT)

test3 <- events |>
  event_type == "shot"
         










