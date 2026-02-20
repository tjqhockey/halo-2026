library(here)
library(tidyverse)
library(ggpubr)
library(sportyR)
library(grid)

## Not needed as I'm writing this and will take some time to run
#source(here('docs', 'xt_basic.R'))


## Updated to sum xT_sc
top_10 <- xT_basic_data |>
  # filter(str_detect(player_name, 'Griffith')) |> View()
  group_by(player_id, player_name, team) |>
  summarize(total_xT = sum(change_xT_sc, na.rm = T),
            num_relevant_events = n()) |> 
  ungroup() |>
  arrange(desc(total_xT)) |>
  # filter(str_detect(player_name, 'Griffith'))
  mutate(rank = 1:n()) |>
  slice_head(n = 10)

bot_10 <- xT_basic_data |>
  # filter(str_detect(player_name, 'Griffith')) |> View()
  group_by(player_id, player_name, team) |>
  summarize(total_xT = sum(change_xT_sc, na.rm = T),
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
  rename(`Rank` = rank,
         `"Player Name"` = player_name,
         `xTA[sc]` = total_xT,
         `"# Relevant Events"` = num_relevant_events,
         Team = team) |>
  ggtexttable(rows = NULL,
              theme = ttheme(
                "light",
                colnames.style = colnames_style(col = "white",
                                                fontface = "bold",
                                                cex = 1.2,
                                                fill = "steelblue",
                                                parse = TRUE),
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
  summarize(total_xT = sum(change_xT_sc, na.rm = T),
            num_relevant_events = n()) |> 
  ungroup() |>
  arrange(desc(total_xT)) |>
  # filter(str_detect(player_name, 'Griffith'))
  mutate(rank = 1:n()) |>
  slice_head(n = 5)

top_5_with_types <- xT_basic_data |>
  filter(player_id %in% top_5$player_id) |>
  group_by(player_id,player_name,team,event_type) |>
  summarize(total_xT = sum(change_xT_sc, na.rm = T),
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
  mutate(event_type = recode(event_type,
                             carry = "Carry",
                             controlledbreakout = "Controlled Breakout",
                             dumpin = "Dump-In",
                             dumpout = "Dump-Out",
                             failedpasslocation = "Failed Pass",
                             lpr = "Loose Puck Recovery",
                             pass = "Pass",
                             puckprotection = "Puck Protection",
                             reception = "Reception",
                             shot = "Shot"
  )) |>
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
       y = expression("Total " ~ xTA[sc]),
       fill = "Event Type") +
  theme_bw()


## Making ice rink xT added at a frame plot
frame_1_plot <- tracking |>
  filter(game_id == "df609ab7-6328-37cf-a71a-fb6672cedaf4",
         sl_event_id == "548") |>
  left_join(events)
x_target <- 75.24410
y_target <- 5.275387

geom_hockey(league = "AHL",
            display_range = "offense") +
  geom_segment(
    data = frame_1_plot |> filter(player_name == "Bourque, Mavrik"),
    aes(x = x_adj, y = y_adj-1,
        xend = x_target, yend = y_target - 3),
    color = "black",
    linewidth = 1.5,
    arrow = arrow(length = unit(0.2, "inches"), type = "closed")
  )+
  geom_point(
    data = frame_1_plot,
    aes(x = tracking_x, y = tracking_y, color = team_name),
    size = 5,
    alpha = 0.8
  ) +
  scale_color_manual(values = c("Stars" = "darkgreen", 
                                "Wolves" = "darkred")) +
  geom_point(data = frame_1_plot |>
               filter(player_name == "Bourque, Mavrik"),
             aes(x = x_adj,y = y_adj),
             size = 3,
             color = "black") +
  geom_text(
    data = tibble(),
    aes(x = 71, y = 30),
    label = '"+" * .096 ~ xTA[sc]',
    parse = TRUE,
    color = "black",
    size = 4,
    hjust = 0,
    vjust = 1
  ) +
  labs(color = "Team Name")




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
  filter(game_id ==
           "df609ab7-6328-37cf-a71a-fb6672cedaf4",
         sl_event_id == 548)

test3 <- xT_basic_data |>
  #filter(player_name == "Bourque, Mavrik")
  filter(possession_id == 
  "df609ab7-6328-37cf-a71a-fb6672cedaf4_6_27")
         
## Just fooling around
test4 <- xT_basic_data |>
  # filter(str_detect(player_name, 'Griffith')) |> View()
  group_by(player_id, player_name, team) |>
  summarize(total_xT = mean(change_xT_sc, na.rm = T),
            num_relevant_events = n()) |> 
  ungroup() |>
  arrange(desc(total_xT)) |>
  # filter(str_detect(player_name, 'Griffith'))
  filter(num_relevant_events > 500) |>
  mutate(rank = 1:n()) |>
  slice_head(n = 25)
test4 |>
  separate(player_name, into = c("Last", "First"), sep = ",\\s*") |>
  unite("player_name", First, Last, sep = " ") |>
  select(rank,player_name,team,num_relevant_events,total_xT) |>
  mutate(total_xT = round(total_xT,4)) |>
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







