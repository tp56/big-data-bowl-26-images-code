library(dplyr)
library(ggplot2)
library(gganimate)
library(nflplotR)

commitment_data_w_context <- all_data |>
  left_join(supplementary_data |>
              select(game_id, play_id, team_coverage_man_zone, team_coverage_type, pass_result),
            by = c("game_id", "play_id")
  ) |> left_join(final_with_names |>
                   select(nfl_id,player_name, player_position), 
                 by = ("nfl_id"))


# coverage summary --------------------------------------------------------

summary_coverage <- commitment_data_w_context %>%
  group_by(team_coverage_type) %>%
  summarise(
    mean_commitment = mean(commitment, na.rm = TRUE),
    median_commitment = median(commitment, na.rm = TRUE),
    min_commitment = min(commitment, na.rm = TRUE),
    max_commitment = max(commitment, na.rm = TRUE),
    sd_commitment = sd(commitment, na.rm = TRUE)
  ) 

ggplot(commitment_data_w_context, aes(x = team_coverage_type, y = commitment)) +
  geom_boxplot() +
  labs(title = "Commitment Across Coverage Types",
       x = "Coverage Type",
       y = "Commitment") +
  theme_minimal()

commitment_by_coverage <- commitment_data_w_context |>
  group_by(team_coverage_type) |>
  summarise(mean_commitment = mean(commitment, na.rm = TRUE))

commitment_by_result <- commitment_data_w_context %>%
  group_by(team_coverage_man_zone, pass_result) %>%
  summarise(mean_commitment = mean(commitment, na.rm = TRUE))

anova_result <- aov(commitment ~ team_coverage_man_zone, data = commitment_data_w_context)

library(dplyr)
library(gt)
library(scales)

commitment_by_coverage <- commitment_by_coverage |>
  filter(team_coverage_type != "") 
  
max_val <- max(commitment_by_coverage$mean_commitment, na.rm = TRUE)
min_val <- min(commitment_by_coverage$mean_commitment, na.rm = TRUE)

gt_tbl <- commitment_by_coverage |>
  mutate(team_coverage_type = gsub("_", " ", team_coverage_type)) %>%
  arrange(desc(mean_commitment)) %>%
  gt() %>%
  tab_header(
    title = "Average Team Commitment by Coverage Type"
  ) %>%
  cols_label(
    team_coverage_type = "Coverage Type",
    mean_commitment = "Mean Commitment"
  ) %>%
  fmt_number(mean_commitment, decimals = 3) %>%
  cols_align(align = "center", everything()) %>%
  
  # 🔴 Lowest value in red
  tab_style(
    style = cell_fill(color = "#FDE0DD"),
    locations = cells_body(
      columns = mean_commitment,
      rows = mean_commitment == min_val
    )
  ) %>%
  
  # 🟢 Highest value in green
  tab_style(
    style = cell_fill(color = "#E5F5E0"),
    locations = cells_body(
      columns = mean_commitment,
      rows = mean_commitment == max_val
    )
  ) %>%
  
  opt_table_outline()


gtsave(gt_tbl, "commitment_by_coverage.html")
print(gt_tbl)
# timing ------------------------------------------------------------------

max_commit_rows_play <- all_frame_data |>
  mutate(commit = as.numeric(commit)) |>
  group_by(play_id) |>
  slice_max(order_by = commit, with_ties = FALSE) |>
  ungroup()

print(mean(max_commit_rows_play$time_before_land))
print(mean(max_commit_rows_play$commit))

play_with_max_of_max <- max_commit_rows_play |>
  slice_max(order_by = commit, n = 1, with_ties = FALSE) 

play_with_min_of_max <- max_commit_rows_play |>
  slice_min(order_by = commit, n = 1, with_ties = FALSE) 

play_with_max_of_max
play_with_min_of_max

# teams -------------------------------------------------------------------

library(nflplotR)
team_commitment_summary <- max_commit_rows_play |>
  group_by(defensive_team, team_coverage_man_zone) |>
  summarise(
    avg_max_commit = mean(commit, na.rm = TRUE),
    avg_time_of_max_commit = mean(time_before_land, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

man_commitment <- team_commitment_summary %>%
  filter(team_coverage_man_zone == "MAN_COVERAGE")

zone_commitment <- team_commitment_summary %>%
  filter(team_coverage_man_zone == "ZONE_COVERAGE")

ggplot(
  man_commitment,
  aes(
    x = avg_time_of_max_commit,
    y = avg_max_commit,
    team_abbr = defensive_team
  )
) +
  geom_nfl_logos(width = 0.045, alpha = 0.9) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.3) +
  coord_cartesian(xlim = c(-0.8, -0.2)) +
  labs(
    title = "Max Commitment vs Time by Team in Man Coverage",
    x = "Average Time of Max Commitment (s)",
    y = "Average Max Commitment"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title.position = "plot", 
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.margin = margin(t = 80, r = 10, b = 10, l = 10),  # big top margin
    panel.grid.minor = element_blank()
  )

ggplot(
  zone_commitment,
  aes(
    x = avg_time_of_max_commit,
    y = avg_max_commit,
    team_abbr = defensive_team
  )
) +
  geom_nfl_logos(width = 0.045, alpha = 0.9) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.3) +
  coord_cartesian(xlim = c(-0.8, -0.2)) +
  labs(
    title = "Max Commitment vs Time by Team in Zone Coverage",
    x = "Average Time of Max Commitment (s)",
    y = "Average Max Commitment"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title.position = "plot", 
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.margin = margin(t = 80, r = 10, b = 10, l = 10),  # big top margin
    panel.grid.minor = element_blank()
  )


# players -----------------------------------------------------------------
library(dplyr)
library(gt)

top10_players <- final_with_names %>%
  select(player_name, mean_commitment, player_position) %>%
  arrange(desc(mean_commitment)) %>%
  slice_head(n = 10)

# Create a polished table
top10_players %>%
  gt() %>%
  tab_header(
    title = "Top 10 Players by Mean Commitment"
  ) %>%
  cols_label(
    player_name = "Player Name",
    mean_commitment = "Mean Commitment",
    player_position = "Position"
  ) %>%
  fmt_number(
    columns = vars(mean_commitment),
    decimals = 2
  ) %>%
  tab_options(
    table.width = pct(70),
    heading.align = "center"
  )
