# Step 1: Compute per-player sums
player_doe <- df[, .(
  plays = .N,
  doe = sum(disruption.x - expected_disruption)
), by = nfl_id]

# Step 4: Add player names
player_lookup <- unique(df[, .(nfl_id, player_name)])
player_doe <- merge(player_doe, player_lookup, by = "nfl_id", all.x = TRUE)
# Make sure player_doe is a data.table
setDT(player_doe)

# Step 4: Add player names
player_lookup <- unique(df[, .(nfl_id, player_name)])
player_doe <- merge(player_doe, player_lookup, by = "nfl_id", all.x = TRUE)

# Step 5: Order by DOE
setorder(player_doe, -doe)

# Step 6: Get top 10 players by DOE
top10 <- player_doe[1:10]
setDT(top10)
# Turn nfl_id into character
top10[, nfl_id := as.character(nfl_id)]

# Step 7: Add GSIS ID
players <- load_players()          # loads full NFL player table
setDT(players)
gsis_data <- players[, .(nfl_id, gsis_id)]
top10 <- merge(top10, gsis_data, by = "nfl_id", all.x = TRUE)

# Step 8: View final table
top10[, .(player_name.x, nfl_id, gsis_id, doe)]

# Plot
top10_plot <- top10[, .(player_name = player_name.x, nfl_id, gsis_id, doe)]
ggplot(top10_plot, aes(x = reorder(player_name, doe), y = doe)) +
  geom_col(fill = "steelblue") +
  geom_nfl_headshots(aes(player_gsis = gsis_id), width = 0.1, height = 0.1) +
  coord_flip() +  # horizontal bars
  labs(x = "", y = "DOE", title = "Top 10 Players by DOE") +
  theme_minimal()