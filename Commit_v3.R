library(data.table)

# ---------------------------------------------------------
# FRAME-BY-FRAME COMMITMENT FUNCTION (output-only version)
# ---------------------------------------------------------
calc_commitment <- function(dt, blx, bly) {
  if (nrow(dt) < 2) return(NA_real_)
  
  # Sort by time
  setorder(dt, frame_id)
  
  # Defender velocity
  dt[, dx := shift(x, type="lead") - x]
  dt[, dy := shift(y, type="lead") - y]
  
  # Speed
  dt[, speed := sqrt(dx^2 + dy^2)]
  
  # Direction to ball landing
  dt[, bx := blx - x]
  dt[, by := bly - y]
  dt[, bmag := sqrt(bx^2 + by^2)]
  dt[, ux := ifelse(bmag > 0, bx/bmag, 0)]
  dt[, uy := ifelse(bmag > 0, by/bmag, 0)]
  
  # Velocity unit vector
  dt[, pmag := sqrt(dx^2 + dy^2)]
  dt[, vx := ifelse(pmag > 0, dx/pmag, 0)]
  dt[, vy := ifelse(pmag > 0, dy/pmag, 0)]
  
  # Cosine similarity
  dt[, cosang := vx*ux + vy*uy]
  
  # Commitment = speed towards ball * cos(angle)
  dt[, commit := speed * cosang]
  
  return(mean(dt$commit, na.rm = TRUE))
}



# ============================================================
#                PROCESS ALL 18 WEEKS
# ============================================================
all_results <- list()

for (week in 1:18) {
  
  input_path  <- sprintf("data/114239_nfl_competition_files_published_analytics_final/train/input_2023_w%02d.csv", week)
  output_path <- sprintf("data/114239_nfl_competition_files_published_analytics_final/train/output_2023_w%02d.csv", week)
  
  input  <- fread(input_path)
  output <- fread(output_path)
  
  # Filter target defenders (player_to_predict only exists in input)
  defenders <- input[player_side=="Defense" & player_to_predict==1,
                     .(game_id, play_id, nfl_id, player_position,
                       ball_land_x, ball_land_y)]
  
  # Keep unique defender-play rows
  defenders <- unique(defenders)
  
  # Prepare storage
  week_out <- vector("list", nrow(defenders))
  
  # Loop through defenders
  for (i in seq_len(nrow(defenders))) {
    
    g  <- defenders$game_id[i]
    p  <- defenders$play_id[i]
    id <- defenders$nfl_id[i]
    blx <- defenders$ball_land_x[i]
    bly <- defenders$ball_land_y[i]
    
    post <- output[game_id==g & play_id==p & nfl_id==id]
    
    if (nrow(post) < 2) next
    
    commit_val <- calc_commitment(post, blx, bly)
    
    week_out[[i]] <- data.table(
      game_id = g,
      play_id = p,
      nfl_id  = id,
      position = defenders$position[i],
      commitment = commit_val
    )
  }
  
  all_results[[week]] <- rbindlist(week_out, fill = TRUE)
}

# Combine all weeks
all_data <- rbindlist(all_results, fill = TRUE)

# ============================================================
#             GROUP BY PLAYER + POSITION & RANK
# ============================================================

# Summarize by player
final_player_scores <- all_data[, .(
  plays = .N,
  mean_commitment = mean(commitment, na.rm=TRUE),
  sd_commitment = sd(commitment, na.rm=TRUE)
), by=.(nfl_id)][
  plays >= 30][
    order(-mean_commitment)]

print(final_player_scores)
# Append player + position to the table
player_lookup <- rbindlist(lapply(1:18, function(week) {
  input_path  <- sprintf("data/114239_nfl_competition_files_published_analytics_final/train/input_2023_w%02d.csv", week)
  fread(input_path)[, .(nfl_id, player_name, player_position)]
}))[, .SD[1], by = nfl_id]   # unique per nfl_id

final_with_names <- merge(
  final_player_scores,
  player_lookup,
  by = "nfl_id",
  all.x = TRUE
)
final_with_names <- final_with_names[order(-mean_commitment)]
View(final_with_names)
plot(final_with_names$mean_commitment, final_with_names$plays)

# Summarize by position
final_position_scores <- final_with_names[, .(
  players = .N,
  avg_mean_commitment = mean(mean_commitment, na.rm=TRUE),
  sd_mean_commitment = sd(mean_commitment, na.rm=TRUE)
), by=.(player_position)][
  order(-avg_mean_commitment)]
View(final_position_scores)

# Player rankings by position
final_rankings_by_position <- final_with_names[order(player_position, -mean_commitment)]
View(final_rankings_by_position)
# Save final results


