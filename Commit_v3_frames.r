library(data.table)
library(dplyr)

calc_commitment_frames <- function(dt, blx, bly) {
  if (nrow(dt) < 2) return(NULL)
  
  setorder(dt, frame_id)
  
  # Defender velocity
  dt[, dx := shift(x, type="lead") - x]
  dt[, dy := shift(y, type="lead") - y]
  
  # Speed
  dt[, speed := sqrt(dx^2 + dy^2)]
  
  # Vector to ball landing
  dt[, bx := blx - x]
  dt[, by := bly - y]
  dt[, bmag := sqrt(bx^2 + by^2)]
  dt[, ux := ifelse(bmag > 0, bx / bmag, 0)]
  dt[, uy := ifelse(bmag > 0, by / bmag, 0)]
  
  # Velocity unit vector
  dt[, pmag := sqrt(dx^2 + dy^2)]
  dt[, vx := ifelse(pmag > 0, dx / pmag, 0)]
  dt[, vy := ifelse(pmag > 0, dy / pmag, 0)]
  
  # Cosine similarity
  dt[, cosang := vx * ux + vy * uy]
  
  # Frame-level commitment
  dt[, commit := speed * cosang]
  
  # Add time since ball landed as a new column
  # Assuming the ball landed on the last frame (max frame_id)
  last_frame <- max(dt$frame_id)
  dt[, time_before_land := (last_frame - frame_id) / 10]  # Assuming 10 FPS
  
  dt[, time_before_land := ifelse(frame_id != last_frame, -time_before_land, 0)]
  
  # Return only relevant columns
  return(dt[, .(frame_id, commit, time_before_land)])
}

frame_results <- list()

idx <- 1

for (week in 1:18) {
  
  input  <- fread(sprintf("data/114239_nfl_competition_files_published_analytics_final/train/input_2023_w%02d.csv", week))
  output <- fread(sprintf("data/114239_nfl_competition_files_published_analytics_final/train/output_2023_w%02d.csv", week))
  
  defenders <- input[player_side=="Defense" & player_to_predict==1,
                     .(game_id, play_id, nfl_id, ball_land_x, ball_land_y)]
  defenders <- unique(defenders)
  
  for (i in seq_len(nrow(defenders))) {
    
    g  <- defenders$game_id[i]
    p  <- defenders$play_id[i]
    id <- defenders$nfl_id[i]
    blx <- defenders$ball_land_x[i]
    bly <- defenders$ball_land_y[i]
    
    post <- output[game_id==g & play_id==p & nfl_id==id]
    if (nrow(post) < 2) next
    
    # --- FRAME-BY-FRAME VALUES ---
    frames_dt <- calc_commitment_frames(post, blx, bly)
    
    # Attach identifying columns
    if (!is.null(frames_dt)) {
      frames_dt[, `:=`(
        game_id = g,
        play_id = p,
        nfl_id = id
      )]
      
      frame_results[[idx]] <- frames_dt
      idx <- idx + 1
    }
  }
}

all_frame_data <- rbindlist(frame_results, fill = TRUE) |>
  left_join(supplementary_data |>
              select(game_id, play_id, team_coverage_man_zone, team_coverage_type, pass_result, defensive_team),
            by = c("game_id", "play_id"))|> 
  left_join(final_with_names |>
              select(nfl_id, player_name, player_position), 
            by = ("nfl_id"))


# plot --------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(gganimate)

# Filter the data for the specific play (game_id = 2023090700, play_id = 101)
play_data <- all_frame_data[game_id == 2023090700 & play_id == 101,]

# Sort the data by time_before_land to make sure the animation progresses in order
play_data <- play_data[order(time_before_land)] |>
  arrange(play_id, nfl_id, time_before_land)

p <- ggplot(play_data, aes(x = time_before_land, y = commit, color = player_name, group = player_name)) +
  geom_line(linewidth = 1.2) +
  labs(
    title = "Play: J.Goff pass incomplete deep right to J.Reynolds",
    x = "Time Before Ball Lands",
    y = "Commit"
  ) +
  theme_minimal(base_size = 14) +
  transition_reveal(time_before_land)

goff_incomplete <- animate(p, nframes = 80, fps = 20, width = 900, height = 500)
anim_save("commitment_by_play.gif", animation = goff_incomplete)
print(goff_incomplete)

# Bland intercepts Howell
bland_interception_data <- all_frame_data[game_id == 2023112301 & play_id == 3879,] |>
  arrange(play_id, nfl_id, time_before_land)

bland_int <- ggplot(bland_interception_data, aes(x = time_before_land, y = commit, color = player_name, group = player_name)) +
  geom_line(linewidth = 1.2) +
  labs(
    title = "S. Howell Intercepted by D. Bland",
    x = "Time Before Ball Lands",
    y = "Commit"
  ) +
  theme_minimal(base_size = 14) +
  transition_reveal(time_before_land)

bland_int <- animate(bland_int, nframes = 80, fps = 20, width = 900, height = 500)
anim_save("bland_int.gif", animation = bland_int)
print(bland_int)
print("made it")
