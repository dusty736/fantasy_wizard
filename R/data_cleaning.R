################################################################################
# Load Libraries
################################################################################

require(tidyverse)
require(lubridate)

################################################################################
# Create File Path to Data Folder
################################################################################

if (!file.exists(file.path("data", "processed"))) {
  dir.create(file.path("data", "processed"))
}

################################################################################
# Process Play-by-Play Data
################################################################################

# Set game variables
pbp_vars <- c("play_id", "game_id", "old_game_id", "home_team", "away_team",
              "season_type", "week", "posteam", "posteam_type", "defteam", 
              "side_of_field", "yardline_100", "quarter_seconds_remaining", 
              "half_seconds_remaining", "game_seconds_remaining", "game_half",
              "quarter_end", "drive", "qtr", "down", "goal_to_go", "time",
              "ydstogo", "play_type", "yards_gained", "shotgun", "no_huddle",
              "qb_dropback", "qb_kneel", "qb_spike", "qb_scramble", 
              "pass_length", "pass_location", "air_yards", "yards_after_catch",
              "run_location", "run_gap", "field_goal_result", "kick_distance",
              "extra_point_result", "two_point_conv_result", 
              "home_timeouts_remaining", "away_timeouts_remaining", "timeout",
              "timeout_team", "td_team", "td_player_id", 
              "posteam_timeouts_remaining", "defteam_timeouts_remaining", 
              "total_home_score", "total_away_score", "posteam_score", 
              "defteam_score", "score_differential", "first_down_rush",
              "first_down_pass", "first_down_penalty", "third_down_converted", 
              "third_down_failed", "fourth_down_converted", "fourth_down_failed",
              "incomplete_pass", "interception", "qb_hit", "rush_attempt", 
              "pass_attempt", "sack", "touchdown", "pass_touchdown", 
              "rush_touchdown", "two_point_attempt", "extra_point_attempt",
              "field_goal_attempt", "fumble", "complete_pass", "lateral_reception",
              "lateral_rush", "passing_yards", "receiver_player_id", "receiving_yards",
              "rusher_player_id", "rushing_yards", "lateral_rusher_player_id",
              "lateral_rushing_yards", "interception_player_id", "series", 
              "series_success", "series_result", "start_time", "time_of_day",
              "stadium", "weather", "play_type_nfl", "surface", "roof", "temp",
              "wind", "home_coach", "away_coach", "stadium_id")

# Load data
pbp_data <- data.table::fread("data/raw/pbp/pbp_raw_data.csv",
                     stringsAsFactors=FALSE) %>% 
  dplyr::select(pbp_vars)

# Get game data
game_vars <- c("game_id", "old_game_id", "week", "home_team", "away_team",
               "season_type", "home_coach", "away_coach", "start_time",
               "stadium", "weather", "surface", "roof", 
               "temp", "wind", "total_home_score", "total_away_score")

game_data <- pbp_data %>% 
  dplyr::select(game_vars) %>% 
  mutate(season = substr(game_id, 1, 4)) %>% 
  group_by(season, game_id, old_game_id, week, home_team, away_team,
           season_type, home_coach, away_coach, start_time,
           stadium, weather, surface, roof, 
           temp, wind) %>% 
  summarize(home_score = max(total_home_score),
            away_score = max(total_away_score),
            winning_team = ifelse(max(total_home_score) > max(total_away_score),
                                  home_team,
                                  away_team)) %>% 
  ungroup(.)

# Get Coach data
coaching_dictionary <- game_data %>% 
  pivot_longer(., cols=c(home_coach, away_coach), names_to='home_away', values_to = 'coach') %>% 
  mutate(team = ifelse(home_away == 'home_coach',
                       home_team,
                       away_team)) %>% 
  dplyr::select(coach, season, team) %>% 
  distinct() %>% 
  arrange(coach, season, team) %>% 
  group_by(coach, team) %>% 
  summarize(stint_start = min(season),
            stint_end = max(season)) %>% 
  ungroup(.) %>% 
  arrange(coach, stint_start, team)
