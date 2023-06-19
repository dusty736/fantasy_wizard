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

# Get coaching decision data
away_data <- pbp_data %>% 
  mutate(season = substr(game_id, 1, 4),
         score_differential = total_away_score - total_home_score,
         home_away = 'away') %>% 
  filter(posteam == away_team) %>% 
  rename(coach = away_coach,
         timeouts_remaining = away_timeouts_remaining,
         team = away_team,
         opposing_team = home_team) %>% 
  dplyr::select(season, week, game_id, coach, home_away, team, opposing_team, yardline_100, half_seconds_remaining,
                game_seconds_remaining, down, goal_to_go, ydstogo, play_type, yards_gained,
                shotgun, qb_dropback, qb_scramble, pass_length, pass_location, air_yards,
                yards_after_catch, run_location, run_gap, first_down_pass, first_down_rush,
                first_down_penalty, third_down_converted, third_down_failed, fourth_down_converted,
                fourth_down_failed, qb_hit, interception, sack, pass_touchdown, rush_touchdown,
                two_point_attempt, fumble, timeout, total_home_score, total_away_score, score_differential) %>% 
  filter(play_type %in% c('run', 'pass'))

home_data <- pbp_data %>% 
  mutate(season = substr(game_id, 1, 4),
         score_differential = total_away_score - total_home_score,
         home_away = 'home') %>% 
  filter(posteam == home_team) %>% 
  rename(coach = home_coach,
         timeouts_remaining = home_timeouts_remaining,
         team = home_team,
         opposing_team = away_team) %>% 
  dplyr::select(season, week, game_id, coach, home_away, team, opposing_team, yardline_100, half_seconds_remaining,
                game_seconds_remaining, down, goal_to_go, ydstogo, play_type, yards_gained,
                shotgun, qb_dropback, qb_scramble, pass_length, pass_location, air_yards,
                yards_after_catch, run_location, run_gap, first_down_pass, first_down_rush,
                first_down_penalty, third_down_converted, third_down_failed, fourth_down_converted,
                fourth_down_failed, qb_hit, interception, sack, pass_touchdown, rush_touchdown,
                two_point_attempt, fumble, timeout, total_home_score, total_away_score, score_differential) %>% 
  filter(play_type %in% c('run', 'pass'))

coach_total_data <- rbind(away_data, home_data)

# coaching variable ideas
# most common play on each down
# 3rd down conversion rate
# 4th down attempt rate
# 4th down conversion rate
# Run/pass ratio
# Pass formation ratio
# short/long pass ratio
# Average time for using timeouts in each half
# win/loss record
# Home/away differences

coaching_passing_aggregation <- coach_total_data %>% 
  group_by(coach, week, season) %>% 
  mutate(n_offensive_plays = n(),
         n_3rd_down = sum(down == 3 & (third_down_converted | third_down_failed) , na.rm=TRUE),
         n_3rd_down_pass = sum(down == 3 & play_type == 'pass', na.rm=TRUE),
         n_3rd_down_rush = sum(down == 3 & play_type == 'run', na.rm=TRUE),
         n_3rd_down_converted = sum(third_down_converted, na.rm=TRUE),
         n_3rd_down_failed = sum(third_down_failed, na.rm=TRUE),
         yrds_to_go_3rd_down = mean(ydstogo, na.rm=TRUE),
         n_4th_down = sum(down == 4, na.rm=TRUE),
         n_4th_down_pass = sum(down == 4 & play_type == 'pass', na.rm=TRUE),
         n_4th_down_rush = sum(down == 4 & play_type == 'run', na.rm=TRUE),
         n_4th_down_converted = sum(fourth_down_converted, na.rm=TRUE),
         n_4th_down_failed = sum(fourth_down_failed, na.rm=TRUE),
         ) %>% 
  filter(play_type == 'pass') %>% 
  summarize()

coaching_3rd_down <- coach_total_data %>% 
  filter(down == 3) %>% 
  group_by(coach, home_away, week, season) %>% 
  mutate(n_3rd_down = n(),
         n_3rd_down_pass = sum(play_type == 'pass', na.rm=TRUE),
         n_3rd_down_rush = sum(play_type == 'run', na.rm=TRUE),
         total_yrds_to_go_3rd_down = mean(ydstogo, na.rm=TRUE),
         total_3rd_down_conversion_rate = sum(third_down_converted, na.rm=TRUE) / n(),
         total_3rd_down_failed = sum(third_down_failed, na.rm=TRUE)) %>% 
  group_by(coach, home_away, season) %>% 
  summarize(mean_n_3rd_down = mean(n_3rd_down),
            mean_pass_rate_3rd_down = mean(n_3rd_down_pass / n_3rd_down),
            mean_3rd_down_conversion_rate = mean(total_3rd_down_conversion_rate),
            to_go_3rd_down = mean(ydstogo, na.rm=TRUE)) %>% 
  mutate_if(is.numeric, round, 3)

coaching_4th_down <- coach_total_data %>% 
  filter(down == 4) %>% 
  group_by(coach, home_away, week, season) %>% 
  mutate(n_4th_down = n(),
         n_4th_down_pass = sum(play_type == 'pass', na.rm=TRUE),
         n_4th_down_rush = sum(play_type == 'run', na.rm=TRUE),
         total_yrds_to_go_4th_down = mean(ydstogo, na.rm=TRUE),
         total_4th_down_conversion_rate = sum(fourth_down_converted, na.rm=TRUE) / n(),
         total_4th_down_failed = sum(fourth_down_failed, na.rm=TRUE)) %>% 
  group_by(coach, home_away, season) %>% 
  summarize(mean_n_4th_down = mean(n_4th_down),
            mean_pass_rate_4th_down = mean(n_4th_down_pass / n_4th_down),
            mean_4th_down_conversion_rate = mean(total_4th_down_conversion_rate),
            to_go_4th_down = mean(ydstogo, na.rm=TRUE)) %>% 
  mutate_if(is.numeric, round, 3)
  

coaching_game_aggregation <- coach_total_data %>% 
  left_join(., game_data[c('game_id', 'home_score', 'away_score', 'winning_team')],
            by='game_id') %>% 
  group_by(coach, season, week, home_away) %>% 
  summarize(score = ifelse(home_away == 'home', 
                           home_score,
                           away_score),
            opposing_score = ifelse(home_away == 'away', 
                                    home_score,
                                    away_score)) %>% 
  mutate(win = ifelse(score > opposing_score,
                      1,
                      0)) %>% 
  distinct()

coaching_win_loss <- coaching_game_aggregation %>% 
  group_by(coach, season) %>% 
  summarize(total_wins = sum(win, na.rm=TRUE),
            total_losses = sum(!win, na.rm=TRUE),
            total_win_pct = sum(win, na.rm=TRUE) / n(),
            points_scored = sum(score, na.rm=TRUE),
            points_allowed = sum(opposing_score, na.rm=TRUE),
            ppg = mean(score, na.rm=TRUE),
            ppg_allowed = mean(opposing_score, na.rm=TRUE),
            avg_differential = mean(score - opposing_score, na.rm=TRUE)) %>% 
  mutate_if(is.numeric, round, 3)

coaching_win_loss_home_away <- coaching_game_aggregation %>% 
  group_by(coach, season, home_away) %>% 
  summarize(total_wins = sum(win, na.rm=TRUE),
            total_losses = sum(!win, na.rm=TRUE),
            total_win_pct = sum(win, na.rm=TRUE) / n(),
            points_scored = sum(score, na.rm=TRUE),
            points_allowed = sum(opposing_score, na.rm=TRUE),
            ppg = mean(score, na.rm=TRUE),
            ppg_allowed = mean(opposing_score, na.rm=TRUE),
            avg_differential = mean(score - opposing_score, na.rm=TRUE)) %>% 
  mutate_if(is.numeric, round, 3)

