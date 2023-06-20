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
                     stringsAsFactors=FALSE, nrows = 1000) %>% 
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

################################################################################
# Process Coaching Data
################################################################################

# Get Coach timeline data
coaching_dictionary <- game_data %>% 
  pivot_longer(., cols=c(home_coach, away_coach), names_to='home_away', values_to = 'coach') %>% 
  mutate(team = ifelse(home_away == 'home_coach',
                       home_team,
                       away_team)) %>% 
  dplyr::select(coach, season, week, team) %>% 
  distinct() %>% 
  arrange(coach, season, team) %>% 
  group_by(coach, team) %>% 
  summarize(stint_start = min(season),
            stint_end = max(season),
            week_start = min(week),
            week_end = max(week)) %>% 
  ungroup(.) %>% 
  arrange(coach, stint_start, week_start, team)

# Separate home and away data
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

# Get 3rd down coaching data
coaching_3rd_down <- coach_total_data %>% 
  filter(down == 3) %>% 
  group_by(coach, home_away, week, season) %>% 
  mutate(n_3rd_down = n(),
         n_3rd_down_pass = sum(play_type == 'pass', na.rm=TRUE),
         n_3rd_down_rush = sum(play_type == 'run', na.rm=TRUE),
         pct_3rd_down_deep_pass = sum(play_type == 'pass' & pass_length == 'deep', na.rm=TRUE) / n(),
         total_yrds_to_go_3rd_down = mean(ydstogo, na.rm=TRUE),
         total_3rd_down_conversion_rate = sum(third_down_converted, na.rm=TRUE) / n(),
         total_3rd_down_failed = sum(third_down_failed, na.rm=TRUE)) %>% 
  group_by(coach, home_away) %>% 
  summarize(mean_n_3rd_down = mean(n_3rd_down),
            mean_pass_rate_3rd_down = mean(n_3rd_down_pass / n_3rd_down),
            mean_pct_3rd_down_deep_pass = mean(pct_3rd_down_deep_pass),
            mean_3rd_down_conversion_rate = mean(total_3rd_down_conversion_rate),
            to_go_3rd_down = mean(ydstogo, na.rm=TRUE)) %>% 
  mutate_if(is.numeric, round, 3) %>% 
  pivot_wider(names_from = home_away, values_from = c(mean_n_3rd_down, mean_pass_rate_3rd_down,
                                                      mean_pct_3rd_down_deep_pass, mean_3rd_down_conversion_rate,
                                                      to_go_3rd_down), names_sep = "_")

# Get 4th down coaching data
coaching_4th_down <- coach_total_data %>% 
  filter(down == 4) %>% 
  group_by(coach, home_away, week, season) %>% 
  mutate(n_4th_down = n(),
         n_4th_down_pass = sum(play_type == 'pass', na.rm=TRUE),
         n_4th_down_rush = sum(play_type == 'run', na.rm=TRUE),
         pct_4th_down_deep_pass = sum(play_type == 'pass' & pass_length == 'deep', na.rm=TRUE) / n(),
         total_yrds_to_go_4th_down = mean(ydstogo, na.rm=TRUE),
         total_4th_down_conversion_rate = sum(fourth_down_converted, na.rm=TRUE) / n(),
         total_4th_down_failed = sum(fourth_down_failed, na.rm=TRUE)) %>% 
  group_by(coach, home_away) %>% 
  summarize(mean_n_4th_down = mean(n_4th_down),
            mean_pass_rate_4th_down = mean(n_4th_down_pass / n_4th_down),
            mean_pct_4th_down_deep_pass = mean(pct_4th_down_deep_pass),
            mean_4th_down_conversion_rate = mean(total_4th_down_conversion_rate),
            to_go_4th_down = mean(ydstogo, na.rm=TRUE)) %>% 
  mutate_if(is.numeric, round, 3) %>% 
  pivot_wider(names_from = home_away, values_from = c(mean_n_4th_down, mean_pass_rate_4th_down,
                                                      mean_pct_4th_down_deep_pass, mean_4th_down_conversion_rate,
                                                      to_go_4th_down), names_sep = "_")
  
# Get game-wise data
coaching_game_aggregation <- coach_total_data %>% 
  left_join(., game_data[c('game_id', 'home_score', 'away_score', 'winning_team')],
            by='game_id') %>% 
  group_by(coach, season, week, home_away) %>% 
  summarize(score = ifelse(home_away == 'home', 
                           home_score,
                           away_score),
            opposing_score = ifelse(home_away == 'away', 
                                    home_score,
                                    away_score),
            n_sack_allowed = sum(sack),
            n_fumble_allowed = sum(fumble),
            n_qb_hit_allowed = sum(qb_hit),
            n_pass_touchdown = sum(pass_touchdown),
            n_rush_touchdown = sum(rush_touchdown),
            n_interception_allowed = sum(interception),
            pct_1st_down_pass = sum(first_down_pass) / (sum(first_down_pass) + sum(first_down_rush)),
            n_1st_down_penalty = sum(first_down_penalty)) %>% 
  mutate(win = ifelse(score > opposing_score,
                      1,
                      0)) %>% 
  distinct()

coaching_win_loss <- coaching_game_aggregation %>% 
  group_by(coach) %>% 
  summarize(total_wins = sum(win, na.rm=TRUE),
            total_losses = sum(!win, na.rm=TRUE),
            total_win_pct = sum(win, na.rm=TRUE) / n(),
            points_scored = sum(score, na.rm=TRUE),
            points_allowed = sum(opposing_score, na.rm=TRUE),
            ppg = mean(score, na.rm=TRUE),
            ppg_allowed = mean(opposing_score, na.rm=TRUE),
            avg_differential = mean(score - opposing_score, na.rm=TRUE),
            avg_pass_td = mean(n_pass_touchdown),
            avg_rush_td = mean(n_rush_touchdown),
            avg_1st_down_penalty = mean(n_1st_down_penalty),
            avg_1st_down_pass_pct = mean(pct_1st_down_pass),
            avg_int_thrown = mean(n_interception_allowed),
            avg_fumbles_allowed = mean(n_fumble_allowed),
            avg_sack_allowed = mean(n_sack_allowed),
            avg_qb_hit_allowed = mean(n_qb_hit_allowed)) %>% 
  mutate_if(is.numeric, round, 3)

# Get home-away splits
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
  group_by(coach, home_away) %>% 
  summarize(wins = sum(total_wins),
            losses = sum(total_losses),
            win_pct = mean(total_win_pct),
            mean_ppg = mean(ppg),
            mean_ppg_allowed = mean(ppg_allowed),
            mean_differential = mean(avg_differential)) %>% 
  mutate_if(is.numeric, round, 3) %>% 
    pivot_wider(names_from = home_away, values_from = c(wins, losses,
                                                        win_pct, mean_ppg,
                                                        mean_ppg_allowed, mean_differential), names_sep = "_")

# Timeouts
timeout_data <- pbp_data %>% 
  filter(timeout == 1) %>% 
  mutate(season = substr(game_id, 1, 4),
         coach = ifelse(timeout_team == home_team,
                        home_coach,
                        away_coach)) %>% 
  group_by(game_id, coach, season, week, timeout_team, game_half) %>% 
  summarize(n_timeout = n(),
            first_timeout = max(half_seconds_remaining),
            last_timeout = min(half_seconds_remaining)) %>% 
  pivot_wider(names_from = game_half, values_from = c(n_timeout, first_timeout,
                                                      last_timeout), names_sep = "_") %>% 
  group_by(coach) %>% 
  summarize(avg_half1_timeouts_taken = mean(n_timeout_Half1, na.rm = TRUE),
            avg_half1_tr_first_timeout = mean(first_timeout_Half1, na.rm=TRUE),
            avg_half1_tr_last_timeout = mean(last_timeout_Half1, na.rm=TRUE),
            avg_half2_timeouts_taken = mean(n_timeout_Half2, na.rm = TRUE),
            avg_half2_tr_first_timeout = mean(first_timeout_Half2, na.rm=TRUE),
            avg_half2_tr_last_timeout = mean(last_timeout_Half2, na.rm=TRUE),
            avg_ot_tr_first_timeout = mean(first_timeout_Overtime, na.rm=TRUE),
            avg_ot_tr_last_timeout = mean(last_timeout_Overtime, na.rm=TRUE)) %>% 
  mutate_if(is.numeric, round, 1)

# combine
total_coaching <- coaching_win_loss %>% 
  left_join(., coaching_win_loss_home_away, by='coach') %>% 
  left_join(., coaching_3rd_down, by='coach') %>% 
  left_join(., coaching_4th_down, by='coach') %>% 
  left_join(., timeout_data, by='coach')

# coach_ppg_range <- range(c(total_coaching$ppg, 
#                            total_coaching$ppg_allowed), na.rm=TRUE)
# 
# coach_team_dictionary <- coaching_dictionary %>% 
#   group_by(coach) %>%
#   arrange(desc(stint_start)) %>%
#   slice(1) %>% 
#   left_join(., load_teams(), by=c('team' = 'team_abbr'))
# 
# total_coaching %>%
#   left_join(., coach_team_dictionary, by='coach') %>% 
#   ggplot() +
#   geom_point(aes(x = ppg, y = ppg_allowed, 
#              size=total_win_pct), color=coach_team_dictionary$team_color, 
#              alpha=0.6) + 
#   geom_hline(yintercept = mean(total_coaching$ppg_allowed), 
#              color = "red", linetype = "dashed", alpha=0.5) +
#   geom_vline(xintercept =  mean(total_coaching$ppg), 
#              color = "red", linetype = "dashed", alpha=0.5) +
#   ggrepel::geom_text_repel(aes(x = ppg, y = ppg_allowed, label=coach), size=2) +
#   #stat_smooth(aes(x = ppg_allowed, y = ppg), geom='line', alpha=0.5, se=FALSE, method='lm') +
#   coord_fixed(ratio = 1,
#               xlim = c(5, 35), 
#               ylim = c(37, 15)) +
#   labs(x = "Points Per Game",
#        y = "Opponent Points Per Game",
#        title = "Coach PPG",
#        size = "Win Percentage")

# Create File Path
folder_path <- file.path("data", "processed", "coaches")

if (!file.exists(folder_path)) {
  dir.create(folder_path)
}

# Save Data
data.table::fwrite(total_coaching, file.path(folder_path, 
                                           "processed_coaching_data.csv"),
                   row.names = FALSE)

# Cleanup
rm(away_data)
rm(coach_team_dictionary)
rm(coach_total_data)
rm(coaching_3rd_down)
rm(coaching_4th_down)
rm(coaching_aggregation_win_loss)
rm(coaching_dictionary)
rm(coaching_game_aggregation)
rm(coaching_passing_aggregation)
rm(coaching_win_loss)
rm(coaching_win_loss_home_away)
rm(home_data)
rm(timeout_data)
rm(total_coaching)
gc()

################################################################################
# Process Play-by-Play Data
################################################################################





