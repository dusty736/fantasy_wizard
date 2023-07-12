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
              "ydstogo", "play_type", "yards_gained", "first_down", "shotgun", "no_huddle",
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
                game_seconds_remaining, down, goal_to_go, ydstogo, first_down, play_type, yards_gained,
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
                game_seconds_remaining, down, goal_to_go, ydstogo, first_down, play_type, yards_gained,
                shotgun, qb_dropback, qb_scramble, pass_length, pass_location, air_yards,
                yards_after_catch, run_location, run_gap, first_down_pass, first_down_rush,
                first_down_penalty, third_down_converted, third_down_failed, fourth_down_converted,
                fourth_down_failed, qb_hit, interception, sack, pass_touchdown, rush_touchdown,
                two_point_attempt, fumble, timeout, total_home_score, total_away_score, score_differential) %>% 
  filter(play_type %in% c('run', 'pass'))

coach_total_data <- rbind(away_data, home_data)

# Early down
coaching_early_down <- coach_total_data %>% 
  filter(down %in% 1:2) %>% 
  group_by(coach, home_away) %>% 
  summarize(pct_early_down_pass = sum(play_type == 'pass', na.rm=TRUE) / n(),
         pct_early_down_rush = sum(play_type == 'run', na.rm=TRUE) / n(),
         pct_early_down_deep_pass = sum(play_type == 'pass' & pass_length == 'deep', na.rm=TRUE) / n(),
         pct_early_down_short_pass = sum(play_type == 'pass' & pass_length == 'short', na.rm=TRUE) / n(),
         total_yrds_gained_early_down = mean(yards_gained, na.rm=TRUE),
         early_down_conversion_rate = sum(first_down, na.rm=TRUE) / n()) %>% 
  mutate_if(is.numeric, round, 3) %>% 
  pivot_wider(names_from = home_away, values_from = c(pct_early_down_pass, 
                                                      pct_early_down_rush,
                                                      pct_early_down_deep_pass, 
                                                      pct_early_down_short_pass,
                                                      total_yrds_gained_early_down, 
                                                      total_early_down_conversion_rate), 
              names_sep = "_")

# Get 3rd down coaching data
coaching_3rd_down <- coach_total_data %>% 
  filter(down == 3) %>% 
  group_by(coach, home_away) %>% 
  summarize(to_go_third_down = mean(ydstogo, na.rm=TRUE),
         pct_third_down_pass = sum(play_type == 'pass', na.rm=TRUE) / n(),
         pct_third_down_rush = sum(play_type == 'run', na.rm=TRUE) / n(),
         pct_third_down_deep_pass = sum(play_type == 'pass' & pass_length == 'deep', na.rm=TRUE) / n(),
         pct_third_down_short_pass = sum(play_type == 'pass' & pass_length == 'short', na.rm=TRUE) / n(),
         mean_yrds_gained_third_down = mean(yards_gained, na.rm=TRUE),
         third_down_conversion_rate = sum(first_down, na.rm=TRUE) / n(),
         third_down_failed_rate = mean(third_down_failed, na.rm=TRUE)) %>% 
  mutate_if(is.numeric, round, 3) %>% 
  pivot_wider(names_from = home_away, values_from = c(to_go_third_down, 
                                                      pct_third_down_pass,
                                                      pct_third_down_rush, 
                                                      pct_third_down_deep_pass,
                                                      pct_third_down_short_pass,
                                                      mean_yrds_gained_third_down,
                                                      third_down_conversion_rate,
                                                      third_down_failed_rate), 
              names_sep = "_")

# Get 4th down coaching data
coaching_4th_down <- coach_total_data %>% 
  filter(down == 4) %>% 
  group_by(coach, home_away) %>% 
  summarize(to_go_fourth_down = mean(ydstogo, na.rm=TRUE),
            pct_fourth_down_pass = sum(play_type == 'pass', na.rm=TRUE) / n(),
            pct_fourth_down_rush = sum(play_type == 'run', na.rm=TRUE) / n(),
            pct_fourth_down_deep_pass = sum(play_type == 'pass' & pass_length == 'deep', na.rm=TRUE) / n(),
            pct_fourth_down_short_pass = sum(play_type == 'pass' & pass_length == 'short', na.rm=TRUE) / n(),
            mean_yrds_gained_fourth_down = mean(yards_gained, na.rm=TRUE),
            fourth_down_conversion_rate = sum(first_down, na.rm=TRUE) / n(),
            fourth_down_failed_rate = mean(third_down_failed, na.rm=TRUE)) %>% 
  mutate_if(is.numeric, round, 3) %>% 
  pivot_wider(names_from = home_away, values_from = c(to_go_fourth_down, 
                                                      pct_fourth_down_pass,
                                                      pct_fourth_down_rush, 
                                                      pct_fourth_down_deep_pass,
                                                      pct_fourth_down_short_pass,
                                                      mean_yrds_gained_fourth_down,
                                                      fourth_down_conversion_rate,
                                                      fourth_down_failed_rate), 
              names_sep = "_")


  
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

# I need play by play where the id column is the game clock and possession team
# Home and away teams will have accumulating variables like n_sack_home, n_sack_away

defense <- pbp_data %>% 
  filter(!(desc %in% c('GAME', 'END QUARTER 1', 'Two-Minute Warning', 'END QUARTER 2',
                     'END QUARTER 3', 'END GAME'))) %>% 
  dplyr::select(game_id, week, home_team, away_team, defteam, time, quarter_seconds_remaining,
                half_seconds_remaining, game_seconds_remaining, qtr, game_half, 
                yrdln, down, goal_to_go, play_type, yards_gained,
                home_timeouts_remaining, away_timeouts_remaining, posteam_score,
                defteam_score, third_down_failed, fourth_down_failed, incomplete_pass, interception,
                fumble_forced, fumble_lost, solo_tackle, safety, tackled_for_loss,
                qb_hit, sack) %>% 
  # Add score for defense
  mutate(defense_lead = ifelse(defteam_score > posteam_score,
                               1,
                               0),
         post_2min_warning = ifelse(half_seconds_remaining <= 120,
                                    1,
                                    0)) %>% 
  mutate(across(where(is.numeric), ~ replace(., is.na(.), 0))) %>% 
  group_by(game_id, defteam) %>% 
  mutate(n_interception = cumsum(interception),
         n_fumble_forced = cumsum(fumble_forced),
         n_fumbled_recovered = cumsum(fumble_lost),
         n_incomplete_pass = cumsum(incomplete_pass),
         n_solo_tackle = cumsum(solo_tackle),
         n_tackled_for_loss = cumsum(tackled_for_loss),
         n_qb_hit = cumsum(qb_hit),
         n_sack = cumsum(sack),
         n_third_down_stopped = cumsum(third_down_failed),
         n_fourth_down_stopped = cumsum(fourth_down_failed)) %>% 
  dplyr::select(game_id, week, defteam, time, qtr, n_interception, n_incomplete_pass,
                n_solo_tackle, n_tackled_for_loss, n_qb_hit, n_sack, n_fumble_forced,
                n_fumbled_recovered, n_third_down_stopped, n_fourth_down_stopped)

offense <- pbp_data %>% 
  filter(!(desc %in% c('GAME', 'END QUARTER 1', 'Two-Minute Warning', 'END QUARTER 2',
                       'END QUARTER 3', 'END GAME'))) %>% 
  dplyr::select(game_id, week, home_team, away_team, posteam, time, quarter_seconds_remaining,
                half_seconds_remaining, game_seconds_remaining, qtr, game_half, 
                yrdln, down, goal_to_go, play_type, ydstogo, yards_gained,
                defteam_timeouts_remaining, posteam_timeouts_remaining, posteam_score,
                defteam_score, passer_player_name, receiver_player_name, air_yards, yards_after_catch,
                receiving_yards, incomplete_pass, complete_pass, rusher_player_name, 
                rushing_yards, interception, fumble_lost, tackled_for_loss,
                qb_hit, sack, posteam_score, defteam_score, posteam_timeouts_remaining,
                first_down, first_down, third_down_converted, third_down_failed,
                fourth_down_converted, fourth_down_failed, pass_touchdown,
                rush_touchdown, field_goal_attempt, field_goal_result, drive_first_downs, 
                drive_inside20, drive_yards_penalized) %>% 
  mutate(current_first_down = ifelse(down == 1 & play_type %in% c('run', 'pass'),
                                     1,
                                     0),
         current_first_down_converted = ifelse(down == 1 & play_type %in% c('run', 'pass') & first_down == 1,
                                     1,
                                     0),
         current_second_down = ifelse(down == 2 & play_type %in% c('run', 'pass'),
                                      1,
                                      0),
         current_second_down_converted = ifelse(down == 2 & play_type %in% c('run', 'pass') & first_down == 1,
                                      1,
                                      0),
         current_third_down = ifelse(down == 3 & play_type %in% c('run', 'pass'),
                                     1,
                                     0),
         current_third_down_converted = ifelse(down == 3 & play_type %in% c('run', 'pass') & first_down == 1,
                                     1,
                                     0),
         current_forth_down = ifelse(down == 4 & play_type %in% c('run', 'pass'),
                                 1,
                                 0),
         current_forth_down_converted = ifelse(down == 4 & play_type %in% c('run', 'pass') & first_down == 1,
                                     1,
                                     0),
         rush = ifelse(play_type == 'run',
                         1,
                         0),
         pass = ifelse(play_type == 'pass',
                         1,
                         0),
         pass_0_10_attempt = ifelse(play_type == 'pass' & air_yards <= 10,
                             1,
                             0),
         pass_0_10_complete = ifelse(play_type == 'pass' & air_yards <= 10 & complete_pass == 1,
                                    1,
                                    0),
         pass_10_20_attempt = ifelse(play_type == 'pass' & (air_yards > 10 & air_yards <= 20),
                                    1,
                                    0),
         pass_10_20_complete = ifelse(play_type == 'pass' & (air_yards > 10 & air_yards <= 20) & complete_pass == 1,
                                     1,
                                     0),
         pass_20_30_attempt = ifelse(play_type == 'pass' & (air_yards > 20 & air_yards <= 30),
                                     1,
                                     0),
         pass_20_30_complete = ifelse(play_type == 'pass' & (air_yards > 20 & air_yards <= 30) & complete_pass == 1,
                                      1,
                                      0),
         pass_30_40_attempt = ifelse(play_type == 'pass' & (air_yards > 30 & air_yards <= 40),
                                     1,
                                     0),
         pass_30_40_complete = ifelse(play_type == 'pass' & (air_yards > 30 & air_yards <= 40) & complete_pass == 1,
                                      1,
                                      0),
         pass_40_plus_attempt = ifelse(play_type == 'pass' & air_yards > 40,
                                     1,
                                     0),
         pass_40_plus_complete = ifelse(play_type == 'pass' & air_yards > 40 & complete_pass == 1,
                                      1,
                                      0),
         rush_negative = ifelse(play_type == 'run' & rushing_yards <= 0,
                                1,
                                0),
         rush_positive = ifelse(play_type == 'run' & rushing_yards > 0,
                                1,
                                0),
         rush_10_plus = ifelse(play_type == 'run' & rushing_yards > 10,
                                1,
                                0),
         off_lead = ifelse(posteam_score > defteam_score,
                           1,
                           0)) %>% 
  mutate(across(where(is.numeric), ~ replace(., is.na(.), 0))) %>% 
  group_by(game_id, posteam) %>% 
  mutate(n_rush = cumsum(rush),
         n_rush_yards = cumsum(rushing_yards),
         n_rush_negative = cumsum(rush_negative),
         n_rush_positive = cumsum(rush_positive),
         pct_rush_10_plus = cumsum(rush_10_plus) / n_rush,
         n_rush_td = cumsum(rush_touchdown),
         n_pass = cumsum(pass),
         n_air_yards = cumsum(air_yards),
         n_receiving_yards = cumsum(receiving_yards),
         n_receiving_yac = cumsum(yards_after_catch),
         n_passing_td = cumsum(pass_touchdown),
         completion_pct = 1 - cumsum(incomplete_pass) / cumsum(pass),
         completion_pct_0_10 = cumsum(pass_0_10_complete) / cumsum(pass_0_10_attempt),
         completion_pct_10_20 = cumsum(pass_10_20_complete) / cumsum(pass_10_20_attempt),
         completion_pct_20_30 = cumsum(pass_20_30_complete) / cumsum(pass_20_30_attempt),
         completion_pct_30_40 = cumsum(pass_30_40_complete) / cumsum(pass_30_40_attempt),
         completion_pct_40_plus = cumsum(pass_40_plus_complete) / cumsum(pass_40_plus_attempt),
         pct_mtc_first_down = cumsum(current_first_down_converted) / cumsum(current_first_down),
         pct_mtc_second_down = cumsum(current_second_down_converted) / cumsum(current_second_down),
         pct_mtc_third_down = cumsum(current_third_down_converted) / cumsum(current_third_down),
         pct_mtc_fourth_down = cumsum(current_forth_down_converted) / cumsum(current_forth_down),
         n_drive_yards_penalized = cumsum(drive_yards_penalized)) %>% 
  dplyr::select(game_id, posteam, time, qtr, off_lead, n_rush, n_rush_yards, 
                n_rush_negative, n_rush_positive, pct_rush_10_plus, n_rush_td,
                n_pass, n_air_yards, n_receiving_yards, n_receiving_yac, 
                n_passing_td, completion_pct, completion_pct_0_10, completion_pct_10_20,
                completion_pct_20_30, completion_pct_30_40, completion_pct_40_plus, 
                pct_mtc_first_down, pct_mtc_second_down, pct_mtc_third_down, pct_mtc_fourth_down,
                n_drive_yards_penalized) %>% 
  mutate_if(is.numeric, round, 2)

# combine
pbp_eng <- offense %>% 
  left_join(., defense, by=c('game_id', 'time', 'qtr'))









