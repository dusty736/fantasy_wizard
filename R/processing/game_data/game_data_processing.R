################################################################################
# Game prediction variables
################################################################################

# Model Description:
#   - Build a model to predict the win-loss outcome of a game.  The target with 
#   be a 1 or 0 for the home team indicating whether they won or not.

# Variables: 
#   - Home Team
#   - Away Team
#   - Week
#   - Home Coach
#   - Away Coach
#   - Home QB
#   - Away QB
#   - Stadium status (dome, <40, >80)
#   - Field Status (Turf or grass)
#   - Vegas line
#   - Win pct last 5 games
#   - Win pct last season
#   - OFF PPG
#   - OFF YPG
#   - OFF TD
#   - OFF TO
#   - DEF PPG
#   - DEF TD
#   - DEF TO 

################################################################################
# Load Libraries
################################################################################

require(tidyverse)
require(lubridate)

################################################################################
# Get game data
################################################################################

schedule_data <- nflfastR::fast_scraper_schedules(2000:2023) %>% 
  pivot_longer(., cols=c(away_team, home_team), names_to='home_away', values_to='team') %>% 
  mutate(opposing_team = ifelse(home_away == 'home_team',
                                str_split(game_id, "_", simplify = TRUE)[, 3],
                                str_split(game_id, "_", simplify = TRUE)[, 4]),
         season = as.character(season),
         qb = ifelse(home_away == 'home_team',
                     home_qb_name,
                     away_qb_name),
         coach = ifelse(home_away == 'home_team',
                        home_coach,
                        away_coach),
         team = ifelse(team == 'STL',
                       'LA',
                       ifelse(team == 'SD',
                              'LAC',
                              ifelse(team == 'OAK',
                                     'LV',
                                     team))),
         opposing_team = ifelse(opposing_team == 'STL',
                                'LA',
                                ifelse(opposing_team == 'SD',
                                       'LAC',
                                       ifelse(opposing_team == 'OAK',
                                              'LV',
                                              opposing_team)))) %>% 
  filter(home_away == 'home_team') %>%
  rename(opposing_qb = away_qb_name,
         opposing_coach = away_coach) %>% 
  dplyr::select(game_id, season, week, team, opposing_team, stadium_id, coach, qb, opposing_qb,
                opposing_coach, game_type, weekday, gametime, result, overtime,
                home_rest, away_rest, spread_line, total_line, div_game, roof,
                surface, temp, wind) %>% 
  filter(result != 0) %>%  # Remove ties
  filter(surface != '')

################################################################################
# Feature Engineering
################################################################################

game_data <- schedule_data %>% 
  # Result
  mutate(point_diff = result) %>% 
  mutate(result = ifelse(result > 0, 1, 0)) %>% 
  # Rest
  mutate(home_rest = ifelse(home_rest < 7, 'short',
                       ifelse(home_rest < 10, 'medium',
                              'long')),
         away_rest = ifelse(away_rest < 7, 'short',
                            ifelse(away_rest < 10, 'medium',
                                   'long'))) %>% 
  # Conditions
  mutate(temp_conditions = ifelse(roof %in% c('dome', 'closed'),
                       'indoor',
                       ifelse(temp < 40,
                              'cold',
                              ifelse(temp < 80,
                                     'medium',
                                     'hot')))) %>% 
  mutate(wind_conditions = ifelse(roof %in% c('dome', 'closed'),
                             'indoor',
                             ifelse(wind < 10,
                                    'low',
                                    ifelse(temp < 20,
                                           'medium',
                                           'high')))) %>% 
  # Surface
  mutate(surface = ifelse(surface %in% c('grass', 'grass', 'dessograss'),
                       'grass',
                       'turf')) %>% 
  # Home favored
  mutate(favored = ifelse(spread_line > 0,
                          1,
                          0)) %>% 
  # Game Time
  mutate(start_hour = ifelse(weekday != 'Monday',
                             as.numeric(substr(gametime, 1, 2)),
                             21)) %>% 
  mutate(game_window = ifelse(start_hour < 16 & start_hour > 8,
                           'morning',
                           ifelse(start_hour < 20,
                                  'afternoon',
                                  'evening'))) %>% 
  # Organize colomns
  dplyr::select(game_id, season, week, game_type, stadium_id, weekday, game_window,
                team, opposing_team,  coach, opposing_coach, qb, opposing_qb,
                overtime, home_rest, away_rest, div_game, roof, temp_conditions,
                wind_conditions, spread_line, favored, result)

# Get result map
result_map <- nflfastR::fast_scraper_schedules(2000:2023) %>% 
  filter(home_score != away_score) %>% 
  mutate(winning_team = ifelse(home_score > away_score, home_team, away_team)) %>% 
  dplyr::select(game_id, winning_team) %>% 
  distinct()

# Get scores map
home_score_map <- nflfastR::fast_scraper_schedules(2000:2023) %>% 
  filter(home_score != away_score) %>% 
  dplyr::select(game_id, home_team, home_score) %>% 
  distinct() %>% 
  rename(team = home_team,
         score = home_score)

away_score_map <- nflfastR::fast_scraper_schedules(2000:2023) %>% 
  filter(home_score != away_score) %>% 
  dplyr::select(game_id, away_team, away_score) %>% 
  distinct() %>% 
  rename(team = away_team,
         score = away_score)

score_map <- rbind(home_score_map, away_score_map) %>% 
  arrange(game_id, team)
  
################################################################################
# Offense Season Stats
################################################################################

# Get pbp data
pbp_data <- data.table::fread("data/raw/pbp/pbp_raw_data.csv")

# Home data
home_team_off_stats <- pbp_data %>% 
  mutate(season = substr(game_id, 1, 4),
         week = substr(game_id, 6, 7),
         coach = home_coach) %>% 
  filter(posteam == home_team) %>% 
  dplyr::select(season, week, game_id, coach, home_team, play_type,
                first_down, yards_gained, first_down_pass, first_down_rush,
                first_down_penalty, interception, fumble, sack, pass_touchdown, 
                rush_touchdown, penalty_team, penalty_yards, home_score) %>% 
  rename(points_scored = home_score) %>% 
  filter(play_type %in% c('run', 'pass')) %>% 
  group_by(game_id, home_team, season, week, points_scored) %>% 
  summarize(n_play = n(),
            run_pct = sum(play_type == 'run') / n(),
            pass_pct = sum(play_type == 'pass') / n(),
            n_rush_yards = sum(ifelse(play_type == 'run',
                                      yards_gained,
                                      0)),
            n_pass_yards = sum(ifelse(play_type == 'pass',
                                      yards_gained,
                                      0)),
            n_rush_td = sum(rush_touchdown),
            n_pass_td = sum(pass_touchdown),
            n_first_down = sum(first_down),
            n_fumble = sum(fumble),
            n_sack = sum(sack),
            n_interception = sum(sack)) %>% 
  arrange(season, home_team, week) %>% 
  rename(team = home_team) %>% 
  mutate(home_away = 'home')

# Away data
away_team_off_stats <- pbp_data %>% 
  mutate(season = substr(game_id, 1, 4),
         week = substr(game_id, 6, 7),
         coach = home_coach) %>% 
  filter(posteam == away_team) %>% 
  dplyr::select(season, week, game_id, coach, away_team, play_type,
                first_down, yards_gained, first_down_pass, first_down_rush,
                first_down_penalty, interception, fumble, sack, pass_touchdown, 
                rush_touchdown, penalty_team, penalty_yards, away_score) %>% 
  rename(points_scored = away_score) %>% 
  filter(play_type %in% c('run', 'pass')) %>% 
  group_by(game_id, away_team, season, week, points_scored) %>% 
  summarize(n_play = n(),
            run_pct = sum(play_type == 'run') / n(),
            pass_pct = sum(play_type == 'pass') / n(),
            n_rush_yards = sum(ifelse(play_type == 'run',
                                      yards_gained,
                                      0)),
            n_pass_yards = sum(ifelse(play_type == 'pass',
                                      yards_gained,
                                      0)),
            n_rush_td = sum(rush_touchdown),
            n_pass_td = sum(pass_touchdown),
            n_first_down = sum(first_down),
            n_fumble = sum(fumble),
            n_sack = sum(sack),
            n_interception = sum(sack)) %>% 
  arrange(season, away_team, week) %>% 
  rename(team = away_team) %>% 
  mutate(home_away = 'away')

# Combine
total_offense <- rbind(away_team_off_stats, home_team_off_stats) %>% 
  arrange(season, team, week) %>% 
  left_join(., result_map, by='game_id') %>% 
  mutate(result = ifelse(team == winning_team, 1, 0)) %>% 
  dplyr::select(game_id, team, home_away, season, week, result, points_scored, everything(), -winning_team)

################################################################################
# Defense Season Stats
################################################################################

# Home data
home_team_def_stats <- pbp_data %>% 
  mutate(season = substr(game_id, 1, 4),
         week = substr(game_id, 6, 7),
         coach = home_coach) %>% 
  filter(defteam == home_team) %>% 
  dplyr::select(season, week, game_id, coach, home_team, play_type,
                first_down, yards_gained, first_down_pass, first_down_rush,
                first_down_penalty, interception, fumble, sack, pass_touchdown, 
                rush_touchdown, penalty_team, penalty_yards, away_score) %>% 
  rename(points_allowed = away_score) %>% 
  filter(play_type %in% c('run', 'pass')) %>% 
  group_by(game_id, home_team, season, week, points_allowed) %>% 
  summarize(n_play = n(),
            run_pct = sum(play_type == 'run') / n(),
            pass_pct = sum(play_type == 'pass') / n(),
            n_rush_yards = sum(ifelse(play_type == 'run',
                                      yards_gained,
                                      0)),
            n_pass_yards = sum(ifelse(play_type == 'pass',
                                      yards_gained,
                                      0)),
            n_rush_td = sum(rush_touchdown),
            n_pass_td = sum(pass_touchdown),
            n_first_down = sum(first_down),
            n_fumble = sum(fumble),
            n_sack = sum(sack),
            n_interception = sum(sack)) %>% 
  arrange(season, home_team, week) %>% 
  rename(team = home_team) %>% 
  mutate(home_away = 'home')

# Away data
away_team_def_stats <- pbp_data %>% 
  mutate(season = substr(game_id, 1, 4),
         week = substr(game_id, 6, 7),
         coach = home_coach) %>% 
  filter(defteam == away_team) %>% 
  dplyr::select(season, week, game_id, coach, away_team, play_type,
                first_down, yards_gained, first_down_pass, first_down_rush,
                first_down_penalty, interception, fumble, sack, pass_touchdown, 
                rush_touchdown, penalty_team, penalty_yards, home_score) %>% 
  rename(points_allowed = home_score) %>% 
  filter(play_type %in% c('run', 'pass')) %>% 
  group_by(game_id, away_team, season, week, points_allowed) %>% 
  summarize(n_play = n(),
            run_pct = sum(play_type == 'run') / n(),
            pass_pct = sum(play_type == 'pass') / n(),
            n_rush_yards = sum(ifelse(play_type == 'run',
                                      yards_gained,
                                      0)),
            n_pass_yards = sum(ifelse(play_type == 'pass',
                                      yards_gained,
                                      0)),
            n_rush_td = sum(rush_touchdown),
            n_pass_td = sum(pass_touchdown),
            n_first_down = sum(first_down),
            n_fumble = sum(fumble),
            n_sack = sum(sack),
            n_interception = sum(sack)) %>% 
  arrange(season, away_team, week) %>% 
  rename(team = away_team) %>% 
  mutate(home_away = 'away')

# Combine
total_defense <- rbind(away_team_def_stats, home_team_def_stats) %>% 
  arrange(season, team, week) %>% 
  left_join(., result_map, by='game_id') %>% 
  mutate(result = ifelse(team == winning_team, 1, 0)) %>% 
  dplyr::select(game_id, team, home_away, season, week, result, points_allowed, everything(), -winning_team)

################################################################################
# Combine
################################################################################

total_games_stats <- total_offense %>% 
  left_join(., total_defense, by=c('game_id', 'team', 'home_away', 'season', 'week', 'result'),
            suffix=c("_offense", "_defense")) %>% 
  left_join(., score_map, by=c('game_id', 'team'))

################################################################################
# Get Season Stats
################################################################################

season_stats <- total_games_stats %>% 
  group_by(team, season) %>% 
  summarize(prev_season_win_pct = mean(result, na.rm=TRUE),
            prev_season_off_ppg = mean(points_scored, na.rm=TRUE),
            prev_season_off_plays_per_game = mean(n_play_offense, na.rm=TRUE),
            prev_season_off_run_pct = mean(run_pct_offense, na.rm=TRUE),
            prev_season_off_pass_pct = mean(pass_pct_offense, na.rm=TRUE),
            prev_season_off_pypg = mean(n_pass_yards_offense, na.rm=TRUE),
            prev_season_off_rypg = mean(n_rush_yards_offense, na.rm=TRUE),
            prev_season_off_typg = mean(n_pass_yards_offense + n_rush_yards_offense, na.rm=TRUE),
            prev_season_off_ptdpg = mean(n_pass_td_offense, na.rm=TRUE),
            prev_season_off_rtdpg = mean(n_rush_td_offense, na.rm=TRUE),
            prev_season_off_ttdpg = mean(n_pass_td_offense + n_rush_td_offense, na.rm=TRUE),
            prev_season_off_fdpg = mean(n_first_down_offense, na.rm=TRUE),
            prev_season_off_spg = mean(n_sack_offense, na.rm=TRUE),
            prev_season_off_ipg = mean(n_interception_offense, na.rm=TRUE),
            prev_season_def_ppg = mean(points_allowed, na.rm=TRUE),
            prev_season_def_plays_per_game = mean(n_play_defense, na.rm=TRUE),
            prev_season_def_run_pct = mean(run_pct_defense, na.rm=TRUE),
            prev_season_def_pass_pct = mean(pass_pct_defense, na.rm=TRUE),
            prev_season_def_pypg = mean(n_pass_yards_defense, na.rm=TRUE),
            prev_season_def_rypg = mean(n_rush_yards_defense, na.rm=TRUE),
            prev_season_def_typg = mean(n_pass_yards_defense + n_rush_yards_defense, na.rm=TRUE),
            prev_season_def_ptdpg = mean(n_pass_td_defense, na.rm=TRUE),
            prev_season_def_rtdpg = mean(n_rush_td_defense, na.rm=TRUE),
            prev_season_def_ttdpg = mean(n_pass_td_defense + n_rush_td_defense, na.rm=TRUE),
            prev_season_def_fdpg = mean(n_first_down_defense, na.rm=TRUE),
            prev_season_def_spg = mean(n_sack_defense, na.rm=TRUE),
            prev_season_def_ipg = mean(n_interception_defense, na.rm=TRUE)) %>% 
  mutate(prev_season = as.numeric(season))

################################################################################
# Get Hot Stats
################################################################################

window_size <- 4

rolling_stats <- total_games_stats %>% 
  ungroup(.) %>% 
  arrange(team, season, week) %>% 
  mutate(rolling_win_pct = lag(zoo::rollapplyr(result, width = window_size, FUN = mean, fill = NA, align = "right", na.rm=TRUE)),
         rolling_off_ppg = lag(zoo::rollapplyr(points_scored, width = window_size, FUN = mean, fill = NA, align = "right", na.rm=TRUE)),
         rolling_off_pypg = lag(zoo::rollapplyr(n_pass_yards_offense, width = window_size, FUN = mean, fill = NA, align = "right", na.rm=TRUE)),
         rolling_off_rypg = lag(zoo::rollapplyr(n_rush_yards_offense, width = window_size, FUN = mean, fill = NA, align = "right", na.rm=TRUE)),
         rolling_off_typg = lag(zoo::rollapplyr((n_pass_yards_offense + n_rush_yards_offense), width = window_size, FUN = mean, fill = NA, align = "right", na.rm=TRUE)),
         rolling_off_ptdpg = lag(zoo::rollapplyr(n_pass_td_offense, width = window_size, FUN = mean, fill = NA, align = "right", na.rm=TRUE)),
         rolling_off_rtdpg = lag(zoo::rollapplyr(n_rush_td_offense, width = window_size, FUN = mean, fill = NA, align = "right", na.rm=TRUE)),
         rolling_off_ttdpg = lag(zoo::rollapplyr((n_pass_td_offense + n_rush_td_offense), width = window_size, FUN = mean, fill = NA, align = "right", na.rm=TRUE)),
         rolling_def_ppg = lag(zoo::rollapplyr(points_allowed, width = window_size, FUN = mean, fill = NA, align = "right", na.rm=TRUE)),
         rolling_def_pypg = lag(zoo::rollapplyr(n_pass_yards_defense, width = window_size, FUN = mean, fill = NA, align = "right", na.rm=TRUE)),
         rolling_def_rypg = lag(zoo::rollapplyr(n_rush_yards_defense, width = window_size, FUN = mean, fill = NA, align = "right", na.rm=TRUE)),
         rolling_def_typg = lag(zoo::rollapplyr((n_pass_yards_defense + n_rush_yards_defense), width = window_size, FUN = mean, fill = NA, align = "right", na.rm=TRUE)),
         rolling_def_ptdpg = lag(zoo::rollapplyr(n_pass_td_defense, width = window_size, FUN = mean, fill = NA, align = "right", na.rm=TRUE)),
         rolling_def_rtdpg = lag(zoo::rollapplyr(n_rush_td_defense, width = window_size, FUN = mean, fill = NA, align = "right", na.rm=TRUE)),
         rolling_def_ttdpg = lag(zoo::rollapplyr((n_pass_td_defense + n_rush_td_defense), width = window_size, FUN = mean, fill = NA, align = "right", na.rm=TRUE)))

################################################################################
# Add prev season stats
################################################################################

modeling_game_vars <- rolling_stats %>% 
  mutate(prev_season = as.numeric(season) - 1) %>% 
  left_join(., season_stats, by=c('team', 'prev_season'), suffix=c('', '_prev_season')) %>% 
  dplyr::select(-prev_season, -season_prev_season)

################################################################################
# Limit to seasons with all data
################################################################################

modeling_game_data <- game_data %>% 
  dplyr::select(game_id, game_type, stadium_id, weekday, game_window,
                coach, opposing_coach, qb, opposing_qb, overtime, home_rest,
                away_rest, div_game, roof, temp_conditions, wind_conditions,
                spread_line, favored)

modeling_game_vars_final <- modeling_game_vars %>% 
  filter(season >= 2001) %>% 
  left_join(., modeling_game_data, by='game_id') %>% 
  mutate(qb = ifelse(home_away == 'home', qb, opposing_qb),
         coach = ifelse(home_away == 'home', coach, opposing_coach),
         favored = ifelse(home_away == 'home', favored, !favored)) %>% 
  dplyr::select(game_id, season, week, team, home_away, points_scored, points_allowed,
                qb, coach, favored, result, everything()) %>% 
  arrange(team, season, week) %>% 
  ungroup(.)

# Define Mode function for character columns
Mode_Character <- function(x) {
  non_na_values <- x[!is.na(x)]  # Exclude NA values
  if (length(non_na_values) > 0) {
    table_x <- table(non_na_values)
    names(table_x)[which.max(table_x)]
  } else {
    NA  # Return NA if all values are NA
  }
}

# Define Mode function for numeric columns
Mode_Numeric <- function(x) {
  non_na_values <- x[!is.na(x)]  # Exclude NA values
  if (length(non_na_values) > 0) {
    ux <- unique(non_na_values)
    ux[which.max(tabulate(match(non_na_values, ux)))]
  } else {
    NA  # Return NA if all values are NA
  }
}

# Example data frame: modeling_game_vars_final

modeling_game_vars_final_filled <- modeling_game_vars_final %>%
  group_by(team, season, week) %>%
  mutate(across(where(is.character), ~replace(.x, is.na(.x), Mode_Character(.x))),
         across(where(is.numeric), ~replace(.x, is.na(.x), Mode_Numeric(.x))))

################################################################################
# Add in Player Data
################################################################################

player_data <- data.table::fread("data/processed/games/player_data.csv") %>% 
  mutate(team = ifelse(team == 'STL',
                         'LA',
                         ifelse(team == 'SD',
                                'LAC',
                                ifelse(team == 'OAK',
                                       'LV',
                                       team)))) %>% 
  filter(position == 'QB' & game_type == 'REG' & depth_team == 1) %>% 
  group_by(player, season, week) %>% 
  mutate(max_start_year = max(starting_year)) %>% 
  filter(starting_year == max_start_year) %>%
  dplyr::select(-max_start_year) %>% 
  ungroup(.) %>% 
  rename(qb = player) %>% 
  mutate(season = as.numeric(season),
         week = as.numeric(week)) %>% 
  arrange(team, season, week) %>% 
  distinct() %>% 
  group_by(team, season, week) %>% 
  slice(1) %>% 
  mutate(across(where(is.numeric), ~ifelse(is.nan(.x), NA, .x)),
         across(where(is.character), ~ifelse(is.nan(.x), "", .x))) %>% 
  filter(season >= 2016) %>% 
  ungroup(.) %>% 
  mutate(on_report = ifelse(!is.na(report_status) & report_status != '', 1, 0),
         on_practice_report = ifelse(!is.na(practice_status) & practice_status != '', 1, 0)) %>% 
         #rolling_avg_passer_rating = lag(zoo::rollapplyr(passer_rating, width = window_size, FUN = mean, fill = NA, align = "right", na.rm=TRUE))) %>% 
  mutate(rolling_avg_time_to_throw = lag(zoo::rollapplyr(avg_time_to_throw, width = window_size, FUN = mean, fill = NA, align = "right", na.rm=TRUE)),
         rolling_avg_completed_air_yards = lag(zoo::rollapplyr(avg_completed_air_yards, width = window_size, FUN = mean, fill = NA, align = "right", na.rm=TRUE)),
         rolling_avg_intended_air_yards = lag(zoo::rollapplyr(avg_intended_air_yards, width = window_size, FUN = mean, fill = NA, align = "right", na.rm=TRUE)),
         rolling_avg_air_yards_differential = lag(zoo::rollapplyr(avg_air_yards_differential, width = window_size, FUN = mean, fill = NA, align = "right", na.rm=TRUE)),
         rolling_avg_attempts = lag(zoo::rollapplyr(attempts, width = window_size, FUN = mean, fill = NA, align = "right", na.rm=TRUE)),
         rolling_avg_pass_yards = lag(zoo::rollapplyr(pass_yards, width = window_size, FUN = mean, fill = NA, align = "right", na.rm=TRUE)),
         rolling_avg_pass_touchdowns = lag(zoo::rollapplyr(pass_touchdowns, width = window_size, FUN = mean, fill = NA, align = "right", na.rm=TRUE)),
         rolling_avg_interceptions = lag(zoo::rollapplyr(interceptions, width = window_size, FUN = mean, fill = NA, align = "right", na.rm=TRUE)),
         rolling_avg_passer_rating = lag(zoo::rollapplyr(passer_rating, width = window_size, FUN = mean, fill = NA, align = "right", na.rm=TRUE)),
         rolling_avg_completions = lag(zoo::rollapplyr(completions, width = window_size, FUN = mean, fill = NA, align = "right", na.rm=TRUE)),
         rolling_avg_completion_percentage = lag(zoo::rollapplyr(completion_percentage, width = window_size, FUN = mean, fill = NA, align = "right", na.rm=TRUE)),
         rolling_avg_expected_completion_percentage = lag(zoo::rollapplyr(expected_completion_percentage, width = window_size, FUN = mean, fill = NA, align = "right", na.rm=TRUE)),
         rolling_avg_completion_percentage_above_expectation = lag(zoo::rollapplyr(completion_percentage_above_expectation, width = window_size, FUN = mean, fill = NA, align = "right", na.rm=TRUE)),
         rolling_avg_avg_air_distance = lag(zoo::rollapplyr(avg_air_distance, width = window_size, FUN = mean, fill = NA, align = "right", na.rm=TRUE)),
         rolling_avg_max_air_distance = lag(zoo::rollapplyr(max_air_distance, width = window_size, FUN = mean, fill = NA, align = "right", na.rm=TRUE)),
         rolling_n_on_report = lag(zoo::rollapplyr(on_report, width = window_size, FUN = sum, fill = NA, align = "right", na.rm=TRUE)),
         rolling_n_on_practice_report = lag(zoo::rollapplyr(on_practice_report, width = window_size, FUN = sum, fill = NA, align = "right", na.rm=TRUE))) %>% 
  filter(!is.nan(rolling_avg_passer_rating)) %>% 
  filter(!is.na(rolling_avg_passer_rating))

blah <- player_data %>% 
  group_by(qb, season, week, team) %>% 
  summarize(n = n()) %>% 
  arrange(team, season, week)

modeling_game_vars_final_final <- modeling_game_vars_final %>% 
  mutate(season = as.numeric(season),
         week = as.numeric(week)) %>% 
  filter(!is.na(qb)) %>% 
  inner_join(., player_data, by=c('qb', 'season', 'week', 'team')) %>% 
  mutate(years_left = expiring_year - season) %>% 
  filter(season >= 2016)

blah <- modeling_game_vars_final_final %>% 
  dplyr::select(qb, season, week, team, rolling_avg_passer_rating, passer_rating)

################################################################################
# Save
################################################################################

data.table::fwrite(modeling_game_vars_final, "data/processed/games/modeling_game_data.csv")














