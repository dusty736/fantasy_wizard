################################################################################
# Load Libraries
################################################################################

require(tidyverse)
require(lubridate)

################################################################################
# Create File Path to Data Folder
################################################################################

if (!file.exists(file.path("data", "processed", "season"))) {
  dir.create(file.path("data", "processed", "season"))
}

################################################################################
# Load Data
################################################################################

player_data <- data.table::fread("data/raw/player_stats/raw_player_stats.csv",
                                 stringsAsFactors=FALSE)

################################################################################
# Calculate Games Per Season Per Player
################################################################################

games_season_count <- player_data %>% 
  group_by(player_id, season) %>% 
  summarize(n_games = n()) %>% 
  ungroup(.) %>% 
  group_by(player_id) %>% 
  summarize(n_games_per_season = round(mean(n_games))) %>% 
  ungroup(.)

################################################################################
# Limit Data
################################################################################

player_data <- player_data %>% 
  filter(season_type == 'REG')

################################################################################
# Generalize
################################################################################

seasons <- 2012:2021

qb_stat_lst <- list()
for (s in seasons) {
  print(paste("Season: ", s))
  
  # Set seasons
  current_season <- s
  next_season <- s + 1
  
  # Get game count
  games_per_season <- ifelse(current_season < 2021, 16, 17)
  
  # Set IDs
  current_season_ids <- player_data %>% 
    filter(season == s) %>% 
    group_by(player_id) %>% 
    summarize(n = n(),
              n_yards = sum(rushing_yards, na.rm=TRUE),
              n_att = sum(attempts)) %>% 
    ungroup(.) %>% 
    # left_join(., games_season_count, by='player_id') %>% 
    #filter(n >= games_per_season - 4) %>% 
    #filter(n_att >= 100) %>% 
    pull(player_id) %>% 
    unique()
  
  next_season_ids <- player_data %>% 
    filter(season == s + 1) %>% 
    group_by(player_id) %>% 
    summarize(n = n(),
              n_yards = sum(rushing_yards, na.rm=TRUE),
              n_carries = sum(carries)) %>% 
    ungroup(.) %>% 
    # left_join(., games_season_count, by='player_id') %>% 
    filter(n >= games_per_season - 4) %>% 
    #filter(n_carries >= 100) %>% 
    pull(player_id) %>% 
    unique()
  
  # Get targets
  qb_target_stats <- player_data %>% 
    filter(position == 'QB') %>% 
    filter(player_id %in% next_season_ids) %>% 
    filter(season == next_season) %>% 
    group_by(player_id, player_display_name) %>% 
    summarize(target_carries = sum(carries, na.rm=TRUE),
              target_rushing_yd = sum(rushing_yards, na.rm=TRUE),
              target_rushing_td = sum(rushing_tds, na.rm=TRUE),
              target_rushing_fbl = sum(rushing_fumbles, na.rm=TRUE),
              target_completions = sum(completions, na.rm=TRUE),
              target_passing_yd = sum(passing_yards, na.rm=TRUE),
              target_passing_td = sum(passing_tds, na.rm=TRUE),
              target_interceptions = sum(interceptions, na.rm=TRUE),
              target_sack_yards = sum(sack_yards, na.rm=TRUE),
              target_sack_fbl = sum(sack_fumbles, na.rm=TRUE)) %>% 
    ungroup(.) %>% 
    mutate(target_season = next_season)
  
  # Get career stats up to target season
  qb_cumulative_career_stats <- player_data %>% 
    filter(position == 'QB') %>% 
    filter(player_id %in% next_season_ids) %>% 
    filter(season <= current_season) %>% 
    group_by(player_id, player_display_name) %>% 
    summarize(games_played = n(),
              seasons_played = n_distinct(season),
              career_carries = sum(carries, na.rm=TRUE),
              career_carries_pg = sum(carries, na.rm=TRUE) / n(),
              career_rushing_yd = sum(rushing_yards, na.rm=TRUE),
              career_rushing_ypg = sum(rushing_yards, na.rm=TRUE) / n(),
              career_rushing_td = sum(rushing_tds, na.rm=TRUE),
              career_rushing_td_pg = sum(rushing_tds, na.rm=TRUE) / n(),
              career_rushing_fd = sum(rushing_first_downs, na.rm=TRUE),
              career_rushing_fd_pg = sum(rushing_first_downs, na.rm=TRUE) / n(),
              career_rushing_epa = sum(rushing_epa, na.rm=TRUE),
              career_rushing_epa_pg = sum(rushing_epa, na.rm=TRUE) / n(),
              career_completions = sum(completions, na.rm=TRUE),
              career_attempts = sum(attempts, na.rm=TRUE),
              career_completion_pct = sum(completions, na.rm=TRUE) / sum(attempts, na.rm=TRUE),
              career_passing_yd = sum(passing_yards, na.rm=TRUE),
              career_passing_ypg = sum(passing_yards, na.rm=TRUE) / n(),
              career_passing_yd_ac = sum(passing_yards_after_catch),
              career_passing_yd_ac_pg = sum(passing_yards_after_catch) / n(),
              career_passing_int = sum(interceptions, na.rm=TRUE),
              career_passing_int_pg = sum(interceptions, na.rm=TRUE) / n(),
              career_passing_td = sum(passing_tds, na.rm=TRUE),
              career_passing_td_pg = sum(passing_tds, na.rm=TRUE) / n(),
              career_sacks_pg = sum(sacks, na.rm=TRUE) / n(),
              career_sack_ypg = sum(sack_yards, na.rm=TRUE) / n(),
              career_sack_fbl_pg = sum(sack_fumbles, na.rm=TRUE) / n(),
              career_passing_epa_pg = sum(passing_epa, na.rm=TRUE) / n()) %>% 
    ungroup(.)
  
  # Get stats of previous season
  qb_season_stats <- player_data %>% 
    filter(position == 'QB') %>% 
    filter(player_id %in% current_season_ids & player_id %in% next_season_ids) %>% 
    filter(season == current_season) %>% 
    group_by(player_id, player_display_name) %>% 
    summarize(prev_season_carries = sum(carries, na.rm=TRUE),
              prev_season_carries_pg = sum(carries, na.rm=TRUE) / n(),
              prev_season_rushing_yd = sum(rushing_yards, na.rm=TRUE),
              prev_season_rushing_ypg = sum(rushing_yards, na.rm=TRUE) / n(),
              prev_season_rushing_td = sum(rushing_tds, na.rm=TRUE),
              prev_season_rushing_td_pg = sum(rushing_tds, na.rm=TRUE) / n(),
              prev_season_rushing_fd = sum(rushing_first_downs, na.rm=TRUE),
              prev_season_rushing_fd_pg = sum(rushing_first_downs, na.rm=TRUE) / n(),
              prev_season_rushing_epa = sum(rushing_epa, na.rm=TRUE),
              prev_season_rushing_epa_pg = sum(rushing_epa, na.rm=TRUE) / n(),
              prev_season_completions = sum(completions, na.rm=TRUE),
              prev_season_attempts = sum(attempts, na.rm=TRUE),
              prev_season_completion_pct = sum(completions, na.rm=TRUE) / sum(attempts, na.rm=TRUE),
              prev_season_passing_yd = sum(passing_yards, na.rm=TRUE),
              prev_season_passing_ypg = sum(passing_yards, na.rm=TRUE) / n(),
              prev_season_passing_yd_ac = sum(passing_yards_after_catch),
              prev_season_passing_yd_ac_pg = sum(passing_yards_after_catch) / n(),
              prev_season_passing_int = sum(interceptions, na.rm=TRUE),
              prev_season_passing_int_pg = sum(interceptions, na.rm=TRUE) / n(),
              prev_season_passing_td = sum(passing_tds, na.rm=TRUE),
              prev_season_passing_td_pg = sum(passing_tds, na.rm=TRUE) / n(),
              prev_season_sacks_pg = sum(sacks, na.rm=TRUE) / n(),
              prev_season_sack_ypg = sum(sack_yards, na.rm=TRUE) / n(),
              prev_season_sack_fbl_pg = sum(sack_fumbles, na.rm=TRUE) / n(),
              prev_season_passing_epa_pg = sum(passing_epa, na.rm=TRUE) / n()) %>% 
    ungroup(.)
  
  # Put it all together
  qb_output <- qb_cumulative_career_stats %>% 
    inner_join(., qb_season_stats, by=c('player_id', 'player_display_name')) %>% 
    inner_join(., qb_target_stats, by=c('player_id', 'player_display_name')) %>% 
    dplyr::select(player_id, player_display_name, target_season, everything())
  
  # add to list
  if (length(qb_stat_lst) == 0) {
    qb_stat_lst <- list(qb_output)
  } else {
    qb_stat_lst <- c(qb_stat_lst, list(qb_output))
  }
}

################################################################################
# Combine Results
################################################################################
season_training_data <- do.call(rbind, qb_stat_lst)

################################################################################
# Create 2022 Set
################################################################################

season_ids <- player_data %>% 
  filter(season == 2022) %>% 
  # group_by(player_id) %>% 
  # summarize(n = n()) %>% 
  # ungroup(.) %>% 
  # filter(n >= 13) %>% 
  pull(player_id) %>% 
  unique()

# Get career stats up to target season
qb_cumulative_career_stats <- player_data %>% 
  filter(position == 'QB') %>% 
  filter(player_id %in% season_ids) %>% 
  filter(season <= 2022) %>% 
  group_by(player_id, player_display_name) %>% 
  summarize(games_played = n(),
            seasons_played = n_distinct(season),
            career_carries = sum(carries, na.rm=TRUE),
            career_carries_pg = sum(carries, na.rm=TRUE) / n(),
            career_rushing_yd = sum(rushing_yards, na.rm=TRUE),
            career_rushing_ypg = sum(rushing_yards, na.rm=TRUE) / n(),
            career_rushing_td = sum(rushing_tds, na.rm=TRUE),
            career_rushing_td_pg = sum(rushing_tds, na.rm=TRUE) / n(),
            career_rushing_fd = sum(rushing_first_downs, na.rm=TRUE),
            career_rushing_fd_pg = sum(rushing_first_downs, na.rm=TRUE) / n(),
            career_rushing_epa = sum(rushing_epa, na.rm=TRUE),
            career_rushing_epa_pg = sum(rushing_epa, na.rm=TRUE) / n(),
            career_completions = sum(completions, na.rm=TRUE),
            career_attempts = sum(attempts, na.rm=TRUE),
            career_completion_pct = sum(completions, na.rm=TRUE) / sum(attempts, na.rm=TRUE),
            career_passing_yd = sum(passing_yards, na.rm=TRUE),
            career_passing_ypg = sum(passing_yards, na.rm=TRUE) / n(),
            career_passing_yd_ac = sum(passing_yards_after_catch),
            career_passing_yd_ac_pg = sum(passing_yards_after_catch) / n(),
            career_passing_int = sum(interceptions, na.rm=TRUE),
            career_passing_int_pg = sum(interceptions, na.rm=TRUE) / n(),
            career_passing_td = sum(passing_tds, na.rm=TRUE),
            career_passing_td_pg = sum(passing_tds, na.rm=TRUE) / n(),
            career_sacks_pg = sum(sacks, na.rm=TRUE) / n(),
            career_sack_ypg = sum(sack_yards, na.rm=TRUE) / n(),
            career_sack_fbl_pg = sum(sack_fumbles, na.rm=TRUE) / n(),
            career_passing_epa_pg = sum(passing_epa, na.rm=TRUE) / n()) %>% 
  ungroup(.)

# Get stats of previous season
qb_season_stats <- player_data %>% 
  filter(position == 'QB') %>% 
  filter(player_id %in% season_ids) %>% 
  filter(season == 2022) %>% 
  group_by(player_id, player_display_name) %>% 
  summarize(prev_season_carries = sum(carries, na.rm=TRUE),
            prev_season_carries_pg = sum(carries, na.rm=TRUE) / n(),
            prev_season_rushing_yd = sum(rushing_yards, na.rm=TRUE),
            prev_season_rushing_ypg = sum(rushing_yards, na.rm=TRUE) / n(),
            prev_season_rushing_td = sum(rushing_tds, na.rm=TRUE),
            prev_season_rushing_td_pg = sum(rushing_tds, na.rm=TRUE) / n(),
            prev_season_rushing_fd = sum(rushing_first_downs, na.rm=TRUE),
            prev_season_rushing_fd_pg = sum(rushing_first_downs, na.rm=TRUE) / n(),
            prev_season_rushing_epa = sum(rushing_epa, na.rm=TRUE),
            prev_season_rushing_epa_pg = sum(rushing_epa, na.rm=TRUE) / n(),
            prev_season_completions = sum(completions, na.rm=TRUE),
            prev_season_attempts = sum(attempts, na.rm=TRUE),
            prev_season_completion_pct = sum(completions, na.rm=TRUE) / sum(attempts, na.rm=TRUE),
            prev_season_passing_yd = sum(passing_yards, na.rm=TRUE),
            prev_season_passing_ypg = sum(passing_yards, na.rm=TRUE) / n(),
            prev_season_passing_yd_ac = sum(passing_yards_after_catch),
            prev_season_passing_yd_ac_pg = sum(passing_yards_after_catch) / n(),
            prev_season_passing_int = sum(interceptions, na.rm=TRUE),
            prev_season_passing_int_pg = sum(interceptions, na.rm=TRUE) / n(),
            prev_season_passing_td = sum(passing_tds, na.rm=TRUE),
            prev_season_passing_td_pg = sum(passing_tds, na.rm=TRUE) / n(),
            prev_season_sacks_pg = sum(sacks, na.rm=TRUE) / n(),
            prev_season_sack_ypg = sum(sack_yards, na.rm=TRUE) / n(),
            prev_season_sack_fbl_pg = sum(sack_fumbles, na.rm=TRUE) / n(),
            prev_season_passing_epa_pg = sum(passing_epa, na.rm=TRUE) / n()) %>% 
  ungroup(.)

# Put it all together
qb_external_data <- qb_cumulative_career_stats %>% 
  inner_join(., qb_season_stats, by=c('player_id', 'player_display_name')) %>% 
  mutate(target_season = 2023) %>% 
  dplyr::select(player_id, player_display_name, target_season, everything())

################################################################################
# Save Results
################################################################################
data.table::fwrite(season_training_data, "data/processed/season/qb_season_stat_modeling_data.csv")
data.table::fwrite(qb_external_data, "data/processed/season/qb_season_external.csv")
