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
# Generalize
################################################################################

seasons <- 2012:2021

wr_stat_lst <- list()
for (s in seasons) {
  
  # Set seasons
  current_season <- s
  next_season <- s + 1
  
  # Set IDs
  current_season_ids <- player_data %>% 
    filter(season == s) %>% 
    pull(player_id) %>% 
    unique()
  
  next_season_ids <- player_data %>% 
    filter(season == s + 1) %>% 
    pull(player_id) %>% 
    unique()
  
  # Get targets
  wr_target_stats <- player_data %>% 
    filter(position == 'WR') %>% 
    filter(player_id %in% next_season_ids) %>% 
    filter(season == next_season) %>% 
    group_by(player_id, player_display_name) %>% 
    summarize(target_carries = sum(carries, na.rm=TRUE),
              target_rushing_yd = sum(rushing_yards, na.rm=TRUE),
              target_rushing_td = sum(rushing_tds, na.rm=TRUE),
              target_rushing_fb_lst = sum(rushing_fumbles_lost, na.rm=TRUE),
              target_receptions = sum(receptions, na.rm=TRUE),
              target_receiving_yd = sum(receiving_yards, na.rm=TRUE),
              target_receiving_td = sum(receiving_tds, na.rm=TRUE),
              target_receiving_fb_lst = sum(receiving_fumbles_lost, na.rm=TRUE)) %>% 
    ungroup(.) %>% 
    mutate(target_season = next_season)
  
  # Get career stats up to target season
  wr_cumulative_career_stats <- player_data %>% 
    filter(position == 'WR') %>% 
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
              career_receptions = sum(receptions, na.rm=TRUE),
              career_targets = sum(targets, na.rm=TRUE),
              career_target_share = mean(target_share, na.rm=TRUE),
              career_catch_rate = sum(receptions, na.rm=TRUE) / sum(targets, na.rm=TRUE),
              career_receptions_pg = sum(receptions, na.rm=TRUE) / n(),
              career_receiving_yd = sum(receiving_yards, na.rm=TRUE),
              career_receiving_ypg = sum(receiving_yards, na.rm=TRUE) / n(),
              career_receiving_td = sum(receiving_tds, na.rm=TRUE),
              career_receiving_td_pg = sum(receiving_tds, na.rm=TRUE) / n(),
              career_receiving_fd = sum(receiving_first_downs, na.rm=TRUE),
              career_receiving_fd_pg = sum(receiving_first_downs, na.rm=TRUE) / n(),
              career_receiving_epa = sum(receiving_epa, na.rm=TRUE),
              career_receiving_epa_pg = sum(receiving_epa, na.rm=TRUE) / n()) %>% 
    ungroup(.)
  
  # Get stats of previous season
  wr_season_stats <- player_data %>% 
    filter(position == 'WR') %>% 
    filter(player_id %in% next_season_ids) %>% 
    filter(season == current_season) %>% 
    group_by(player_id, player_display_name) %>% 
    summarize(prev_season_games_played = n(),
              prev_season_carries = sum(carries, na.rm=TRUE),
              prev_season_carries_pg = sum(carries, na.rm=TRUE) / n(),
              prev_season_rushing_yd = sum(rushing_yards, na.rm=TRUE),
              prev_season_rushing_ypg = sum(rushing_yards, na.rm=TRUE) / n(),
              prev_season_rushing_td = sum(rushing_tds, na.rm=TRUE),
              prev_season_rushing_td_pg = sum(rushing_tds, na.rm=TRUE) / n(),
              prev_season_rushing_fb = sum(rushing_fumbles, na.rm=TRUE),
              prev_season_rushing_fb_pg = sum(rushing_fumbles, na.rm=TRUE) / n(),
              prev_season_rushing_fd = sum(rushing_first_downs, na.rm=TRUE),
              prev_season_rushing_fd_pg = sum(rushing_first_downs, na.rm=TRUE) / n(),
              prev_season_rushing_epa = sum(rushing_epa, na.rm=TRUE),
              prev_season_rushing_epa_pg = sum(rushing_epa, na.rm=TRUE) / n(),
              prev_season_receptions = sum(receptions, na.rm=TRUE),
              prev_season_targets = sum(targets, na.rm=TRUE),
              prev_season_target_share = mean(target_share, na.rm=TRUE),
              prev_season_catch_rate = sum(receptions, na.rm=TRUE) / sum(targets, na.rm=TRUE),
              prev_season_receptions_pg = sum(receptions, na.rm=TRUE) / n(),
              prev_season_receiving_yd = sum(receiving_yards, na.rm=TRUE),
              prev_season_receiving_ypg = sum(receiving_yards, na.rm=TRUE) / n(),
              prev_season_receiving_td = sum(receiving_tds, na.rm=TRUE),
              prev_season_receiving_td_pg = sum(receiving_tds, na.rm=TRUE) / n(),
              prev_season_receiving_fd = sum(receiving_first_downs, na.rm=TRUE),
              prev_season_receiving_fd_pg = sum(receiving_first_downs, na.rm=TRUE) / n(),
              prev_season_receiving_epa = sum(receiving_epa, na.rm=TRUE),
              prev_season_receiving_epa_pg = sum(receiving_epa, na.rm=TRUE) / n()) %>% 
    ungroup(.)
  
  # Put it all together
  wr_output <- wr_cumulative_career_stats %>% 
    inner_join(., wr_season_stats, by=c('player_id', 'player_display_name')) %>% 
    inner_join(., wr_target_stats, by=c('player_id', 'player_display_name')) %>% 
    dplyr::select(player_id, player_display_name, target_season, everything())
  
  # add to list
  if (length(wr_stat_lst) == 0) {
    wr_stat_lst <- list(wr_output)
  } else {
    wr_stat_lst <- c(wr_stat_lst, list(wr_output))
  }
}

################################################################################
# Combine Results
################################################################################
season_training_data <- do.call(rbind, wr_stat_lst)

################################################################################
# Save Results
################################################################################
saveRDS(wr_output, "data/processed/season/wr_season_stat_modeling_data.rds")

