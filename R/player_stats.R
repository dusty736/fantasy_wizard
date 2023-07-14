################################################################################
# Load Libraries
################################################################################

require(tidyverse)
require(lubridate)

################################################################################
# Create File Path to Data Folder
################################################################################

if (!file.exists(file.path("data", "processed", "players"))) {
  dir.create(file.path("data", "processed", "players"))
}


################################################################################
# Load Data
################################################################################

player_data <- data.table::fread("data/raw/player_stats/raw_player_stats.csv",
                              stringsAsFactors=FALSE)

################################################################################
# Get Players from 2021/2022 Season
################################################################################

player_ids_2021 <- player_data %>% 
  filter(season == 2021) %>% 
  pull(player_id) %>% 
  unique()

player_ids_2022 <- player_data %>% 
  filter(season == 2022) %>% 
  pull(player_id) %>% 
  unique()

################################################################################
# Create Career Stats
################################################################################

rb_career_stats <- player_data %>% 
  filter(position == 'RB') %>% 
  filter(player_id %in% player_ids_2022) %>% 
  group_by(player_id, player_display_name) %>% 
  summarize(games_played = n(),
            seasons_played = n_distinct(season),
            career_carries = sum(carries, na.rm=TRUE),
            career_carries_pg = sum(carries, na.rm=TRUE) / n(),
            career_rushing_yd = sum(rushing_yards, na.rm=TRUE),
            career_rushing_ypg = sum(rushing_yards, na.rm=TRUE) / n(),
            career_rushing_td = sum(rushing_tds, na.rm=TRUE),
            career_rushing_td_pg = sum(rushing_tds, na.rm=TRUE) / n(),
            career_rushing_fb = sum(rushing_fumbles, na.rm=TRUE),
            career_rushing_fb_pg = sum(rushing_fumbles, na.rm=TRUE) / n(),
            career_rushing_fd = sum(rushing_first_downs, na.rm=TRUE),
            career_rushing_fd = sum(rushing_first_downs, na.rm=TRUE) / n(),
            career_rushing_epa = sum(rushing_epa, na.rm=TRUE),
            career_rushing_epa_pg = sum(rushing_epa, na.rm=TRUE) / n()) %>% 
  ungroup(.)

rb_2021_stats <- player_data %>% 
  filter(position == 'RB') %>% 
  filter(player_id %in% c(player_ids_2022, player_ids_2021)) %>% 
  filter(season == 2021) %>% 
  group_by(player_id, player_display_name) %>% 
  summarize(games_played = n(),
            prev_season_carries = sum(carries, na.rm=TRUE),
            prev_season_carries_pg = sum(carries, na.rm=TRUE) / n(),
            prev_season_rushing_yd = sum(rushing_yards, na.rm=TRUE),
            prev_season_rushing_ypg = sum(rushing_yards, na.rm=TRUE) / n(),
            prev_season_rushing_td = sum(rushing_tds, na.rm=TRUE),
            prev_season_rushing_td_pg = sum(rushing_tds, na.rm=TRUE) / n(),
            prev_season_rushing_fb = sum(rushing_fumbles, na.rm=TRUE),
            prev_season_rushing_fb_pg = sum(rushing_fumbles, na.rm=TRUE) / n(),
            prev_season_rushing_fd = sum(rushing_first_downs, na.rm=TRUE),
            prev_season_rushing_fd = sum(rushing_first_downs, na.rm=TRUE) / n(),
            prev_season_rushing_epa = sum(rushing_epa, na.rm=TRUE),
            prev_season_rushing_epa_pg = sum(rushing_epa, na.rm=TRUE) / n()) %>% 
  ungroup(.)

################################################################################
# Create Single Dataset
################################################################################

rb_output <- rb_career_stats %>% 
  left_join(., rb_2021_stats, by=c('player_id', 'player_display_name'))

################################################################################
# Save Dataset
################################################################################

saveRDS(rb_output, "data/processed/players/rb_2022_training.rds")


