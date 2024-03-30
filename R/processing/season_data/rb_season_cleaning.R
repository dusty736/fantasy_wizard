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
                              stringsAsFactors=FALSE) %>% 
  rename(team = recent_team)

################################################################################
# Restrict to players with one team per season
################################################################################

player_dictionary <- player_data %>% 
  dplyr::select(player_display_name, season, team) %>% 
  distinct() %>% 
  group_by(player_display_name, season) %>% 
  summarise(n_team = n_distinct(team)) %>% 
  filter(n_team == 1) %>% 
  mutate(name_filter = paste0(player_display_name, season))

player_data <- player_data %>% 
  mutate(name_filter = paste0(player_display_name, season)) %>% 
  inner_join(., player_dictionary, by=c('name_filter', 'player_display_name', 'season'))

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

rb_stat_lst <- list()
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
  rb_target_stats <- player_data %>% 
    filter(position == 'RB') %>% 
    filter(player_id %in% next_season_ids) %>% 
    filter(season == next_season) %>% 
    group_by(team, player_id, player_display_name) %>% 
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
  rb_cumulative_career_stats <- player_data %>% 
    filter(position == 'RB') %>% 
    filter(player_id %in% next_season_ids) %>% 
    filter(season <= current_season) %>% 
    group_by(team, player_id, player_display_name) %>% 
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
  rb_season_stats <- player_data %>% 
    filter(position == 'RB') %>% 
    filter(player_id %in% current_season_ids & player_id %in% next_season_ids) %>% 
    filter(season == current_season) %>% 
    group_by(team, player_id, player_display_name) %>% 
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
  rb_output <- rb_cumulative_career_stats %>% 
    inner_join(., rb_season_stats, by=c('player_id', 'player_display_name', 'team')) %>% 
    inner_join(., rb_target_stats, by=c('player_id', 'player_display_name', 'team')) %>% 
    dplyr::select(team, player_id, player_display_name, target_season, everything())
  
  # add to list
  if (length(rb_stat_lst) == 0) {
    rb_stat_lst <- list(rb_output)
  } else {
    rb_stat_lst <- c(rb_stat_lst, list(rb_output))
  }
}

################################################################################
# Combine Results
################################################################################
season_training_data <- do.call(rbind, rb_stat_lst)

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
rb_cumulative_career_stats <- player_data %>% 
  filter(position == 'RB') %>% 
  filter(player_id %in% season_ids) %>% 
  filter(season <= 2022) %>% 
  group_by(team, player_id, player_display_name) %>% 
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
rb_season_stats <- player_data %>% 
  filter(position == 'RB') %>% 
  filter(player_id %in% season_ids) %>% 
  filter(season == 2022) %>% 
  group_by(team, player_id, player_display_name) %>% 
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
rb_external_data <- rb_cumulative_career_stats %>% 
  inner_join(., rb_season_stats, by=c('team', 'player_id', 'player_display_name')) %>% 
  mutate(target_season = 2023) %>% 
  dplyr::select(team, player_id, player_display_name, target_season, everything())

################################################################################
# Add in roster data
################################################################################

roster_data <- data.table::fread("data/processed/season/roster_stats.csv") %>% 
  rename(player_display_name = full_name) %>% 
  mutate(draft_number = ifelse(is.na(draft_number), 
                               999,
                               draft_number)) %>% 
  mutate(draft_round = ifelse(draft_number <= 32,
                              'r1',
                              ifelse(draft_number >= 33 & draft_number <= 64,
                                     'r2',
                                     ifelse(draft_number >= 65 & draft_number <= 96,
                                            'r3',
                                            ifelse(draft_number >= 97 & draft_number <= 128,
                                                   'r4',
                                                   ifelse(draft_number >= 129 & draft_number <= 160,
                                                          'r5',
                                                          ifelse(draft_number >= 161 & draft_number <= 192,
                                                                 'r6',
                                                                 ifelse(draft_number >= 193 & draft_number <= 300,
                                                                        'r3',
                                                                        'undrafted')))))))) %>% 
  dplyr::select(-draft_number) %>% 
  rename(target_season = season)
  
# Add it in
season_training_data <- season_training_data %>% 
  left_join(., roster_data, by=c('target_season', 'player_display_name'))
rb_external_data <- rb_external_data %>% 
  left_join(., roster_data, by=c('target_season', 'player_display_name'))

################################################################################
# Add in coach data
################################################################################

coaches <- data.table::fread("data/processed/coaches/processed_coaching_data.csv")
coaching_dictionary <- data.table::fread("data/processed/coaches/coaching_dictionary.csv") %>% 
  filter(week_start == 1) %>% 
  mutate()

# Add it in
season_training_data <- season_training_data %>% 
  left_join(., coaching_dictionary, by=c("team")) %>% 
  filter(target_season >= stint_start & target_season <= stint_end) %>% 
  left_join(., coaches, by=c('coach'))

coaches_2023 <- coaching_dictionary %>% 
  filter(stint_end == 2022) %>% 
  mutate(coach = ifelse(team == 'DEN',
                        'Sean Payton',
                        coach),
         coach = ifelse(team == 'HOU',
                        'DeMeco Ryans',
                        coach),
         coach = ifelse(team == 'IND',
                        'Shane Steichen',
                        coach),
         coach = ifelse(team == 'ARI',
                        'Jonathan Gannon',
                        coach),
         coach = ifelse(team == 'CAR',
                        'Frank Reich',
                        coach))

rb_external_data <- rb_external_data %>% 
  left_join(., coaches_2023, by=c("team")) %>% 
  left_join(., coaches, by=c('coach'))

################################################################################
# Add in strength of schedule
################################################################################

pwr_rnk <- data.table::fread("data/processed/season/season_rankings.csv")

prev_season_off_rnk <- pwr_rnk %>% 
  mutate(target_season = season + 1) %>% 
  dplyr::select(target_season, team, off_rush_ypg_rank, off_rush_td_rank, 
                off_pass_ypg_rank, off_pass_td_rank)

prev_season_def_rnk <- pwr_rnk %>% 
  mutate(target_season = season + 1) %>% 
  dplyr::select(target_season, team, def_rush_ypg_rank, def_rush_td_rank, 
                def_pass_ypg_rank, def_pass_td_rank)

season_training_data <- season_training_data %>% 
  left_join(., prev_season_off_rnk, by=c('target_season', 'team')) %>% 
  left_join(., prev_season_def_rnk)

rb_external_data <- rb_external_data %>% 
  left_join(., prev_season_off_rnk, by=c('target_season', 'team')) %>% 
  left_join(., prev_season_def_rnk)

################################################################################
# Save Results
################################################################################
data.table::fwrite(season_training_data, "data/processed/season/rb_season_stat_modeling_data.csv")
data.table::fwrite(rb_external_data, "data/processed/season/rb_season_external.csv")

