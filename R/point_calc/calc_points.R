################################################################################
# Load Libraries
################################################################################

library(tidyverse)
library(lubridate)
library(caret)
library(boot)
options(dplyr.summarise.inform = FALSE)

################################################################################
# Load Data
################################################################################

# RB
rb_carries <- data.table::fread("data/results/RB/exp_r_carries_2023.csv")
rb_rec_td <- data.table::fread("data/results/RB/exp_r_receiving_td_2023.csv")
rb_rec_yd <- data.table::fread("data/results/RB/exp_r_receiving_yd_2023.csv")
rb_rec_tot <- data.table::fread("data/results/RB/exp_r_receptions_2023.csv")
rb_rush_td <- data.table::fread("data/results/RB/exp_r_td_2023.csv")
rb_rush_yd <- data.table::fread("data/results/RB/exp_r_yrd_2023.csv")

# WR
wr_carries <- data.table::fread("data/results/WR/exp_w_carries_2023.csv")
wr_rec_td <- data.table::fread("data/results/WR/exp_w_receiving_td_2023.csv")
wr_rec_yd <- data.table::fread("data/results/WR/exp_w_receiving_yd_2023.csv")
wr_rec_tot <- data.table::fread("data/results/WR/exp_w_receptions_2023.csv")
wr_rush_td <- data.table::fread("data/results/WR/exp_w_td_2023.csv")
wr_rush_yd <- data.table::fread("data/results/WR/exp_w_yrd_2023.csv")

#TE
te_rec_td <- data.table::fread("data/results/TE/exp_t_receiving_td_2023.csv")
te_rec_yd <- data.table::fread("data/results/TE/exp_t_receiving_yd_2023.csv")
te_rec_tot <- data.table::fread("data/results/TE/exp_t_receptions_2023.csv")

################################################################################
# Combine
################################################################################

# Pivot Wide
# RB
rb_carries <- rb_carries %>% 
  pivot_wider(., names_from = var, values_from = c(mean, ci_l, ci_u))
rb_rec_td <- rb_rec_td %>% 
  pivot_wider(., names_from = var, values_from = c(mean, ci_l, ci_u))
rb_rec_yd <- rb_rec_yd %>% 
  pivot_wider(., names_from = var, values_from = c(mean, ci_l, ci_u))
rb_rec_tot <- rb_rec_tot %>% 
  pivot_wider(., names_from = var, values_from = c(mean, ci_l, ci_u))
rb_rush_td <- rb_rush_td %>% 
  pivot_wider(., names_from = var, values_from = c(mean, ci_l, ci_u))
rb_rush_yd <- rb_rush_yd %>% 
  pivot_wider(., names_from = var, values_from = c(mean, ci_l, ci_u))

# WR
wr_carries <- wr_carries %>% 
  pivot_wider(., names_from = var, values_from = c(mean, ci_l, ci_u))
wr_rec_td <- wr_rec_td %>% 
  pivot_wider(., names_from = var, values_from = c(mean, ci_l, ci_u))
wr_rec_yd <- wr_rec_yd %>% 
  pivot_wider(., names_from = var, values_from = c(mean, ci_l, ci_u))
wr_rec_tot <- wr_rec_tot %>% 
  pivot_wider(., names_from = var, values_from = c(mean, ci_l, ci_u))
wr_rush_td <- wr_rush_td %>% 
  pivot_wider(., names_from = var, values_from = c(mean, ci_l, ci_u))
wr_rush_yd <- wr_rush_yd %>% 
  pivot_wider(., names_from = var, values_from = c(mean, ci_l, ci_u))

# TE
te_rec_td <- te_rec_td %>% 
  pivot_wider(., names_from = var, values_from = c(mean, ci_l, ci_u))
te_rec_yd <- te_rec_yd %>% 
  pivot_wider(., names_from = var, values_from = c(mean, ci_l, ci_u))
te_rec_tot <- te_rec_tot %>% 
  pivot_wider(., names_from = var, values_from = c(mean, ci_l, ci_u))

# Combine
rb_df <- rb_carries %>% 
  left_join(., rb_rush_yd, by=c('player_id', 'player_display_name')) %>% 
  left_join(., rb_rush_td, by=c('player_id', 'player_display_name')) %>% 
  left_join(., rb_rec_tot, by=c('player_id', 'player_display_name')) %>% 
  left_join(., rb_rec_yd, by=c('player_id', 'player_display_name')) %>% 
  left_join(., rb_rec_td, by=c('player_id', 'player_display_name')) %>% 
  mutate(position = 'RB') %>% 
  mutate(ff_points = 0.1 * (mean_target_rushing_yd + mean_target_receiving_yd) + 6 * (mean_target_rushing_td + mean_target_receiving_td) + 0.5 * mean_target_receptions,
         ff_lower = 0.1 * (ci_l_target_rushing_yd + ci_l_target_receiving_yd) + 6 * (ci_l_target_rushing_td + ci_l_target_receiving_td) + 0.5 * ci_l_target_receptions,
         ff_upper = 0.1 * (ci_u_target_rushing_yd + ci_u_target_receiving_yd) + 6 * (ci_u_target_rushing_td + ci_u_target_receiving_td) + 0.5 * ci_u_target_receptions) %>% 
  dplyr::select(player_id, player_display_name, position, ff_points, ff_lower, ff_upper, everything())

wr_df <- wr_carries %>% 
  left_join(., wr_rush_yd, by=c('player_id', 'player_display_name')) %>% 
  left_join(., wr_rush_td, by=c('player_id', 'player_display_name')) %>% 
  left_join(., wr_rec_tot, by=c('player_id', 'player_display_name')) %>% 
  left_join(., wr_rec_yd, by=c('player_id', 'player_display_name')) %>% 
  left_join(., wr_rec_td, by=c('player_id', 'player_display_name')) %>% 
  mutate(position = 'WR') %>% 
  mutate(ff_points = 0.1 * (mean_target_rushing_yd + mean_target_receiving_yd) + 6 * (mean_target_rushing_td + mean_target_receiving_td) + 0.5 * mean_target_receptions,
         ff_lower = 0.1 * (ci_l_target_rushing_yd + ci_l_target_receiving_yd) + 6 * (ci_l_target_rushing_td + ci_l_target_receiving_td) + 0.5 * ci_l_target_receptions,
         ff_upper = 0.1 * (ci_u_target_rushing_yd + ci_u_target_receiving_yd) + 6 * (ci_u_target_rushing_td + ci_u_target_receiving_td) + 0.5 * ci_u_target_receptions) %>% 
  dplyr::select(player_id, player_display_name, position, ff_points, ff_lower, ff_upper, everything())
  

te_df <- te_rec_tot %>%
  left_join(., te_rec_yd, by=c('player_id', 'player_display_name')) %>% 
  left_join(., te_rec_td, by=c('player_id', 'player_display_name')) %>% 
  mutate(position = 'TE') %>% 
  mutate(ff_points = 0.1 * (mean_target_receiving_yd) + 6 * (mean_target_receiving_td) + 0.5 * mean_target_receptions,
         ff_lower = 0.1 * (ci_l_target_receiving_yd) + 6 * (ci_l_target_receiving_td) + 0.5 * ci_l_target_receptions,
         ff_upper = 0.1 * (ci_u_target_receiving_yd) + 6 * (ci_u_target_receiving_td) + 0.5 * ci_u_target_receptions) %>% 
  dplyr::select(player_id, player_display_name, position, ff_points, ff_lower, ff_upper, everything())

total_df <- plyr::rbind.fill(rb_df, wr_df, te_df)

################################################################################
# Save
################################################################################

data.table::fwrite(wr_df, "data/results/WR/ff_rankings_2023.csv")
data.table::fwrite(rb_df, "data/results/RB/ff_rankings_2023.csv")
data.table::fwrite(te_df, "data/results/TE/ff_rankings_2023.csv")
data.table::fwrite(total_df, "data/results/ff_rankings_2023.csv")








