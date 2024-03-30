################################################################################
# Load Libraries
################################################################################

library(tidyverse)
library(lubridate)
library(caret)
library(boot)
options(dplyr.summarise.inform = FALSE)

################################################################################
# Load in Data
################################################################################

current_season <- nflfastR::fast_scraper_schedules(2023)
rosters <- nflfastR::fast_scraper_roster(2013:2023)
pbp <- nflfastR::load_pbp(2013:2022)

test <- nflfastR::clean_pbp(pbp)

################################################################################
# Calculate Strength of Schedule
################################################################################

# Offensive Rankings
# - Points Per Game
# - Total Yards Peer Game
# - Rushing TD per Game
# - Rushing Yards Per Game
# - Passing Yards Per Game
# - Passing TD per Game

# Defensive Rankings
# - Points Per Game
# - Total Yards Peer Game
# - Rushing TD per Game
# - Rushing Yards Per Game
# - Passing Yards Per Game
# - Passing TD per Game





