################################################################################
# Load Libraries
################################################################################

require(tidyverse)
require(nflfastR)
require(nflreadr)

################################################################################
# Create File Path to Data Folder
################################################################################

if (!file.exists(file.path("data", "raw"))) {
  dir.create(file.path("data", "raw"))
}

################################################################################
# Load Play-by-Play Data
################################################################################

# Set sesasons
seasons <- 2000:2023

# Download data
pbp_data <- nflfastR::load_pbp(seasons)

# Create File Path
folder_path <- file.path("data", "raw", "pbp")

if (!file.exists(folder_path)) {
  dir.create(folder_path)
}

# Save Data
data.table::fwrite(pbp_data, file.path(folder_path, "pbp_raw_data.csv"),
                   row.names = FALSE)

################################################################################
# Load Rosters
################################################################################

rosters <- progressr::with_progress(nflreadr::load_rosters(seasons = seasons))

# Create File Path
folder_path <- file.path("data", "raw", "rosters")

if (!file.exists(folder_path)) {
  dir.create(folder_path)
}

# Save Data
data.table::fwrite(rosters, file.path(folder_path, "rosters.csv"),
                   row.names = FALSE)

################################################################################
# Load Player Stats
################################################################################

player_stats <- nflreadr::load_player_stats(seasons = seasons,
                                  stat_type = c("offense", "kicking"))

# Create File Path
folder_path <- file.path("data", "raw", "player_stats")

if (!file.exists(folder_path)) {
  dir.create(folder_path)
}

# Save Data
data.table::fwrite(player_stats, file.path(folder_path, "raw_player_stats.csv"),
                   row.names = FALSE)