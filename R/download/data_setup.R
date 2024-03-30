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
seasons <- 2013:2023

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
# Load Depth Charts
################################################################################

dc <- progressr::with_progress(nflreadr::load_depth_charts(seasons = seasons))

# Create File Path
folder_path <- file.path("data", "raw", "depth_charts")

if (!file.exists(folder_path)) {
  dir.create(folder_path)
}

# Save Data
data.table::fwrite(dc, file.path(folder_path, "depth_charts.csv"),
                   row.names = FALSE)

################################################################################
# Injuries
################################################################################

injuries <- progressr::with_progress(nflreadr::load_injuries(seasons = seasons))

# Create File Path
folder_path <- file.path("data", "raw", "injuries")

if (!file.exists(folder_path)) {
  dir.create(folder_path)
}

# Save Data
data.table::fwrite(dc, file.path(folder_path, "injuries.csv"),
                   row.names = FALSE)


################################################################################
# Participation
################################################################################

participation <- progressr::with_progress(nflreadr::load_participation(seasons = 2016:2023))

# Create File Path
folder_path <- file.path("data", "raw", "formations")

if (!file.exists(folder_path)) {
  dir.create(folder_path)
}

# Save Data
data.table::fwrite(participation, file.path(folder_path, "formations.csv"),
                   row.names = FALSE)

################################################################################
# NGS
################################################################################

ngs <- progressr::with_progress(nflreadr::load_nextgen_stats(seasons = 2016:2023))

# Create File Path
folder_path <- file.path("data", "raw", "nextgen_stats")

if (!file.exists(folder_path)) {
  dir.create(folder_path)
}

# Save Data
data.table::fwrite(ngs, file.path(folder_path, "nextgen_stats.csv"),
                   row.names = FALSE)

################################################################################
# Contracts
################################################################################

contracts <- progressr::with_progress(nflreadr::load_contracts())

# Create File Path
folder_path <- file.path("data", "raw", "contracts")

if (!file.exists(folder_path)) {
  dir.create(folder_path)
}

# Save Data
data.table::fwrite(ngs, file.path(folder_path, "contracts.csv"),
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
