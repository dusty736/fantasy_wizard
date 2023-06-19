################################################################################
# Load Libraries
################################################################################

require(tidyverse)
require(nflfastR)

################################################################################
# Create File Path to Data Folder
################################################################################

folder_path <- "data"

if (!file.exists(folder_path)) {
  dir.create(folder_path)
}

################################################################################
# Load Play-by-Play Data
################################################################################

# Set sesasons
seasons <- 2012:2022

# Download data
pbp_data <- nflfastR::load_pbp(seasons)

# Create File Path
folder_path <- file.path("data", "pbp")

if (!file.exists(folder_path)) {
  dir.create(folder_path)
}

# Save Data
write.csv(pbp_data, file.path(folder_path, "pbp_raw_data.csv"))

################################################################################
# Load Rosters
################################################################################

rosters <- progressr::with_progress(nflreadr::load_rosters(seasons = seasons))

