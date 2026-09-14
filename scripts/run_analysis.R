# ------------------------------------------------------------------------------
# Multilevel Models - Main Execution Script
# ------------------------------------------------------------------------------

# Step 1: Data Preparation
# Parses the raw fixed-width data, cleans it, centers variables, and saves to cache
message("Running data preparation...")
source("R/data_prep.R")
get_multilevel_data(format = "wide")

# Step 2: Fit Multilevel Models
# Loads the cached data, fits the various hierarchical models, and exports plots
message("Running models...")
source("R/models/models.R")

message("Pipeline completed successfully!")
