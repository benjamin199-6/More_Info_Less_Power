###############################################################################
# 01_load_data.R
#
# Purpose:
# Load all raw data used in the project.
# No cleaning, no transformations.
###############################################################################

message("Running 01_load_data.R ...")

# ---------------------------------------------------------------------------
# Example: load main dataset
# ---------------------------------------------------------------------------

# Replace filename with your actual raw data file
# Supported formats: .csv, .rds, .sav, .dta

# Example CSV
# raw_data_main <- read_csv(
#   file.path(paths$data_raw, "raw_data_main.csv"),
#   show_col_types = FALSE
# )

# Example RDS
# raw_data_main <- readRDS(
#   file.path(paths$data_raw, "raw_data_main.rds")
# )

# ---------------------------------------------------------------------------
# Temporary placeholder (remove once real data is available)
# ---------------------------------------------------------------------------
raw_data_main <- tibble(
  id = 1:100,
  treatment = rbinom(100, 1, 0.5),
  outcome = rnorm(100)
)

message("Raw data loaded.")

