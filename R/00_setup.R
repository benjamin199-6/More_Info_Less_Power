###############################################################################
# 00_setup.R
# Purpose: Environment setup + path definition + raw data validation
###############################################################################

rm(list = ls())
gc()
message("Running 00_setup.R ...")

# ---------------------------------------------------------------------------
# 1. Required packages (fail if missing)
# ---------------------------------------------------------------------------
required_packages <- c(
  "tidyverse",
  "data.table",
  "lubridate",
  "R.utils",
  "haven",
  "fixest",
  "MatchIt",
  "tableone",
  "stargazer",
  "outliers",
  "estimatr",
  "interactions",
  "janitor",
  "here",
  "readxl"
)

missing_packages <- required_packages[!required_packages %in% rownames(installed.packages())]

if (length(missing_packages) > 0) {
  stop(paste0(
    "Missing required packages: ",
    paste(missing_packages, collapse = ", "),
    "\nPlease install them manually before running the replication."
  ))
}

invisible(lapply(required_packages, library, character.only = TRUE))

# ---------------------------------------------------------------------------
# 2. Global options
# ---------------------------------------------------------------------------
options(
  scipen = 999,
  digits = 4
)

set.seed(123456)

# ---------------------------------------------------------------------------
# 3. Project paths
# ---------------------------------------------------------------------------
paths <- list(
  data_raw       = here::here("data", "raw"),
  data_processed = here::here("data", "processed"),
  output_tables  = here::here("paper", "tables"),
  output_figures = here::here("paper", "figures"),
  output_models  = here::here("output", "models")
)

# Create folders if they do not exist
dir.create(paths$data_processed, showWarnings = FALSE, recursive = TRUE)
dir.create(paths$output_tables, showWarnings = FALSE, recursive = TRUE)
dir.create(paths$output_figures, showWarnings = FALSE, recursive = TRUE)
dir.create(paths$output_models, showWarnings = FALSE, recursive = TRUE)

# ---------------------------------------------------------------------------
# 4. Sanity checks (raw data existence ONLY)
# ---------------------------------------------------------------------------
required_files <- c(
  file.path(paths$data_raw, "PA_cleaned.csv"),
  file.path(paths$data_raw, "MILP_app_usage.xlsx")
)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop(paste0(
    "Missing required raw data files:\n",
    paste(missing_files, collapse = "\n")
  ))
}

message("00_setup.R completed successfully.")