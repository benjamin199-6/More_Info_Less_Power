###############################################################################
# 00_setup.R
#
# Purpose:
# Global setup for the research project.
# - Load required packages
# - Define project paths
# - Set global options
# - Ensure reproducibility
#
# This script must be sourced FIRST.
###############################################################################

message("Running 00_setup.R ...")

# ---------------------------------------------------------------------------
# 1. Clean environment
# ---------------------------------------------------------------------------
rm(list = ls())
gc()
getwd()
# ---------------------------------------------------------------------------
# 2. Required packages
# ---------------------------------------------------------------------------
required_packages <- c(
  "tidyverse",
  "here",
  "janitor",
  "fixest",
  "readr"
)

installed <- rownames(installed.packages())
for (pkg in required_packages) {
  if (!pkg %in% installed) {
    install.packages(pkg)
  }
}

invisible(lapply(required_packages, library, character.only = TRUE))

# ---------------------------------------------------------------------------
# 3. Global options
# ---------------------------------------------------------------------------
options(
  scipen = 999,      # avoid scientific notation
  digits = 4
)

set.seed(123456)

# ---------------------------------------------------------------------------
# 4. Project paths (DO NOT use setwd())
# ---------------------------------------------------------------------------
paths <- list(
  data_raw       = here::here("data", "raw"),
  data_processed = here::here("data", "processed"),
  output_tables  = here::here("paper", "tables"),
  output_figures = here::here("paper", "figures"),
  output_models  = here::here("output", "models")
)

# Create folders if they do not exist
dir.create(paths$data_processed, showWarnings = F, recursive = TRUE)
dir.create(paths$output_tables, showWarnings = FALSE, recursive = TRUE)
dir.create(paths$output_figures, showWarnings = FALSE, recursive = TRUE)
dir.create(paths$output_models, showWarnings = FALSE, recursive = TRUE)

# ---------------------------------------------------------------------------
# 5. Sanity checks
# ---------------------------------------------------------------------------
stopifnot(dir.exists(paths$data_raw))
stopifnot(dir.exists(paths$data_processed))

message("00_setup.R completed successfully.")
