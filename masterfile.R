###############################################################################
# masterfile.R
#
# Purpose:
# Run the full analysis pipeline from raw data to final tables & figures.
###############################################################################

rm(list = ls())
gc()

message("Starting masterfile...")

source("R/00_setup.R")
source("R/01_load_data.R")
source("R/02_cleaning.R")
source("R/03_descriptives.R")
source("R/04_analysis.R")
source("R/05_tables_figures.R")

message("Masterfile finished successfully.")
