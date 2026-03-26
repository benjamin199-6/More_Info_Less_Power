###############################################################################
# masterfile_prestudy.R
#
# Purpose:
# Run all pre-study scripts (power analysis, simulation, randomization).
###############################################################################

rm(list = ls())
gc()

message("Starting prestudy masterfile...")

source("prestudy/R/00_setup_prestudy.R")
source("prestudy/R/01_power_simulation.R")
source("prestudy/R/02_randomization_template.R")

message("Prestudy masterfile finished successfully.")
