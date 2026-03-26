###############################################################################
# 00_setup_prestudy.R
#
# Purpose:
# Global setup for pre-study simulations and power analysis.
###############################################################################

message("Running prestudy setup...")

# Packages (keep minimal)
library(tidyverse)

# Global options
options(stringsAsFactors = FALSE)

# Set seed PLACEHOLDER (to be fixed per project)
set.seed(12345)

# Generic parameters (to be overwritten in real projects)
params <- list(
  n_total = 1000,     # placeholder sample size
  alpha   = 0.05,
  power   = 0.80
)

message("Prestudy setup completed.")

