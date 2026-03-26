###############################################################################
# 02_randomization_template.R
#
# Purpose:
# Template for random assignment prior to data collection.
###############################################################################

message("Running randomization template...")

# Placeholder sample
n <- params$n_total

randomization_table <- tibble(
  id = 1:n,
  treatment = sample(c(0, 1), size = n, replace = TRUE)
)

head(randomization_table)

message("Randomization template completed.")
