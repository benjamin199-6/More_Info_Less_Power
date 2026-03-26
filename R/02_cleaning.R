###############################################################################
# 02_cleaning.R
#
# Purpose:
# Clean and prepare raw data for analysis.
# Output: one clean analysis-ready dataset.
###############################################################################

message("Running 02_cleaning.R ...")

# ---------------------------------------------------------------------------
# 1. Basic cleaning
# ---------------------------------------------------------------------------

analysis_data <- raw_data_main %>%
  janitor::clean_names() %>%
  mutate(
    treatment = factor(
      treatment,
      levels = c(0, 1),
      labels = c("Control", "Treatment")
    )
  )

# ---------------------------------------------------------------------------
# 2. Sanity checks
# ---------------------------------------------------------------------------

stopifnot(!any(is.na(analysis_data$id)))
stopifnot(all(levels(analysis_data$treatment) %in% c("Control", "Treatment")))

# ---------------------------------------------------------------------------
# 3. Save processed data
# ---------------------------------------------------------------------------

write_csv(
  analysis_data,
  file.path(paths$data_processed, "analysis_data.csv")
)

message("Data cleaning completed successfully.")
