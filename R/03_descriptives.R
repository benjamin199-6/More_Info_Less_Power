###############################################################################
# 03_descriptives.R
#
# Purpose:
# Create descriptive statistics and basic sample summaries.
###############################################################################

message("Running 03_descriptives.R ...")

# ---------------------------------------------------------------------------
# 1. Overall descriptives
# ---------------------------------------------------------------------------

descriptives_overall <- analysis_data %>%
  summarise(
    n = n(),
    mean_outcome = mean(outcome),
    sd_outcome   = sd(outcome),
    min_outcome  = min(outcome),
    max_outcome  = max(outcome)
  )

# ---------------------------------------------------------------------------
# 2. Descriptives by treatment group
# ---------------------------------------------------------------------------

descriptives_by_treatment <- analysis_data %>%
  group_by(treatment) %>%
  summarise(
    n = n(),
    mean_outcome = mean(outcome),
    sd_outcome   = sd(outcome)
  )

# ---------------------------------------------------------------------------
# 3. Save descriptives
# ---------------------------------------------------------------------------

write_csv(
  descriptives_overall,
  file.path(paths$output_tables, "descriptives_overall.csv")
)

write_csv(
  descriptives_by_treatment,
  file.path(paths$output_tables, "descriptives_by_treatment.csv")
)

message("Descriptives completed successfully.")
