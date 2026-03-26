###############################################################################
# 05_tables_figures.R
#
# Purpose:
# Create publication-ready tables and figures.
###############################################################################

message("Running 05_tables_figures.R ...")

# ---------------------------------------------------------------------------
# 1. Regression table (LaTeX)
# ---------------------------------------------------------------------------

etable(
  models,
  tex = TRUE,
  file = file.path(paths$output_tables, "regression_results.tex"),
  replace = TRUE,
  digits = 3,
  se = "hetero",
  title = "Treatment Effects on Outcome"
)

# ---------------------------------------------------------------------------
# 2. Simple outcome distribution plot
# ---------------------------------------------------------------------------

p_density <- ggplot(analysis_data, aes(x = outcome, fill = treatment)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(
    title = "Outcome Distribution by Treatment Status",
    x = "Outcome",
    y = "Density",
    fill = "Group"
  )

ggsave(
  filename = file.path(paths$output_figures, "outcome_density.png"),
  plot = p_density,
  width = 6,
  height = 4,
  dpi = 300
)

message("Tables and figures created successfully.")
