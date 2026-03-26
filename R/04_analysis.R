###############################################################################
# 04_analysis.R
#
# Purpose:
# Estimate econometric models for the main analysis.
# Output: model objects used later for tables and figures.
###############################################################################

message("Running 04_analysis.R ...")

# ---------------------------------------------------------------------------
# 1. Baseline treatment effect
# ---------------------------------------------------------------------------

model_baseline <- feols(
  outcome ~ treatment,
  data = analysis_data
)

# ---------------------------------------------------------------------------
# 2. Extended specification (example)
# ---------------------------------------------------------------------------

# Placeholder for additional controls (if available later)
# model_controls <- feols(
#   outcome ~ treatment + age + income,
#   data = analysis_data
# )

# ---------------------------------------------------------------------------
# 3. Store models in a list (important for tables later)
# ---------------------------------------------------------------------------

models <- list(
  baseline = model_baseline
  # controls = model_controls
)

message("Models estimated successfully.")
