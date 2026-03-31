###############################################################################
# 01_load_and_cache_data.R
#
# Purpose:
# - Load raw data
# - Save as fast-access .rds files
# - NO transformations
###############################################################################

message("Running 01_load_and_cache_data.R ...")
head(raw_consumption)
# ---------------------------------------------------------------------------
# 1. Load raw datasets
# ---------------------------------------------------------------------------

message("Loading consumption + household data...")
raw_consumption <- data.table::fread(
  file.path(paths$data_raw, "PA_cleaned.csv")
)

# ---------------------------------------------------------------------------
# 2. Save as .rds (fast access)
# ---------------------------------------------------------------------------

message("Saving cached .rds files...")

saveRDS(
  raw_consumption,
  file.path(paths$data_processed, "raw_consumption.rds")
)



### Cleaning Google Analytics data 

app_usage <- read_excel(
  file.path(paths$data_raw, "MILP_app_usage.xlsx"),
  sheet = "Sheet2"
)

app_usage <- as.data.table(app_usage)

# ---------------------------------------------------------------------------
# 2. Rename variables (clean + consistent)
# ---------------------------------------------------------------------------
setnames(app_usage,
         old = c("Household ID", "Year", "Month", "Day"),
         new = c("household", "year", "month", "day")
)

setnames(app_usage,
         old = c("Time (Hourly)", "Sessions", "Analysis", "Benchmark", "Game", "Bets"),
         new = c("hour_raw", "sessions", "analysis", "benchmark", "game", "bets")
)

names(app_usage)


if (is.numeric(app_usage$hour_raw)) {
  app_usage[, hour := as.integer(hour_raw)]
} else {
  # fallback if stored as datetime or string
  app_usage[, hour := hour(ymd_hms(hour_raw))]
}

# Remove raw column
app_usage[, hour_raw := NULL]


---------------------------------------------------------------------------
  app_usage[, household := as.integer(household)]
app_usage[, `:=`(
  year  = as.integer(year),
  month = as.integer(month),
  day   = as.integer(day),
  hour  = as.integer(hour)
)]


dup_check <- app_usage[, .N, by = .(household, year, month, day, hour)][N > 1]


saveRDS(app_usage, file.path(paths$data_processed, "app_usage_clean.rds"))

fwrite(app_usage, file.path(paths$data_processed, "app_usage_clean.csv.gz"))



message("01_load_and_cache_data.R completed successfully.")
