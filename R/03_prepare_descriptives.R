###############################################################################
# 03_build_analysis_dataset.R
###############################################################################

message("Running 03_build_analysis_dataset.R ...")

# ---------------------------------------------------------------------------
# 0. Setup
# ---------------------------------------------------------------------------
source(here::here("R", "00_setup.R"))
gc()

library(data.table)

# ---------------------------------------------------------------------------
# 1. Load data
# ---------------------------------------------------------------------------
dt_hourly <- fread(file.path(paths$data_processed, "dt_hourly.csv.gz"))
dt_hh     <- readRDS(file.path(paths$data_processed, "dt_hh.rds"))
dt_app    <- readRDS(file.path(paths$data_processed, "raw_app.rds"))

setDT(dt_hourly)
setDT(dt_app)
setDT(dt_hh)

# ---------------------------------------------------------------------------
# 2. Clean & harmonize app data
# ---------------------------------------------------------------------------
message("Cleaning app data...")

setnames(dt_app,
         old = c("Household ID", "Year", "Month", "Day", "Time (Hourly)",
                 "Sessions", "Analysis", "Benchmark", "Game", "Bets"),
         new = c("household", "year", "month", "day", "hour",
                 "sessions", "analysis", "benchmark", "game", "bets"),
         skip_absent = TRUE)

# Keep only relevant columns (IMPORTANT for memory)
app_vars <- c("sessions", "analysis", "benchmark", "game", "bets")

dt_app <- dt_app[, c("household", "year", "month", "day", "hour", app_vars), with = FALSE]

# Ensure types match
dt_app[, `:=`(
  household = as.integer(household),
  year  = as.integer(year),
  month = as.integer(month),
  day   = as.integer(day),
  hour  = as.integer(hour)
)]

# ---------------------------------------------------------------------------
# 3. Merge app → hourly consumption (FAST JOIN)
# ---------------------------------------------------------------------------
message("Merging app data...")

setkey(dt_hourly, household, year, month, day, hour)
setkey(dt_app,    household, year, month, day, hour)

dt <- dt_app[dt_hourly]   # left join (keeps all dt_hourly rows)

# Replace NA app activity with 0
dt[, (app_vars) := lapply(.SD, function(x) fifelse(is.na(x), 0, x)),
   .SDcols = app_vars]

rm(dt_app, dt_hourly)
gc()

# ---------------------------------------------------------------------------
# 4. Merge household characteristics
# ---------------------------------------------------------------------------
message("Merging household data...")

# Ensure no duplicate HH rows
dt_hh <- unique(dt_hh, by = "household")

setkey(dt_hh, household)
dt <- dt_hh[dt]   # left join

rm(dt_hh)
gc()

# ---------------------------------------------------------------------------
# 5. Quick checks
# ---------------------------------------------------------------------------
message("Running checks...")

# Check no row explosion
stopifnot(nrow(dt) > 0)

# Check app variables
summary(dt[, ..app_vars])

# Group structure check
print(
  dt[, .(n_households = uniqueN(household)), by = groupnumber]
)

# ---------------------------------------------------------------------------
# 6. Save analysis dataset
# ---------------------------------------------------------------------------
message("Saving analysis dataset...")

saveRDS(dt, file.path(paths$data_processed, "dt_analysis.rds"))
fwrite(dt,  file.path(paths$data_processed, "dt_analysis.csv.gz"))

message("03_build_analysis_dataset.R completed successfully.")