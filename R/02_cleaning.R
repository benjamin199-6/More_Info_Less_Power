###############################################################################
# 02_cleaning.R
#
# Purpose:
# - Preserve household characteristics
# - Aggregate consumption across multiple meters
# - Create lean, analysis-ready panel datasets
# - Perform core data quality checks
# - Save clean datasets in efficient formats
###############################################################################

message("Running 02_cleaning.R ...")

# ---------------------------------------------------------------------------
# 0. Setup
# ---------------------------------------------------------------------------
source(here::here("R", "00_setup.R"))
gc()

library(data.table)
library(lubridate)
library(stringr)

# ---------------------------------------------------------------------------
# 1. Load data
# ---------------------------------------------------------------------------
dt <- readRDS(file.path(paths$data_processed, "raw_consumption.rds"))
setDT(dt)

dt[, clock_local := as.POSIXct(clock_local)]

# ---------------------------------------------------------------------------
# 1.1 Parse message variable into structured treatment info
# ---------------------------------------------------------------------------
message("Parsing message variable...")

dt[, discount_level := as.numeric(str_extract(message, "\\d+(?= ?%)"))]
dt[, discount_dummy_clean := as.integer(!is.na(discount_level))]

dt[, message_timing := fcase(
  str_detect(message, regex("morgen", ignore_case = TRUE)), "tomorrow",
  str_detect(message, regex("heute",  ignore_case = TRUE)), "today",
  default = NA_character_
)]

dt[, energy_type := fcase(
  str_detect(message, regex("Windstrom",  ignore_case = TRUE)), "wind",
  str_detect(message, regex("Solarstrom", ignore_case = TRUE)), "solar",
  str_detect(message, regex("Markt",      ignore_case = TRUE)), "market",
  default = "other"
)]

dt[, message_type := fcase(
  str_detect(message, regex("Rabatt",         ignore_case = TRUE)), "discount",
  str_detect(message, regex("Geschenkt",      ignore_case = TRUE)), "free_energy",
  str_detect(message, regex("Stromspar-Test", ignore_case = TRUE)), "experiment",
  str_detect(message, regex("SMART-TARIF",    ignore_case = TRUE)), "marketing",
  default = "other"
)]

dt[, discount_hour := as.numeric(str_extract(message, "(?<=zwischen )\\d{1,2}"))]

# ---------------------------------------------------------------------------
# 1.2 Extract strictly static household characteristics
# ---------------------------------------------------------------------------
message("Extracting household characteristics...")

hh_vars <- c(
  "household",
  "meterid",
  "Tranche1", "Tranche2", "Tranche3",
  "groupnumber",
  "numberofresidents",
  "splithouse", "apartment", "singlefamily",
  "square_meter", "home_owned",
  "gas", "district", "heatPump", "electric", "biomass", "oil", "other",
  "water_gas", "water_district", "water_heatpump", "water_electric",
  "water_biomass", "water_oil", "water_other",
  "dryer", "swimmingPool", "aquarium", "waterBed", "sauna",
  "airCondition", "deepFreezers", "computers",
  "pv_capacity", "batterystorage", "pev", "emotorbike", "ebike", "pv",
  "controlgrp", "appgrp", "tariffgrp"
)

hh_vars <- intersect(hh_vars, names(dt))

dt_hh <- unique(
  dt[, ..hh_vars],
  by = "household"
)

# ---------------------------------------------------------------------------
# 2. Create lean household-time panel
#    Fast approach: aggregate only consumption, keep one row per HH-time
# ---------------------------------------------------------------------------
message("Aggregating multiple meters per household...")

# 2.1 Aggregate only the variable that truly varies across meters
dt_cons_agg <- dt[, .(
  consumption = sum(consumption, na.rm = TRUE)
), by = .(household, clock_local)]

# 2.2 Keep only variables actually needed in the panel
keep_vars <- c(
  "household", "clock_local",
  "year", "month", "day", "dow", "hour",
  "groupnumber", "controlgrp", "appgrp", "tariffgrp",
  "Tranche1", "Tranche2", "Tranche3",
  "tariff_kwh", "tariff_year", "tariff_name",
  "discount", "discount_dummy",
  "discount_end", "discount_duration",
  "discount_reason", "discount_reason_missing",
  "discount_clock", "discount_delay",
  "start_clock", "discount_start",
  "message",
  "discount_level", "discount_dummy_clean",
  "message_timing", "energy_type", "message_type", "discount_hour"
)

keep_vars <- intersect(keep_vars, names(dt))

dt_unique <- unique(
  dt[, ..keep_vars],
  by = c("household", "clock_local")
)

# 2.3 Merge aggregated consumption back
dt_cons <- dt_unique[dt_cons_agg, on = .(household, clock_local)]

rm(dt, dt_unique, dt_cons_agg)
gc()

# Clean edge case from max parsing logic elsewhere / imported artifacts
dt_cons[is.infinite(discount_level), discount_level := NA_real_]

# ---------------------------------------------------------------------------
# 3. Data quality checks
# ---------------------------------------------------------------------------
message("Running data quality checks...")

dt_check <- dt_cons[, .(
  start = min(clock_local),
  end   = max(clock_local),
  expected = as.numeric((max(clock_local) - min(clock_local)) / 900) + 1,
  actual = .N
), by = household]

dt_check[, missing := expected - actual]
dt_check[, coverage := actual / expected]

print(summary(dt_check$missing))
print(summary(dt_check$coverage))

setorder(dt_cons, household, clock_local)
dt_cons[, diff := as.numeric(clock_local - shift(clock_local)), by = household]

message("Time gap distribution:")
print(dt_cons[, .N, by = diff][order(diff)])

dup_check <- dt_cons[, .N, by = .(household, clock_local)][N > 1]
if (nrow(dup_check) > 0) {
  warning("Duplicates detected after aggregation!")
}

# ---------------------------------------------------------------------------
# 4. Behavioral classification
# ---------------------------------------------------------------------------
active_hours <- c(6:9, 17:22)
peak_hours   <- 17:20

dt_cons[, period := fcase(
  hour %in% peak_hours, "peak",
  hour %in% active_hours, "active_non_peak",
  default = "passive"
)]

# Optional but useful: treatment-aligned period
dt_cons[, treated_period := as.integer(!is.na(discount_hour) & hour == discount_hour)]

# ---------------------------------------------------------------------------
# 5. Aggregations
# ---------------------------------------------------------------------------
message("Aggregating consumption...")

dt_daily_behavior <- dt_cons[, .(
  consumption_total           = sum(consumption, na.rm = TRUE),
  consumption_peak            = sum(consumption[period == "peak"], na.rm = TRUE),
  consumption_active_non_peak = sum(consumption[period == "active_non_peak"], na.rm = TRUE),
  consumption_passive         = sum(consumption[period == "passive"], na.rm = TRUE),
  consumption_treated_period  = sum(consumption[treated_period == 1], na.rm = TRUE)
), by = .(household, year, month, day)]

dt_daily_behavior[, share_peak := fifelse(consumption_total > 0,
                                          consumption_peak / consumption_total,
                                          NA_real_)]

dt_daily_behavior[, share_active := fifelse(
  consumption_total > 0,
  (consumption_peak + consumption_active_non_peak) / consumption_total,
  NA_real_
)]

dt_hourly <- dt_cons[, .(
  consumption = sum(consumption, na.rm = TRUE)
), by = .(household, year, month, day, hour)]

dt_daily <- dt_cons[, .(
  consumption = sum(consumption, na.rm = TRUE)
), by = .(household, year, month, day)]

dt_monthly <- dt_cons[, .(
  consumption = sum(consumption, na.rm = TRUE)
), by = .(household, year, month)]

dt_yearly <- dt_cons[, .(
  consumption = sum(consumption, na.rm = TRUE)
), by = .(household, year)]

# ---------------------------------------------------------------------------
# 6. Save datasets
# ---------------------------------------------------------------------------
message("Saving datasets...")

# Save household file
saveRDS(dt_hh, file.path(paths$data_processed, "dt_hh.rds"))

# Save compact behavior file
saveRDS(dt_daily_behavior, file.path(paths$data_processed, "dt_daily_behavior.rds"))
fwrite(dt_daily_behavior, file.path(paths$data_processed, "dt_daily_behavior.csv.gz"))

# Save lean panel and aggregates
fwrite(dt_cons,    file.path(paths$data_processed, "dt_15min.csv.gz"))
fwrite(dt_hourly,  file.path(paths$data_processed, "dt_hourly.csv.gz"))
fwrite(dt_daily,   file.path(paths$data_processed, "dt_daily.csv.gz"))
fwrite(dt_monthly, file.path(paths$data_processed, "dt_monthly.csv.gz"))
fwrite(dt_yearly,  file.path(paths$data_processed, "dt_yearly.csv.gz"))

message("02_cleaning.R completed successfully.")