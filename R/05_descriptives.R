###############################################################################
# 05_descriptive.R
#
# Purpose:
# - Load final hourly analysis data and 15-min data
# - Restrict 15-min data to the final estimation sample
# - Create descriptive figures:
#     * pre-treatment event-time trends (hourly and 15-min)
#     * pre-treatment load curves (hourly and 15-min)
# - Run randomization checks on household characteristics
# - Create app engagement summary tables
###############################################################################

# ---------------------------------------------------------------------------
# 0. Setup
# ---------------------------------------------------------------------------
source(here::here("R", "00_setup.R"))
gc()

library(data.table)
library(ggplot2)
library(wesanderson)
library(tableone)

message("Running 05_descriptive.R ...")

# Output folders
fig_dir <- file.path(paths$output_figures)
tab_dir <- file.path(paths$output_tables)
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(tab_dir, showWarnings = FALSE, recursive = TRUE)

pal2 <- wes_palette("Darjeeling1", 2, type = "discrete")

# ---------------------------------------------------------------------------
# 1. Load data
# ---------------------------------------------------------------------------
message("Loading final hourly data...")
dt_hourly <- readRDS(file.path(paths$data_processed, "dt_final.rds"))
setDT(dt_hourly)

message("Loading 15-min data...")
dt_15 <- fread(file.path(paths$data_processed, "dt_15min.csv.gz"))
setDT(dt_15)


# ---------------------------------------------------------------------------
# 2. Harmonize time variables
# ---------------------------------------------------------------------------
if (!"datetime" %in% names(dt_hourly)) {
  dt_hourly[, datetime := as.POSIXct(
    paste(year, month, day, hour),
    format = "%Y %m %d %H"
  )]
}
if (!"date" %in% names(dt_hourly)) {
  dt_hourly[, date := as.Date(datetime)]
}

dt_15[, datetime := as.POSIXct(clock_local)]
dt_15[, date := as.Date(datetime)]

# ---------------------------------------------------------------------------
# 3. Aggregate 15-min data over meter points first
#    We want one row per household and 15-min timestamp.
# ---------------------------------------------------------------------------
message("Aggregating 15-min data to household level...")

dt_15 <- dt_15[, .(
  consumption = sum(consumption, na.rm = TRUE),
  year        = first(year),
  month       = first(month),
  day         = first(day),
  hour        = first(hour),
  dow         = first(dow),
  controlgrp  = first(controlgrp),
  tariffgrp   = first(tariffgrp),
  appgrp      = first(appgrp),
  Tranche1    = first(Tranche1),
  Tranche2    = first(Tranche2),
  Tranche3    = first(Tranche3)
), by = .(household, datetime, date)]

# safety check
dup_15 <- dt_15[, .N, by = .(household, datetime)][N > 1]
stopifnot(nrow(dup_15) == 0)

# ---------------------------------------------------------------------------
# 4. Restrict 15-min data to final sample households and time window
# ---------------------------------------------------------------------------
message("Filtering 15-min data to final sample households and time window...")

final_hh <- unique(dt_hourly$household)
dt_15 <- dt_15[household %in% final_hh]

hh_window <- dt_hourly[, .(
  min_time = min(datetime, na.rm = TRUE),
  max_time = max(datetime, na.rm = TRUE)
), by = household]

setkey(hh_window, household)
setkey(dt_15, household)

dt_15 <- hh_window[dt_15]
dt_15 <- dt_15[datetime >= min_time & datetime <= max_time]
dt_15[, c("min_time", "max_time") := NULL]

# ---------------------------------------------------------------------------
# 5. Bring over household-constant treatment/group variables from final sample
#    Important: do NOT include post here because post is time-varying.
# ---------------------------------------------------------------------------
group_map <- unique(dt_hourly[, .(
  household,
  group,
  controlgrp,
  appgrp,
  tariffgrp,
  Tranche1,
  Tranche2,
  Tranche3
)])

# ensure truly one row per household
dup_group <- group_map[, .N, by = household][N > 1]
stopifnot(nrow(dup_group) == 0)

overlap_cols <- intersect(names(dt_15), setdiff(names(group_map), "household"))
if (length(overlap_cols) > 0) {
  dt_15[, (overlap_cols) := NULL]
}

setkey(group_map, household)
setkey(dt_15, household)
dt_15 <- group_map[dt_15]

# derive 15-min position in day for plotting
dt_15[, minute := as.integer(format(datetime, "%M"))]
dt_15[, hour_of_day_15 := as.numeric(format(datetime, "%H")) + minute / 60]

# ---------------------------------------------------------------------------
# 6. Define treatment dates, post, and event time
# ---------------------------------------------------------------------------
tranche_dates <- list(
  tranche1 = as.Date("2017-06-06"),
  tranche2 = as.Date("2017-09-19"),
  tranche3 = as.Date("2017-11-20")
)

dt_hourly[, treat_date := fcase(
  Tranche1 == 1, tranche_dates$tranche1,
  Tranche2 == 1, tranche_dates$tranche2,
  Tranche3 == 1, tranche_dates$tranche3,
  default = as.Date(NA)
)]

dt_15[, treat_date := fcase(
  Tranche1 == 1, tranche_dates$tranche1,
  Tranche2 == 1, tranche_dates$tranche2,
  Tranche3 == 1, tranche_dates$tranche3,
  default = as.Date(NA)
)]

# recreate post in 15-min data exactly as in analysis
dt_15[, post := fcase(
  Tranche1 == 1 & date >= tranche_dates$tranche1, 1,
  Tranche2 == 1 & date >= tranche_dates$tranche2, 1,
  Tranche3 == 1 & date >= tranche_dates$tranche3, 1,
  default = 0
)]

dt_hourly[, rel_day := as.integer(date - treat_date)]
dt_15[, rel_day := as.integer(date - treat_date)]

# ---------------------------------------------------------------------------
# 7. Keep only Control vs App for main descriptives
# ---------------------------------------------------------------------------
dt_hourly_ca <- dt_hourly[group %in% c("Control", "App")]
dt_15_ca     <- dt_15[group %in% c("Control", "App")]

dt_hourly_pre <- dt_hourly_ca[post == 0]
dt_15_pre     <- dt_15_ca[post == 0]

# ---------------------------------------------------------------------------
# 8. Pre-treatment event-time trends
# ---------------------------------------------------------------------------
message("Creating pre-treatment event-time trend graphs...")

dt_hourly_event_pre <- dt_hourly_pre[rel_day >= -20 & rel_day <= -1]
dt_15_event_pre     <- dt_15_pre[rel_day >= -20 & rel_day <= -1]

plot_hourly_event <- dt_hourly_event_pre[, .(
  mean_consumption = mean(consumption, na.rm = TRUE)
), by = .(rel_day, group)]

fig_hourly_event <- ggplot(
  plot_hourly_event,
  aes(x = rel_day, y = mean_consumption, color = group)
) +
  geom_line(linewidth = 0.9) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal2) +
  theme_minimal() +
  labs(
    x = "Days relative to treatment",
    y = "Mean hourly consumption (kWh)",
    color = NULL
  )

ggsave(
  file.path(fig_dir, "descriptive_pretrend_hourly_eventtime.pdf"),
  fig_hourly_event,
  width = 8,
  height = 5
)

plot_15_event <- dt_15_event_pre[, .(
  mean_consumption = mean(consumption, na.rm = TRUE)
), by = .(rel_day, group)]

fig_15_event <- ggplot(
  plot_15_event,
  aes(x = rel_day, y = mean_consumption, color = group)
) +
  geom_line(linewidth = 0.9) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal2) +
  theme_minimal() +
  labs(
    x = "Days relative to treatment",
    y = "Mean 15-min consumption (kWh)",
    color = NULL
  )

ggsave(
  file.path(fig_dir, "descriptive_pretrend_15min_eventtime.pdf"),
  fig_15_event,
  width = 8,
  height = 5
)

# ---------------------------------------------------------------------------
# 9. Pre-treatment load curves
# ---------------------------------------------------------------------------
message("Creating pre-treatment load curves...")

loadcurve_hourly <- dt_hourly_pre[, .(
  mean_consumption = mean(consumption, na.rm = TRUE)
), by = .(group, hour)]

fig_loadcurve_hourly <- ggplot(
  loadcurve_hourly,
  aes(x = hour, y = mean_consumption, color = group)
) +
  geom_line(linewidth = 0.9) +
  scale_x_continuous(breaks = seq(0, 23, by = 2)) +
  scale_color_manual(values = pal2) +
  theme_minimal() +
  labs(
    x = "Hour of day",
    y = "Mean hourly consumption (kWh)",
    color = NULL
  )

ggsave(
  file.path(fig_dir, "descriptive_loadcurve_hourly_pre.pdf"),
  fig_loadcurve_hourly,
  width = 8,
  height = 5
)

loadcurve_15 <- dt_15_pre[, .(
  mean_consumption = mean(consumption, na.rm = TRUE)
), by = .(group, hour_of_day_15)]

fig_loadcurve_15 <- ggplot(
  loadcurve_15,
  aes(x = hour_of_day_15, y = mean_consumption, color = group)
) +
  geom_line(linewidth = 0.9) +
  scale_x_continuous(breaks = seq(0, 24, by = 2)) +
  scale_color_manual(values = pal2) +
  theme_minimal() +
  labs(
    x = "Hour of day",
    y = "Mean 15-min consumption (kWh)",
    color = NULL
  )

ggsave(
  file.path(fig_dir, "descriptive_loadcurve_15min_pre.pdf"),
  fig_loadcurve_15,
  width = 8,
  height = 5
)

message("Creating 15-min pre-treatment load curves by tranche...")

# ---------------------------------------------------------------------------
# 1. Keep pre-treatment observations only
# ---------------------------------------------------------------------------
dt_15_pre_tranche <- dt_15[
  post == 0 &
    !is.na(treat_date)
]

# ---------------------------------------------------------------------------
# 2. Define tranche variable
# ---------------------------------------------------------------------------
dt_15_pre_tranche[, tranche := fcase(
  Tranche1 == 1, "Tranche 1",
  Tranche2 == 1, "Tranche 2",
  Tranche3 == 1, "Tranche 3"
)]

dt_15_pre_tranche[, tranche := factor(tranche,
                                      levels = c("Tranche 1","Tranche 2","Tranche 3"))]



# --- t------------------------------------------------------------------------
# 10. Household-level randomization checks
# ---------------------------------------------------------------------------
message("Preparing household-level randomization checks...")

hh <- unique(dt_hourly_ca[, .(
  household,
  group,
  numberofresidents,
  square_meter,
  tariff_kwh,
  tariff_name,
  splithouse,
  apartment,
  singlefamily,
  home_owned,
  gas,
  district,
  heatPump,
  electric,
  biomass,
  oil,
  other,
  water_gas,
  water_district,
  water_heatpump,
  water_electric,
  water_biomass,
  water_oil,
  water_other,
  dryer,
  swimmingPool,
  aquarium,
  waterBed,
  sauna,
  airCondition,
  deepFreezers,
  computers,
  pv_capacity,
  batterystorage,
  pev,
  emotorbike,
  ebike,
  pv
)])

vars_num <- c(
  "tariff_kwh", "numberofresidents", "square_meter", "computers", "pv_capacity"
)

vars_cat <- c(
  "tariff_name", "splithouse", "apartment", "singlefamily", "home_owned",
  "gas", "district", "heatPump", "electric", "biomass", "oil", "other",
  "water_gas", "water_district", "water_heatpump", "water_electric",
  "water_biomass", "water_oil", "water_other", "dryer", "swimmingPool",
  "aquarium", "waterBed", "sauna", "airCondition", "deepFreezers",
  "pev", "emotorbike", "ebike", "pv"
)

vars_all <- intersect(c(vars_num, vars_cat), names(hh))
vars_cat <- intersect(vars_cat, names(hh))

hh_raw <- copy(hh)

for (v in vars_cat) {
  hh[, (v) := factor(get(v))]
}

hh[, group := factor(group, levels = c("Control", "App"))]

tab_rand <- CreateTableOne(
  vars       = vars_all,
  strata     = "group",
  data       = as.data.frame(hh),
  factorVars = vars_cat,
  test       = TRUE
)

rand_txt <- capture.output(
  print(tab_rand, showAllLevels = TRUE, test = TRUE, smd = TRUE)
)

writeLines(
  rand_txt,
  file.path(tab_dir, "randomization_check_tableone.txt")
)

hh_counts <- hh[, .(N_households = uniqueN(household)), by = group]
fwrite(
  hh_counts,
  file.path(tab_dir, "randomization_household_counts.csv")
)

# ---------------------------------------------------------------------------
# 11. Binary-variable difference summary
# ---------------------------------------------------------------------------
message("Computing binary difference checks...")

binary_vars <- vars_cat[
  sapply(hh_raw[, ..vars_cat], function(x) {
    ux <- sort(unique(na.omit(x)))
    length(ux) <= 2
  })
]

binary_results <- rbindlist(lapply(binary_vars, function(v) {
  tab <- table(hh_raw[[v]], hh_raw$group)
  
  pval <- tryCatch(
    suppressWarnings(chisq.test(tab)$p.value),
    error = function(e) NA_real_
  )
  
  data.table(
    variable = v,
    p_value = pval,
    mean_control = hh_raw[group == "Control", mean(get(v), na.rm = TRUE)],
    mean_app     = hh_raw[group == "App", mean(get(v), na.rm = TRUE)]
  )
}), fill = TRUE)

fwrite(
  binary_results,
  file.path(tab_dir, "randomization_binary_differences.csv")
)

# ---------------------------------------------------------------------------
# 12. App engagement table
# ---------------------------------------------------------------------------
message("Creating app engagement table...")

app_dt <- dt_hourly[group == "App"]

engagement_vars <- intersect(
  c("sessions", "analysis", "benchmark", "game", "bets"),
  names(app_dt)
)

for (v in engagement_vars) {
  app_dt[is.na(get(v)), (v) := 0]
}

app_dt[, app_engagement := 0]
for (v in engagement_vars) {
  app_dt[, app_engagement := app_engagement + get(v)]
}

app_hh <- app_dt[, .(
  total_sessions   = if ("sessions"  %in% names(app_dt)) sum(sessions,   na.rm = TRUE) else NA_real_,
  total_analysis   = if ("analysis"  %in% names(app_dt)) sum(analysis,   na.rm = TRUE) else NA_real_,
  total_benchmark  = if ("benchmark" %in% names(app_dt)) sum(benchmark,  na.rm = TRUE) else NA_real_,
  total_game       = if ("game"      %in% names(app_dt)) sum(game,       na.rm = TRUE) else NA_real_,
  total_bets       = if ("bets"      %in% names(app_dt)) sum(bets,       na.rm = TRUE) else NA_real_,
  total_engagement = sum(app_engagement, na.rm = TRUE)
), by = household]

app_hh[, `:=`(
  ever_sessions   = as.integer(total_sessions   > 0),
  ever_analysis   = as.integer(total_analysis   > 0),
  ever_benchmark  = as.integer(total_benchmark  > 0),
  ever_game       = as.integer(total_game       > 0),
  ever_bets       = as.integer(total_bets       > 0),
  ever_engaged    = as.integer(total_engagement > 0)
)]

engagement_table <- app_hh[, .(
  N_households            = .N,
  share_ever_engaged      = mean(ever_engaged,   na.rm = TRUE),
  share_ever_sessions     = mean(ever_sessions,  na.rm = TRUE),
  share_ever_analysis     = mean(ever_analysis,  na.rm = TRUE),
  share_ever_benchmark    = mean(ever_benchmark, na.rm = TRUE),
  share_ever_game         = mean(ever_game,      na.rm = TRUE),
  share_ever_bets         = mean(ever_bets,      na.rm = TRUE),
  mean_total_sessions     = mean(total_sessions,   na.rm = TRUE),
  mean_total_analysis     = mean(total_analysis,   na.rm = TRUE),
  mean_total_benchmark    = mean(total_benchmark,  na.rm = TRUE),
  mean_total_game         = mean(total_game,       na.rm = TRUE),
  mean_total_bets         = mean(total_bets,       na.rm = TRUE),
  mean_total_engagement   = mean(total_engagement, na.rm = TRUE),
  median_total_engagement = median(total_engagement, na.rm = TRUE)
)]

fwrite(
  engagement_table,
  file.path(tab_dir, "app_engagement_summary.csv")
)

app_daily <- app_dt[, .(
  total_engagement = sum(app_engagement, na.rm = TRUE)
), by = date]

fig_app <- ggplot(app_daily, aes(x = date, y = total_engagement)) +
  geom_line(linewidth = 0.8) +
  theme_minimal() +
  labs(
    x = "Date",
    y = "Total app engagement"
  )

ggsave(
  file.path(fig_dir, "descriptive_app_engagement_daily.pdf"),
  fig_app,
  width = 8,
  height = 5
)

# ---------------------------------------------------------------------------
# 13. Save filtered 15-min descriptive sample
# ---------------------------------------------------------------------------
saveRDS(
  dt_15,
  file.path(paths$data_processed, "dt_15min_finalsample.rds")
)

message("05_descriptive.R completed successfully.")