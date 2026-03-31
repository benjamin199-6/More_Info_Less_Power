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
length(final_hh)
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

# Define tranche variable
dt_hourly[, tranche := fcase(
  Tranche1 == 1, "Tranche 1",
  Tranche2 == 1, "Tranche 2",
  Tranche3 == 1, "Tranche 3"
)]


dt_hourly %>% group_by(tranche,group) %>% summarise(n_distinct(household))

# ---------------------------------------------------------------------------
# 7. Keep only Control vs App for main descriptives
# ---------------------------------------------------------------------------
dt_hourly_ca <- dt_hourly[group %in% c("Control", "App")]
dt_15_ca     <- dt_15[group %in% c("Control", "App")]

dt_hourly_pre <- dt_hourly_ca[post == 0]
dt_15_pre     <- dt_15_ca[post == 0]


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



###############################################################################
# Pre-treatment comparison plot by tranche (calendar time)
###############################################################################
names(dt_hourly_pre_tranche)
# ---------------------------------------------------------------------------
# 9b. Pre-treatment hourly comparison by tranche (calendar time)
#     Plot hourly mean consumption for App vs Control in each tranche
# ---------------------------------------------------------------------------
message("Creating hourly pre-treatment comparison by tranche (calendar time)...")

# Keep only Control and App households in the pre-treatment period
dt_hourly_pre_tranche <- dt_hourly[
  group %in% c("Control", "App") &
    post == 0 &
    !is.na(treat_date)
]

# Define tranche variable
dt_hourly_pre_tranche[, tranche := fcase(
  Tranche1 == 1, "Tranche 1",
  Tranche2 == 1, "Tranche 2",
  Tranche3 == 1, "Tranche 3"
)]

dt_hourly_pre_tranche[, tranche := factor(
  tranche,
  levels = c("Tranche 1", "Tranche 2", "Tranche 3")
)]

# Ensure group order is consistent
dt_hourly_pre_tranche[, group := factor(group, levels = c("Control", "App"))]

# Aggregate to hourly mean consumption by calendar time, tranche, and group
plot_hourly_tranche <- dt_hourly_pre_tranche[, .(
  mean_consumption = mean(consumption, na.rm = TRUE)
), by = .(datetime, tranche, group)]

N_table <- dt_hourly_pre_tranche[, .(
  N = uniqueN(household)
), by = .(tranche, group)]

N_wide <- dcast(N_table, tranche ~ group, value.var = "N")

# Create clean labels
N_wide[, label := paste0(
  tranche, " (Control: ", Control, ", App: ", App, ")"
)]

label_map <- setNames(N_wide$label, N_wide$tranche)

# Plot
fig_hourly_tranche_pre <- ggplot(
  plot_hourly_tranche,
  aes(x = datetime, y = mean_consumption, color = group)
) +
  geom_line(linewidth = 0.5, alpha = 0.9) +
  facet_wrap(
    ~ tranche,
    scales = "free_x",
    ncol = 1,
    labeller = labeller(tranche = label_map)
  )+
  scale_color_manual(values = pal2) +
  theme_minimal() +
  labs(
    x = "Date",
    y = "Mean hourly consumption (kWh)",
    color = NULL
  ) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(
  file.path(fig_dir, "descriptive_hourly_pre_by_tranche_calendar.pdf"),
  fig_hourly_tranche_pre,
  width = 10,
  height = 8
)




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







# ---------------------------------------------------------------------------
# Covariate balance table (Control vs App) – tailored to your data
# ---------------------------------------------------------------------------
message("Creating covariate balance table (TableOne)...")

library(data.table)
library(tableone)

# ---------------------------------------------------------------------------
# 1. Household-level dataset
# ---------------------------------------------------------------------------
hh <- unique(dt_hourly_ca[, .(
  household,
  group,
  
  # Heating
  gas, district, biomass, oil, electric, heatPump,
  
  # Housing
  home_owned, apartment, singlefamily, splithouse,
  
  # Appliances
  swimmingPool, sauna, airCondition, aquarium, dryer,
  waterBed, deepFreezers, computers, pev, ebike,
  
  # Other
  tariffgrp, numberofresidents, square_meter
)])

# Ensure correct group order
hh[, group := factor(group, levels = c("Control", "App"))]

# ---------------------------------------------------------------------------
# 2. Variable lists (matching your table structure)
# ---------------------------------------------------------------------------
vars <- c(
  # Heating
  "gas","district","biomass","oil","electric","heatPump",
  
  # Housing
  "home_owned","apartment","singlefamily","splithouse",
  
  # Appliances
  "swimmingPool","sauna","airCondition","aquarium","dryer",
  "waterBed","deepFreezers","computers","pev","ebike",
  
  "numberofresidents","square_meter"
)

# ---------------------------------------------------------------------------
# 3. Create TableOne (t-tests only, no SMD)
# ---------------------------------------------------------------------------
tab <- CreateTableOne(
  vars = vars,
  strata = "group",
  data = as.data.frame(hh),
  test = TRUE
)

# ---------------------------------------------------------------------------
# 4. Print clean output
# ---------------------------------------------------------------------------
print(
  tab,
  showAllLevels = FALSE,
  test = TRUE,
  smd = FALSE
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