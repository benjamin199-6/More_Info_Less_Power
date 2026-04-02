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





# Count number of pre-treatment days per household
hh_days <- dt_hourly_pre_tranche[, .(
  n_days = uniqueN(date)
), by = .(household, tranche)]

# Find the balanced window (minimum across households)
window_tranche <- hh_days[, .(
  min_days = min(n_days),
  mean_days = mean(n_days),
  median_days = median(n_days)
), by = tranche]

print(window_tranche)

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
# 12. App engagement (clean + paper-ready)
# ---------------------------------------------------------------------------
message("Creating app engagement table...")

# --- 1. Subset App group ----------------------------------------------------
app_dt <- dt_hourly[group == "App"]

# Engagement variables (robust to missing columns)
engagement_vars <- intersect(
  c("sessions", "analysis", "benchmark", "game", "bets"),
  names(app_dt)
)

# Replace NA with 0
for (v in engagement_vars) {
  app_dt[is.na(get(v)), (v) := 0]
}

# --- 2. Total engagement per observation ------------------------------------
app_dt[, app_engagement := 0]
for (v in engagement_vars) {
  app_dt[, app_engagement := app_engagement + get(v)]
}

# --- 3. Collapse to household level -----------------------------------------
app_hh <- app_dt[, .(
  total_sessions   = sum(get("sessions"),   na.rm = TRUE),
  total_analysis   = sum(get("analysis"),   na.rm = TRUE),
  total_benchmark  = sum(get("benchmark"),  na.rm = TRUE),
  total_game       = sum(get("game"),       na.rm = TRUE),
  total_bets       = sum(get("bets"),       na.rm = TRUE),
  total_engagement = sum(app_engagement,    na.rm = TRUE)
), by = household]

# --- 4. Extensive margin (used at least once) --------------------------------
app_hh[, `:=`(
  ever_sessions   = as.integer(total_sessions   > 0),
  ever_analysis   = as.integer(total_analysis   > 0),
  ever_benchmark  = as.integer(total_benchmark  > 0),
  ever_game       = as.integer(total_game       > 0),
  ever_bets       = as.integer(total_bets       > 0),
  ever_engaged    = as.integer(total_engagement > 0)
)]

# --- 5. Summary table --------------------------------------------------------
engagement_table <- app_hh[, .(
  N_households              = .N,
  
  # Shares (these go into paper)
  share_used_app            = mean(ever_engaged),
  share_used_analysis       = mean(ever_analysis),
  share_used_benchmark      = mean(ever_benchmark),
  share_used_game           = mean(ever_game),
  
  # Totals (these go into paper)
  total_analysis            = sum(total_analysis),
  total_benchmark           = sum(total_benchmark),
  total_game                = sum(total_game),
  
  # Intensity (optional appendix)
  mean_total_engagement     = mean(total_engagement),
  median_total_engagement   = median(total_engagement)
)]

print(engagement_table)

# # Save table
# fwrite(
#   engagement_table,
#   file.path(tab_dir, "app_engagement_summary.csv")
# )

# --- 6. Concentration  ---------------------------------
app_hh[, total_interactions := total_analysis + total_benchmark + total_game]
setorder(app_hh, -total_interactions)

top10 <- app_hh[1:ceiling(.N * 0.1)]

share_top10 <- sum(top10$total_interactions) / sum(app_hh$total_interactions)

message(paste0("Top 10% account for ", round(100 * share_top10, 1), "% of interactions"))

# --- 7. Time dynamics --------------------------------------------------------
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

round(100 * engagement_table$share_used_app, 0)
round(100 * engagement_table$share_used_benchmark, 0)
round(100 * engagement_table$share_used_game, 0)

engagement_table$total_analysis
engagement_table$total_benchmark
engagement_table$total_game



# ---------------------------------------------------------------------------
# 13. Monthly App Engagement Table
# ---------------------------------------------------------------------------
message("Creating monthly app engagement table...")


# --- 1. Keep App households -------------------------------------------------
app_dt <- dt_hourly[group == "App" & !is.na(treat_date)]

# --- 2. Define month since treatment ----------------------------------------
app_dt[, month_since := interval(treat_date, date) %/% months(1)]

# Keep only post-treatment months
app_dt <- app_dt[month_since >= 0]

# --- 3. Clean engagement vars ------------------------------------------------
engagement_vars <- c("analysis", "benchmark", "game", "sessions")

for (v in engagement_vars) {
  if (v %in% names(app_dt)) {
    app_dt[is.na(get(v)), (v) := 0]
  } else {
    app_dt[, (v) := 0]
  }
}

# --- 4. Collapse to household × month ---------------------------------------
hh_month <- app_dt[, .(
  total_analysis  = sum(analysis),
  total_benchmark = sum(benchmark),
  total_game      = sum(game),
  total_sessions  = sum(sessions)
), by = .(household, month_since)]

# Define "active" = any interaction
hh_month[, active := as.integer(
  total_analysis + total_benchmark + total_game + total_sessions > 0
)]

# Feature-specific usage
hh_month[, `:=`(
  use_analysis  = as.integer(total_analysis  > 0),
  use_benchmark = as.integer(total_benchmark > 0),
  use_game      = as.integer(total_game      > 0)
)]

# Total usage intensity
hh_month[, total_usage := total_analysis + total_benchmark + total_game]

# --- 5. Aggregate to monthly table ------------------------------------------
monthly_table <- hh_month[, .(
  Total_Users = .N,
  Active_Users = sum(active),
  
  # Shares (in %)
  pct_info      = 100 * mean(use_analysis),
  pct_social    = 100 * mean(use_benchmark),
  pct_game      = 100 * mean(use_game),
  
  # Usage (conditional on active users)
  mean_usage = mean(total_usage[active == 1]),
  median_usage = median(total_usage[active == 1])
), by = month_since][order(month_since)]

print(monthly_table)


ggplot(monthly_table, aes(x = month_since, y = Active_Users / Total_Users)) +
  geom_line() +
  theme_minimal() +
  labs(
    x = "Months since treatment",
    y = "Share of active users"
  )

# --- 6. Export ---------------------------------------------------------------
fwrite(
  monthly_table,
  file.path(tab_dir, "monthly_app_engagement.csv")
)


# ---------------------------------------------------------------------------
# 12. App Engagement Analysis (Full Script)
# ---------------------------------------------------------------------------
message("Running app engagement analysis...")

# ---------------------------------------------------------------------------
# 1. Prepare data
# ---------------------------------------------------------------------------
app_dt <- copy(dt_hourly[group == "App"])

# Ensure date exists
app_dt[, date := as.Date(clock_local)]

# ---------------------------------------------------------------------------
# 2. Define treatment timing (staggered design)
# ---------------------------------------------------------------------------
treatment_dates <- data.table(
  tranche = c("Tranche 1", "Tranche 2", "Tranche 3"),
  treatment_start = as.Date(c("2017-06-06", "2017-09-19", "2017-11-20"))
)

app_dt <- merge(app_dt, treatment_dates, by = "tranche")

# Event time (clean version using calendar months)
app_dt[, month_since := 
         (year(date) - year(treatment_start)) * 12 +
         (month(date) - month(treatment_start))
]

# Keep post-treatment only
app_dt <- app_dt[month_since >= 0]

# ---------------------------------------------------------------------------
# 3. Engagement variables
# ---------------------------------------------------------------------------
engagement_vars <- intersect(
  c("sessions", "analysis", "benchmark", "game", "bets"),
  names(app_dt)
)

# Replace NA with 0
for (v in engagement_vars) {
  app_dt[is.na(get(v)), (v) := 0]
}

# Total engagement
app_dt[, app_engagement := 0]
for (v in engagement_vars) {
  app_dt[, app_engagement := app_engagement + get(v)]
}

# ---------------------------------------------------------------------------
# 4. Household-month panel
# ---------------------------------------------------------------------------
hh_month <- app_dt[, .(
  total_usage = sum(app_engagement, na.rm = TRUE),
  active      = as.integer(sum(app_engagement, na.rm = TRUE) > 0),
  
  # Feature usage
  info_usage   = sum(analysis,  na.rm = TRUE),
  social_usage = sum(benchmark, na.rm = TRUE),
  game_usage   = sum(game,      na.rm = TRUE)
  
), by = .(household, tranche, month_since)]

# ---------------------------------------------------------------------------
# 5. Fixed number of households per tranche
# ---------------------------------------------------------------------------
hh_tranche <- unique(app_dt[, .(household, tranche)])
N_tranche  <- hh_tranche[, .(Total = .N), by = tranche]

# ---------------------------------------------------------------------------
# 6. Monthly aggregation (core table)
# ---------------------------------------------------------------------------
monthly_tranche <- hh_month[, .(
  
  # Extensive margin
  Active = sum(active, na.rm = TRUE),
  
  # Total usage
  total_usage = sum(total_usage, na.rm = TRUE),
  
  # Conditional intensity
  mean_usage = if (sum(active) > 0) {
    mean(total_usage[active == 1], na.rm = TRUE)
  } else NA_real_,
  
  median_usage = if (sum(active) > 0) {
    median(total_usage[active == 1], na.rm = TRUE)
  } else NA_real_,
  
  # Feature totals
  info_total   = sum(info_usage,   na.rm = TRUE),
  social_total = sum(social_usage, na.rm = TRUE),
  game_total   = sum(game_usage,   na.rm = TRUE)
  
), by = .(tranche, month_since)]

# Merge totals
monthly_tranche <- merge(monthly_tranche, N_tranche, by = "tranche")

# Final metrics
monthly_tranche[, `:=`(
  active_share   = Active / Total,
  usage_per_user = total_usage / Total
)]

setorder(monthly_tranche, tranche, month_since)

# ---------------------------------------------------------------------------
# 7. Persistence (how many months households are active)
# ---------------------------------------------------------------------------
hh_persistence <- hh_month[, .(
  active_months = sum(active)
), by = household]

hh_persistence[, category := fifelse(
  active_months == 0, "Never",
  fifelse(active_months == 1, "1 month",
          fifelse(active_months <= 3, "2–3 months", "4+ months"))
)]

total_hh <- nrow(hh_persistence)

persistence_table <- hh_persistence[, .(
  share = .N / total_hh
), by = category]


# ---------------------------------------------------------------------------
# 8. Concentration (top users dominate?)
# ---------------------------------------------------------------------------
hh_total <- hh_month[, .(
  total_usage = sum(total_usage)
), by = household]

hh_total <- hh_total[order(-total_usage)]

top10_n <- ceiling(0.10 * nrow(hh_total))

top10_share <- hh_total[1:top10_n, sum(total_usage)] /
  sum(hh_total$total_usage)

# ---------------------------------------------------------------------------
# 9. Summary statistics
# ---------------------------------------------------------------------------
engagement_summary <- hh_month[, .(
  N_households        = uniqueN(household),
  share_active_ever   = mean(active > 0),
  mean_usage_active   = mean(total_usage[active == 1], na.rm = TRUE),
  median_usage_active = median(total_usage[active == 1], na.rm = TRUE)
)]

# ---------------------------------------------------------------------------
# 10. Save outputs
# ---------------------------------------------------------------------------
fwrite(monthly_tranche, file.path(tab_dir, "monthly_engagement_tranche.csv"))
fwrite(persistence_table, file.path(tab_dir, "engagement_persistence.csv"))
fwrite(engagement_summary, file.path(tab_dir, "engagement_summary.csv"))


hh_month[, `:=`(
  use_info   = as.integer(info_usage > 0),
  use_social = as.integer(social_usage > 0),
  use_game   = as.integer(game_usage > 0)
)]

monthly_usage <- hh_month[, .(
  share_any   = mean(active),
  share_info  = mean(use_info),
  share_social= mean(use_social),
  share_game  = mean(use_game)
), by = month_since][order(month_since)]


plot_dt <- melt(
  monthly_usage,
  id.vars = "month_since",
  measure.vars = c("share_info", "share_social", "share_game"),
  variable.name = "feature",
  value.name = "share"
)

plot_dt[, feature := fcase(
  feature == "share_info",   "Information",
  feature == "share_social", "Social comparison",
  feature == "share_game",   "Gamification"
)]

fig_engagement <- ggplot(plot_dt, aes(x = month_since, y = share, color = feature)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  scale_color_manual(values = wes_palette("Darjeeling1", 3, type = "discrete")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal() +
  labs(
    x = "Months since treatment",
    y = "Share",
    color = NULL
  ) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave(
  file.path(fig_dir, "app_engagement.pdf"),
  fig_engagement,
  width = 8,
  height = 5
)


### treatment and randomnization 

# Household-level data (same as for TableOne)
hh <- unique(dt_hourly[, .(
  household,
  group,
  tranche,
  
  # Treatment indicator
  treat = as.integer(group == "App"),
  
  # Covariates
  gas, district, biomass, oil, electric, heatPump,
  home_owned, apartment, singlefamily, splithouse,
  swimmingPool, sauna, airCondition, aquarium, dryer,
  waterBed, deepFreezers, computers, pev, ebike,
  numberofresidents, square_meter
)])

# Regression: treatment on observables + tranche FE
balance_reg <- feols(
  treat ~ 
    gas + district + biomass + oil + electric + heatPump +
    home_owned + apartment + singlefamily + splithouse +
    swimmingPool + sauna + airCondition + aquarium + dryer +
    waterBed + deepFreezers + computers + pev + ebike +
    numberofresidents + square_meter |
    tranche, vcov = "hetero",
  data = hh
)
etable(balance_reg)


wald(balance_reg)



# ---------------------------------------------------------------------------
# Pre-treatment summary statistics by tranche and group
# ---------------------------------------------------------------------------
message("Calculating pre-treatment summary statistics by tranche and group...")

# 1. Household-level totals and means in the pre-treatment period
hh_pre_stats <- dt_hourly_pre_tranche[, .(
  mean_kwh_pre  = mean(consumption, na.rm = TRUE),
  total_kwh_pre = sum(consumption, na.rm = TRUE),
  n_days_pre    = uniqueN(date),
  n_hours_pre   = .N
), by = .(household, tranche, group)]

# 2. Aggregate to tranche x group
pre_summary_tranche <- hh_pre_stats[, .(
  N_households        = .N,
  mean_kwh_hourly     = mean(mean_kwh_pre, na.rm = TRUE),
  sd_kwh_hourly       = sd(mean_kwh_pre, na.rm = TRUE),
  mean_total_kwh_pre  = mean(total_kwh_pre, na.rm = TRUE),
  sd_total_kwh_pre    = sd(total_kwh_pre, na.rm = TRUE),
  mean_days_pre       = mean(n_days_pre, na.rm = TRUE),
  mean_hours_pre      = mean(n_hours_pre, na.rm = TRUE)
), by = .(tranche, group)]

setorder(pre_summary_tranche, tranche, group)

print(pre_summary_tranche)

# Optional: export
fwrite(
  pre_summary_tranche,
  file.path(tab_dir, "pre_treatment_summary_by_tranche.csv")
)
# ---------------------------------------------------------------------------
# 13. Save filtered 15-min descriptive sample
# ---------------------------------------------------------------------------
saveRDS(
  dt_15,
  file.path(paths$data_processed, "dt_15min_finalsample.rds")
)

message("05_descriptive.R completed successfully.")
