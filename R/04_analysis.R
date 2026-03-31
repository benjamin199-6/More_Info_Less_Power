# --- Step 1: Prep Treatment & Time IDs ---
source(here::here("R", "00_setup.R"))
library(zoo); library(fixest); library(data.table); library(lubridate)
gc()

dt_hourly <- readRDS(file.path(paths$data_processed, "dt_final.rds"))
setDT(dt_hourly)

# 1.1 Define Rolling Tranche Dates
tranche_dates <- list(
  t1 = as.Date("2017-06-06"), 
  t2 = as.Date("2017-09-19"), 
  t3 = as.Date("2017-11-20")
)

# 1.2 Assign Treatment Date based on Tranches
dt_hourly[, treat_date := fcase(
  Tranche1 == 1, tranche_dates$t1, 
  Tranche2 == 1, tranche_dates$t2,
  Tranche3 == 1, tranche_dates$t3, 
  default = as.Date(NA)
)]

# 1.3 Create the Global Time IDs (Month and Week)
min_date_val <- min(dt_hourly$date)
dt_hourly[, time_id := as.integer(round((as.yearmon(date) - as.yearmon(min_date_val)) * 12)) + 1]
dt_hourly[, week_id := as.integer(difftime(date, min_date_val, units = "weeks")) + 1]

# 1.4 Create Cohort IDs (Month and Week) for Sun-Abraham
# Control group (appgrp == 0) must be 0
dt_hourly[, cohort_id := 0]
dt_hourly[appgrp == 1, cohort_id := as.integer(round((as.yearmon(treat_date) - as.yearmon(min_date_val)) * 12)) + 1]

dt_hourly[, cohort_id_week := 0]
dt_hourly[appgrp == 1, cohort_id_week := as.integer(difftime(treat_date, min_date_val, units = "weeks")) + 1]

# VERIFY: Do you see 0, 2, 5, 7?
print("Monthly Cohorts (0=Control):")
print(table(dt_hourly$cohort_id))



# --- Step 2: Aggregate to Weekly & Monthly ---

# 2.1 Weekly Aggregation
dt_weekly <- dt_hourly[, .(
  consumption = mean(consumption, na.rm = TRUE), # Mean fixes variable week length spikes
  appgrp = max(appgrp), 
  post = max(post),
  cohort_id_week = max(cohort_id_week)
), by = .(household, week_id)]

# 2.2 Monthly Aggregation
dt_monthly <- dt_hourly[, .(
  consumption = mean(consumption, na.rm = TRUE),
  appgrp = max(appgrp), 
  post = max(post),
  cohort_id = max(cohort_id)
), by = .(household, time_id)]

# 2.3 Create Subset for 9-Month Analysis (without overwriting original)
dt_hourly_9mo  <- dt_hourly[time_id <= 9]
dt_weekly_9mo  <- dt_weekly[week_id <= 39]
dt_monthly_9mo <- dt_monthly[time_id <= 9]

message("Datasets prepared: dt_hourly, dt_weekly, dt_monthly, and 9-month subsets.")



# --- Step 3: Hourly Analysis (Full Period) ---

# Log Model (Percent change)
m_hourly_log <- feols(log1p(consumption) ~ sunab(cohort_id, time_id, ref.c = 0) | date + hour + household, dt_hourly, cluster = ~household)

# Level Model (kWh change)
m_hourly_kwh <- feols(consumption ~ sunab(cohort_id, time_id, ref.c = 0) | date + hour + household, dt_hourly, cluster = ~household)

# Compare
iplot(m_hourly_log, main = "Hourly Log Effects")
iplot(m_hourly_kwh, main = "Hourly kWh Effects")


# --- Step 4: Weekly Sun-Abraham Models ---

# 4.1 Weekly Log Model (Percentage Impact)
m_weekly_log <- feols(consumption ~ sunab(cohort_id_week, week_id, ref.c = 0) | week_id + household, 
                      data = dt_weekly, cluster = ~household)

# 4.2 Weekly Level Model (kWh Impact)
m_weekly_kwh <- feols(consumption ~ sunab(cohort_id_week, week_id, ref.c = 0) | week_id + household, 
                      data = dt_weekly, cluster = ~household)

# 4.3 Summary of Average Treatment Effect (ATT)
message("Weekly ATT (Log):")
print(summary(m_weekly_log, agg = "ATT"))

# 4.4 Plot Results (Should be smooth now with mean aggregation)
par(mfrow = c(1, 2))
iplot(m_weekly_log, main = "Weekly Effects (log)")
iplot(m_weekly_kwh, main = "Weekly Effects (kWh)")
par(mfrow = c(1, 1))


# --- Step 5: 9-Month Subset (Monthly & Weekly) ---

# 5.1 Monthly Log Model (9-Month Subset)
m_monthly_9mo_log <- feols(log1p(consumption) ~ sunab(cohort_id, time_id, ref.c = 0) | time_id + household, 
                           data = dt_monthly_9mo, cluster = ~household)

# 5.2 Weekly Log Model (9-Month Subset)
m_weekly_9mo_log <- feols(log1p(consumption) ~ sunab(cohort_id_week, week_id, ref.c = 0) | week_id + household, 
                          data = dt_weekly_9mo, cluster = ~household)

# Plot comparison
par(mfrow = c(1, 2))
iplot(m_monthly_9mo_log, main = "Monthly 9-Mo (log)")
iplot(m_weekly_9mo_log, main = "Weekly 9-Mo (log)")
par(mfrow = c(1, 1))



# --- Step 6: Bias Check (TWFE vs Sun-Abraham) ---

# 6.1 Standard TWFE Interaction (Biased)
# We use i() for the standard dynamic plot
m_weekly_twfe <- feols(consumption ~ i(week_id, appgrp, ref=1) | week_id + household, 
                       data = dt_weekly, cluster = ~household)

# 6.2 Side-by-Side Plotting
# This visually demonstrates the correction
par(mfrow = c(1, 2))
iplot(m_weekly_twfe, main = "Standard TWFE (Biased)")
iplot(m_weekly_log, main = "Sun-Abraham (Unbiased)")
par(mfrow = c(1, 1))

# 6.3 Compare ATT estimates
etable(m_weekly_twfe, m_weekly_log, 
       headers = c("TWFE", "Sun-Abraham"),
       title = "Comparison of Staggered DiD Estimators")


# --- Step 6.4: Combined Visualization (TWFE vs. Sun-Abraham) ---

# We pass both models to iplot to see them on the same axis
# 'pt.pch' sets different point shapes, 'col' sets colors
iplot(list(m_weekly_twfe, m_weekly_log),
      main = "Comparison: Biased TWFE vs. Unbiased Sun-Abraham",
      xlab = "Weeks Relative to Treatment",
      ylab = "Estimate (log points)",
      sep = 0.2,            # Adds a small horizontal offset so points don't overlap
      pt.join = TRUE,       # Connects the dots for each model
      col = c("gray70", "royalblue"), # Gray for biased, Blue for unbiased
      pch = c(16, 17),      # Circle for TWFE, Triangle for Sun-Abraham
      ref.line = TRUE)      # Adds the horizontal line at 0

# Add a legend to make it clear which is which
legend("bottom", 
       legend = c("Standard TWFE (Biased)", "Sun-Abraham (Unbiased)"), 
       col = c("gray70", "royalblue"), 
       pch = c(16, 17), 
       bty = "n") # 'n' removes the box around the legend




# --- Step 2.5: Daily Aggregation (Dual Outcomes) ---

dt_daily <- dt_hourly[, .(
  # 1. Raw Sum: Total kWh recorded in that day
  consumption_sum = sum(consumption, na.rm = TRUE),
  
  # 2. Standardized Day: Mean hourly load * 24
  consumption_std = mean(consumption, na.rm = TRUE) * 24,
  
  # Quality metrics
  n_hours = .N,
  
  appgrp = max(appgrp), 
  post = max(post),
  treat_date = max(treat_date)
), by = .(household, date)]

# Create IDs
min_date_val <- min(dt_daily$date)
dt_daily[, day_id := as.integer(difftime(date, min_date_val, units = "days")) + 1]
dt_daily[, cohort_id_day := 0]
dt_daily[appgrp == 1, cohort_id_day := as.integer(difftime(treat_date, min_date_val, units = "days")) + 1]

# Log-transform both for comparison
dt_daily[, `:=`(
  log_sum = log1p(consumption_sum),
  log_std = log1p(consumption_std)
)]

# Keep only days with at least 18 hours for the 'Sum' to be meaningful
dt_daily <- dt_daily[n_hours >= 18]

message("Daily Dual-Outcome Data Ready.")


# --- Step 4.E: Sun-Abraham Comparison (Sum vs Standardized) ---

# 1. Model using Raw Sum
m_daily_sunab_sum <- feols(log_sum ~ sunab(cohort_id_day, day_id, ref.c = 0, 
                                           bin = list("Post" = 30:500)) | day_id + household, 
                           data = dt_daily, cluster = ~household)

# 2. Model using Standardized (Mean * 24)
m_daily_sunab_std <- feols(log_std ~ sunab(cohort_id_day, day_id, ref.c = 0, 
                                           bin = list("Post" = 30:500)) | day_id + household, 
                           data = dt_daily, cluster = ~household)

# 3. Combined Visualization
# Gray = Raw Sum, Firebrick = Standardized
iplot(list(m_daily_sunab_sum, m_daily_sunab_std),
      main = "Daily Aggregation Check: Sum vs. Standardized",
      xlab = "Days Relative to App Receipt",
      ylab = "Estimate (log points)",
      col = c("gray70", "firebrick"), 
      pt.join = TRUE,
      pt.pch = c(16, 17),
      ref.line = TRUE,
      sep = 0.1)

legend("topright", 
       legend = c("Raw Sum (Sensitive to missing hours)", "Standardized (Mean * 24)"), 
       col = c("gray70", "firebrick"), 
       pch = c(16, 17), 
       bty = "n")

# Compare ATT estimates
etable(m_daily_sunab_sum, m_daily_sunab_std, 
       headers = c("Raw Sum", "Standardized (Mean*24)"),
       title = "Comparison of Aggregation Methods")


# --- Step 2.6: Daily Aggregation with all IDs and Seasonality ---
library(lubridate) # Ensure lubridate is active

dt_daily <- dt_hourly[, .(
  # Standardized Daily Total (Mean * 24)
  consumption_std = mean(consumption, na.rm = TRUE) * 24,
  
  # Metadata and Grouping IDs
  appgrp = max(appgrp), 
  post = max(post),
  treat_date = max(treat_date),
  
  # Ensure Sun-Abraham Cohort IDs are carried over
  cohort_id = max(cohort_id),           
  cohort_id_week = max(cohort_id_week)
), by = .(household, date)]

# 1. Create the Day-of-Week (Explicitly use lubridate::wday)
dt_daily[, dow := lubridate::wday(date, label = TRUE)]

# 2. Create the Time IDs and Outcomes
min_date_val <- min(dt_daily$date)
dt_daily[, `:=`(
  day_id = as.integer(difftime(date, min_date_val, units = "days")) + 1,
  week_id = as.integer(difftime(date, min_date_val, units = "weeks")) + 1,
  time_id = as.integer(round((as.yearmon(date) - as.yearmon(min_date_val)) * 12)) + 1,
  log_std = log1p(consumption_std)
)]

# 3. Create Relative Week for the TWFE comparison
dt_daily[, rel_week := as.integer(floor(as.numeric(difftime(date, treat_date, units = "days")) / 7))]

message("Daily data fixed. 'dow' and 'cohort_id_week' are now available.")
## --- Step 4.F: Daily Observations, Weekly Treatment Effects ---

# Sun-Abraham (Unbiased)
m_daily_weekly_sunab <- feols(
  log_std ~ sunab(cohort_id_week, week_id, ref.c = 0, bin = list("Post" = 20:60)) | 
    week_id + household + dow, 
  data = dt_daily, 
  cluster = ~household
)

# Standard TWFE (Biased Comparison)
m_daily_weekly_twfe <- feols(
  log_std ~ i(rel_week, appgrp, ref = -1) | week_id + household + dow, 
  data = dt_daily[rel_week >= -10 & rel_week <= 30], 
  cluster = ~household
)

# Combined Plot: Weekly dynamics using daily resolution
iplot(list(m_daily_weekly_twfe, m_daily_weekly_sunab),
      main = "Daily Data: Weekly Effects (with Day-of-Week FE)",
      xlab = "Weeks Since Treatment",
      col = c("gray70", "royalblue"),
      pt.join = TRUE,
      ref.line = TRUE,
      sep = 0.1)

legend("topright", legend = c("TWFE (Daily Obs)", "Sun-Abraham (Daily Obs)"), 
       col = c("gray70", "royalblue"), pch = c(16, 17), bty = "n")

# Single Headline Estimate
summary(m_daily_weekly_sunab, agg = "ATT")


# next


# --- Step 1.4 (Revised): Create all Cohort IDs in Hourly Data ---

# Reference point for all time-series (Day 1 of study)
min_date_study <- min(dt_hourly$date)

# Create Daily Cohort: The day number (1, 2, 3...) when treatment started
dt_hourly[, cohort_id_day := 0]
dt_hourly[appgrp == 1, cohort_id_day := as.integer(difftime(treat_date, min_date_study, units = "days")) + 1]

# Create Weekly Cohort: The week number when treatment started
dt_hourly[, cohort_id_week := 0]
dt_hourly[appgrp == 1, cohort_id_week := as.integer(difftime(treat_date, min_date_study, units = "weeks")) + 1]

# Create Monthly Cohort: The month number when treatment started
dt_hourly[, cohort_id := 0]
dt_hourly[appgrp == 1, cohort_id := as.integer(round((as.yearmon(treat_date) - as.yearmon(min_date_study)) * 12)) + 1]

message("Cohort IDs (Day, Week, Month) defined in dt_hourly.")

# --- Step 2.6 (Revised): Aggregate to Daily ---
library(lubridate)

dt_daily <- dt_hourly[, .(
  consumption_std = mean(consumption, na.rm = TRUE) * 24, # Standardized Day
  appgrp = max(appgrp), 
  post = max(post),
  treat_date = max(treat_date),
  
  # Pull the IDs we just created
  cohort_id_day = max(cohort_id_day),
  cohort_id_week = max(cohort_id_week),
  cohort_id = max(cohort_id)
), by = .(household, date)]

# Create the Daily Time IDs
dt_daily[, `:=`(
  day_id = as.integer(difftime(date, min_date_study, units = "days")) + 1,
  dow = lubridate::wday(date, label = TRUE),
  log_std = log1p(consumption_std)
)]

message("dt_daily is now fully populated with cohort_id_day.")

# --- Step 4.G: Robust Daily Sun-Abraham ---

# 1. Run the model without manual binning to avoid "value not found" errors
# This estimates every single relative day. 
aily_sunab <- feols(
  log_std ~ sunab(cohort_id_day, day_id, ref.c = 0) | 
    day_id + household + dow, 
  data = dt_daily, 
  cluster = ~household
)

# 2. Focus the plot on the relevant window (+/- 60 days)
# 'iplot' handles the windowing for us visually
iplot(m_daily_sunab, 
      xlim = c(-60, 60), 
      main = "Daily Treatment Effects (Focus: +/- 60 Days)",
      xlab = "Days Relative to Treatment",
      ylab = "Estimate (log points)",
      pt.join = TRUE, 
      ref.line = TRUE)

# 3. View the headline result
message("ATT Estimate (Daily):")
summary(m_daily_sunab, agg = "ATT")
