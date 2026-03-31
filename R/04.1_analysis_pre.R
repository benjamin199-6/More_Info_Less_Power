###############################################################################
# 04_analysis_prep.R
###############################################################################

# ---------------------------------------------------------------------------
# 0. Setup
# ---------------------------------------------------------------------------
source(here::here("R", "00_setup.R"))
gc()

library(data.table)
library(ggplot2)
library(wesanderson)

message("Running 04_analysis_prep.R ...")

dt <- readRDS(file.path(paths$data_processed, "dt_analysis.rds"))
setDT(dt)

paths$figures <- file.path(paths$output_figures)
dir.create(paths$figures, showWarnings = FALSE, recursive = TRUE)

# ---------------------------------------------------------------------------
# Tracking frameworks
# ---------------------------------------------------------------------------

track_sample <- function(dt, step_name) {
  dt[, .(
    N_households = uniqueN(household),
    N_obs        = .N
  ), by = group][, step := step_name]
}

run_selection_test <- function(data, var, step_name) {
  data[, group := relevel(factor(group), ref = "App")]
  
  model <- glm(
    as.formula(paste0(var, " ~ group")),
    data   = data,
    family = binomial
  )
  
  res <- as.data.table(summary(model)$coefficients, keep.rownames = "term")
  
  res[, `:=`(
    step       = step_name,
    odds_ratio = exp(Estimate),
    N          = nrow(data)
  )]
  
  return(res)
}

plot_zero_days <- function(dt, consumption_var, label, filename) {
  daily <- dt[, .(
    total_daily_consumption = sum(get(consumption_var), na.rm = TRUE)
  ), by = .(date, household)]
  
  freq <- daily[total_daily_consumption <= 0,
                .(frequency = .N),
                by = date][order(date)]
  
  p <- ggplot(freq, aes(date, frequency)) +
    geom_col(fill = "black") +
    theme_minimal() +
    labs(title = label, x = "Date", y = "Zero-consumption households") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  ggsave(file.path(paths$figures, filename), p, width = 8, height = 5)
  
  return(freq)
}

sample_tracking    <- list()
selection_tracking <- list()

# ---------------------------------------------------------------------------
# 1. Time variables
# ---------------------------------------------------------------------------
dt[, datetime := as.POSIXct(paste(year, month, day, hour), format = "%Y %m %d %H")]
dt[, date := as.Date(datetime)]

# ---------------------------------------------------------------------------
# 2. Treatment groups
# ---------------------------------------------------------------------------
dt[, group := fcase(
  controlgrp == 1, "Control",
  appgrp == 1, "App",
  tariffgrp == 1, "App + Discount"
)]

dt[, group := factor(group)]
hh_group <- unique(dt[, .(household, group)])

sample_tracking[[1]] <- track_sample(dt, "1. Initial sample")

# ---------------------------------------------------------------------------
# 3. Post-treatment
# ---------------------------------------------------------------------------
tranche_dates <- list(
  tranche1 = as.Date("2017-06-06"),
  tranche2 = as.Date("2017-09-19"),
  tranche3 = as.Date("2017-11-20")
)

dt[, post := fcase(
  Tranche1 == 1 & date >= tranche_dates$tranche1, 1,
  Tranche2 == 1 & date >= tranche_dates$tranche2, 1,
  Tranche3 == 1 & date >= tranche_dates$tranche3, 1,
  default = 0
)]

# ---------------------------------------------------------------------------
# 4. Cleaning pipeline
# ---------------------------------------------------------------------------

# ---- 4.1 Remove PV ---------------------------------------------------------
hh_pv <- unique(dt[, .(household, pv)])
hh_pv <- hh_group[hh_pv, on = "household"]
hh_pv[, removed := as.integer(pv == 1)]

selection_tracking[[1]] <- run_selection_test(hh_pv, "removed", "Remove PV")

dt <- dt[pv != 1]
sample_tracking[[2]] <- track_sample(dt, "2. Remove PV")

# ---- 4.2 Keep Control vs App -----------------------------------------------
dt <- dt[group %in% c("Control", "App")]
sample_tracking[[3]] <- track_sample(dt, "3. Keep Control vs App")

# ---- 4.3 Coverage ----------------------------------------------------------
hh_cov <- dt[, .(
  first = min(datetime),
  last  = max(datetime),
  obs   = .N
), by = household]

hh_cov[, possible := as.numeric(difftime(last, first, units = "hours")) + 1]
hh_cov[, coverage := obs / possible]

hh_cov <- hh_group[hh_cov, on = "household"]
hh_cov[, removed := as.integer(coverage <= 0.9)]

selection_tracking[[2]] <- run_selection_test(hh_cov, "removed", "Coverage filter")

dt <- dt[household %in% hh_cov[coverage > 0.9, household]]
sample_tracking[[4]] <- track_sample(dt, "4. Coverage > 90%")

# ---- 4.4 Remove bad days ---------------------------------------------------
zero_before <- plot_zero_days(dt, "consumption",
                              "Before removing bad days",
                              "zeros_before_cleaning.pdf")

bad_days <- zero_before[frequency > 70, date]
dt <- dt[!date %in% bad_days]

sample_tracking[[5]] <- track_sample(dt, "5. Remove bad days")

# ---- 4.5 Clean zeros -------------------------------------------------------
dt[, consumption_all := consumption]
dt[, consumption := fifelse(consumption == 0, NA_real_, consumption)]

sample_tracking[[6]] <- track_sample(dt, "6. Replace zeros")

plot_zero_days(dt, "consumption",
               "After cleaning",
               "zeros_after_cleaning.pdf")

# ---------------------------------------------------------------------------
# 4.6 Outlier detection (dual approach)
# ---------------------------------------------------------------------------
message("Identifying outliers...")

pre <- dt[post == 0]

# --- LEVEL OUTLIERS (main)
hh_mean <- pre[, .(
  mean_cons = mean(consumption, na.rm = TRUE)
), by = household]

hh_mean <- hh_group[hh_mean, on = "household"]

mu  <- mean(hh_mean$mean_cons, na.rm = TRUE)
sd_ <- sd(hh_mean$mean_cons, na.rm = TRUE)

outliers_level <- hh_mean[
  abs(mean_cons - mu) > 3 * sd_,
  household
]

# --- PATTERN OUTLIERS
hh_hour <- pre[, .(
  hourly_cons = mean(consumption, na.rm = TRUE)
), by = .(household, hour)]

hour_ref <- hh_hour[, .(
  mu_hour = mean(hourly_cons, na.rm = TRUE),
  sd_hour = sd(hourly_cons, na.rm = TRUE)
), by = hour]

hh_hour <- hour_ref[hh_hour, on = "hour"]
hh_hour[, z := (hourly_cons - mu_hour) / sd_hour]

outliers_pattern <- hh_hour[
  abs(z) > 3,
  .N,
  by = household
][N > 3, household]

outliers_all <- unique(c(outliers_level, outliers_pattern))

# --- TRACKING
hh_mean[, removed_level := as.integer(household %in% outliers_level)]
hh_mean[, removed_all   := as.integer(household %in% outliers_all)]

selection_tracking[[3]] <- run_selection_test(
  hh_mean, "removed_level", "Outliers (level)"
)

selection_tracking[[4]] <- run_selection_test(
  hh_mean, "removed_all", "Outliers (level + pattern)"
)

# ---------------------------------------------------------------------------
# 4.7 Create TWO datasets
# ---------------------------------------------------------------------------

dt_main   <- dt[!household %in% outliers_level]
dt_main %>% group_by(group) %>% summarise(n_distinct(household))
dt_robust%>% group_by(group) %>% summarise(n_distinct(household))
dt_robust <- dt[!household %in% outliers_all]

sample_tracking[[7]] <- track_sample(dt_main, "7. Remove outliers (level)")

# Continue pipeline ONLY with main sample
dt <- copy(dt_main)

# ---- 4.8 Balanced pre-period -----------------------------------------------
dt <- dt[
  (Tranche1 == 1 & date >= tranche_dates$tranche1 - 20) |
    (Tranche2 == 1 & date >= tranche_dates$tranche2 - 7) |
    (Tranche3 == 1 & date >= tranche_dates$tranche3 - 12)
]

sample_tracking[[8]] <- track_sample(dt, "8. Balanced pre-period")

# ---------------------------------------------------------------------------
# 5. Fixed effects
# ---------------------------------------------------------------------------
dt[, dow := factor(weekdays(date),
                   levels = c("Monday","Tuesday","Wednesday",
                              "Thursday","Friday","Saturday","Sunday"))]

dt[, `:=`(
  month = factor(month),
  year  = factor(year)
)]

# ---------------------------------------------------------------------------
# 6. Save tracking tables
# ---------------------------------------------------------------------------
sample_table    <- rbindlist(sample_tracking)
selection_table <- rbindlist(selection_tracking)

print(sample_table)
print(selection_table)

# ---------------------------------------------------------------------------
# 7. Save datasets
# ---------------------------------------------------------------------------

saveRDS(dt,         file.path(paths$data_processed, "dt_final.rds"))        # main
saveRDS(dt_robust,  file.path(paths$data_processed, "dt_final_robust.rds")) # robustness

message("04_analysis_prep.R completed successfully.")