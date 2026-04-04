###############################################################################
# 09_engagement_subsets_SunAbraham.R
###############################################################################
# ---------------------------------------------------------------------------
# 0. Setup
# ---------------------------------------------------------------------------
source(here::here("R", "00_setup.R"))
gc()

library(dplyr)
library(fixest)
library(lubridate)

message("Running 06_staggered_did_hourly.R ...")

# ---------------------------------------------------------------------------
# 1. Load data
# ---------------------------------------------------------------------------
dt_hourly <- readRDS(file.path(paths$data_processed, "dt_final.rds"))


# ---------------------------------------------------------------------------
# 2. Treatment + login indicators
# ---------------------------------------------------------------------------
dt_hourly[, app := as.integer(appgrp == 1)]
dt_hourly[, login := as.integer(analysis > 0 | benchmark > 0 | game > 0 |bets>0)]

# Force control group to have 0 logins (safety check)
dt_hourly[app == 0, login := 0]

# ---------------------------------------------------------------------------
# 3. Aggregate to DAILY
# ---------------------------------------------------------------------------
dt_daily <- dt_hourly[, .(
  consumption = sum(consumption, na.rm = TRUE),
  login = sum(login, na.rm = TRUE),
  app = max(app),
  post = max(post),
  Tranche1 = max(Tranche1),
  Tranche2 = max(Tranche2),
  Tranche3 = max(Tranche3)
), by = .(household, date)]

# ---------------------------------------------------------------------------
# 4. Define Engagement Groups (based on Post-Period activity)
# ---------------------------------------------------------------------------
usage <- dt_daily %>%
  filter(post == 1, app == 1) %>%
  group_by(household) %>%
  summarise(total_usage = sum(login, na.rm = TRUE), .groups = "drop")

# Define tertiles for Low/Medium/High
q <- quantile(usage$total_usage[usage$total_usage > 0], probs = c(0.33, 0.66), na.rm = TRUE)

usage <- usage %>%
  mutate(
    engagement = case_when(
      total_usage == 0 ~ "Never",
      total_usage <= q[1] ~ "Low",
      total_usage <= q[2] ~ "Medium",
      TRUE ~ "High"
    )
  )

table(usage$engagement)
# Merge back and clean up labels
dt_daily <- dt_daily %>%
  left_join(usage, by = "household") %>%
  mutate(
    engagement = case_when(
      app == 0 ~ "Control",
      is.na(engagement) ~ "Never", # Households in App group with no post-data
      TRUE ~ engagement
    )
  )
table(dt_daily$engagement)
# ---------------------------------------------------------------------------
# 5. Define Time and Cohorts for Sun & Abraham (2021)
# ---------------------------------------------------------------------------
min_date <- min(dt_daily$date)

dt_daily[, `:=`(
  day_id = as.integer(date - min_date) + 1,
  # Assign the real treatment date for everyone in the App group
  true_cohort = case_when(
    Tranche1 == 1 ~ as.integer(as.Date("2017-06-06") - min_date) + 1,
    Tranche2 == 1 ~ as.integer(as.Date("2017-09-19") - min_date) + 1,
    Tranche3 == 1 ~ as.integer(as.Date("2017-11-20") - min_date) + 1,
    TRUE ~ 0 # Control group
  )
)]

print(dt_daily[app == 1, .(HHs = uniqueN(household)), by = .(true_cohort)][order(true_cohort)])

# ---------------------------------------------------------------------------
# 6. Create Subsets and Regression-specific Cohorts
# ---------------------------------------------------------------------------
# CRITICAL: For sunab, we only want the target group to have a cohort > 0.
# The "Never" and "Control" groups act as the pooled baseline (cohort = 0).

prepare_sa_data <- function(data, level) {
  data %>%
    filter(engagement %in% c(level, "Never", "Control")) %>%
    mutate(reg_cohort = if_else(engagement == level, true_cohort, 0))
}

dt_high_sa   <- prepare_sa_data(dt_daily, "High")
dt_medium_sa <- prepare_sa_data(dt_daily, "Medium")
dt_low_sa    <- prepare_sa_data(dt_daily, "Low")


print(table(dt_high_sa$engagement, dt_high_sa$reg_cohort > 0))
print(table(dt_low_sa$engagement, dt_low_sa$reg_cohort > 0))
print(table(dt_medium_sa$engagement, dt_medium_sa$reg_cohort > 0))

# ---------------------------------------------------------------------------
# 7. Run Sun-Abraham Models
# --------------------------------------------------------------------------

m_high_sa <- feols(
  log(consumption + 1) ~ sunab(reg_cohort, day_id) | household + day_id,
  data = dt_high_sa,
  cluster = ~household
)

m_medium_sa <- feols(
  log(consumption + 1) ~ sunab(reg_cohort, day_id) | household + day_id,
  data = dt_medium_sa,
  cluster = ~household
)

m_low_sa <- feols(
  log(consumption + 1) ~ sunab(reg_cohort, day_id) | household + day_id,
  data = dt_low_sa,
  cluster = ~household
)

etable(m_high_sa, m_medium_sa, m_low_sa, agg = "ATT")


# ------------------------------------------------------------
# 1. Update the Function to include is_treated_group 🛠️
# ------------------------------------------------------------
prepare_subset <- function(data, level) {
  data %>%
    filter(engagement %in% c(level, "Never", "Control")) %>%
    mutate(
      is_treated_group = as.integer(engagement == level),
      reg_cohort = if_else(engagement == level, true_cohort, 0)
    )
}

dt_high_sub   <- prepare_subset(dt_daily, "High")
dt_medium_sub <- prepare_subset(dt_daily, "Medium")
dt_low_sub    <- prepare_subset(dt_daily, "Low")

# ------------------------------------------------------------
# 3. Run the OLS Models 📈
# ------------------------------------------------------------
m_high_ols <- feols(log(consumption + 1) ~ is_treated_group:post | household + day_id, 
                    data = dt_high_sub, cluster = ~household)

m_med_ols  <- feols(log(consumption + 1) ~ is_treated_group:post | household + day_id, 
                    data = dt_medium_sub, cluster = ~household)

m_low_ols  <- feols(log(consumption + 1) ~ is_treated_group:post | household + day_id, 
                    data = dt_low_sub, cluster = ~household)


etable(m_high_ols,m_med_ols,m_low_ols)


###############################################################################
# Pre-treatment differences by engagement (descriptive only)
###############################################################################
###############################################################################
# Table: Pre-treatment summary by engagement and tranche
###############################################################################

library(dplyr)

# ---------------------------------------------------------------------------
# 1. Pre-treatment sample
# ---------------------------------------------------------------------------
dt_pre <- dt_daily %>%
  filter(post == 0)

# ---------------------------------------------------------------------------
# 2. Define tranche
# ---------------------------------------------------------------------------
dt_pre <- dt_pre %>%
  mutate(
    tranche = case_when(
      Tranche1 == 1 ~ "Tranche 1",
      Tranche2 == 1 ~ "Tranche 2",
      Tranche3 == 1 ~ "Tranche 3",
      TRUE ~ "Control"
    )
  )

# ---------------------------------------------------------------------------
# 3. Function to compute summary stats
# ---------------------------------------------------------------------------
make_summary <- function(data, group_name) {
  
  data %>%
    filter(engagement %in% c(group_name, "Never", "Control")) %>%
    mutate(group = if_else(engagement == group_name, group_name, "Baseline")) %>%
    group_by(tranche, group) %>%
    summarise(
      total_consumption = sum(consumption, na.rm = TRUE),
      mean_consumption  = mean(consumption, na.rm = TRUE),
      sd_consumption    = sd(consumption, na.rm = TRUE),
      se_consumption    = sd(consumption, na.rm = TRUE) / sqrt(n()),
      N_obs             = n(),
      N_households      = n_distinct(household),
      N_days            = n_distinct(date),
      .groups = "drop"
    )
}

# ---------------------------------------------------------------------------
# 4. Create summaries
# ---------------------------------------------------------------------------
tab_high   <- make_summary(dt_pre, "High")   %>% mutate(sample = "High vs Baseline")
tab_medium <- make_summary(dt_pre, "Medium") %>% mutate(sample = "Medium vs Baseline")
tab_low    <- make_summary(dt_pre, "Low")    %>% mutate(sample = "Low vs Baseline")

# Combine
table_final <- bind_rows(tab_high, tab_medium, tab_low)

# Order nicely
table_final <- table_final %>%
  arrange(sample, tranche, group)

print(table_final)


