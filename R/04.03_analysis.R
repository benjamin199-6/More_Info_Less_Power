###############################################################################
# 06_staggered_did_hourly.R
#
# Purpose:
# - Estimate staggered DiD using hourly data
# - Main estimator: Sun & Abraham (kWh)
# - Robustness: TWFE (kWh)
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
# 2. Define treatment dates (by tranche)
# ---------------------------------------------------------------------------
dt_hourly <- dt_hourly %>%
  mutate(
    treat_date = case_when(
      Tranche1 == 1 ~ as.Date("2017-06-06"),
      Tranche2 == 1 ~ as.Date("2017-09-19"),
      Tranche3 == 1 ~ as.Date("2017-11-20"),
      TRUE ~ as.Date(NA)
    ),
    treat_date = if_else(group == "Control", as.Date(NA), treat_date)
  )

table(dt_hourly$treat_date,dt_hourly$group)

# ---------------------------------------------------------------------------
# 3. Define treatment indicator (TWFE)
# ---------------------------------------------------------------------------
dt_hourly <- dt_hourly %>%
  mutate(
    treated = if_else(group == "App" & date >= treat_date, 1, 0)
  )


table(dt_hourly$treated[dt_hourly$group == "Control"])
table(dt_hourly$treated[dt_hourly$group == "App"])
# ---------------------------------------------------------------------------
# 4. Define time variables (for Sun-Abraham)
# ---------------------------------------------------------------------------
min_date <- min(dt_hourly$date, na.rm = TRUE)

dt_hourly <- dt_hourly %>%
  mutate(
    day_id = as.integer(date - min_date) + 1,
    cohort_id_day = case_when(
      group == "App" ~ as.integer(treat_date - min_date) + 1,
      TRUE ~ 0
    )
  )


dt_daily <- dt_hourly %>%
  group_by(household, date) %>%
  summarise(
    consumption = sum(consumption, na.rm = TRUE),
    group = first(group),
    treat_date = first(treat_date),
    .groups = "drop"
  )

min_date <- min(dt_daily$date)

dt_daily <- dt_daily %>%
  mutate(
    treated = if_else(group == "App" & date >= treat_date, 1, 0),
    day_id = as.integer(date - min_date) + 1,
    cohort_id_day = case_when(
      group == "App" ~ as.integer(treat_date - min_date) + 1,
      TRUE ~ 0
    )
  )

m_sunab <- feols(
  consumption ~ sunab(cohort_id_day, day_id, ref.c = 0) |
    household + day_id,
  data = dt_daily,
  cluster = ~household
)

att_sunab <- summary(m_sunab, agg = "ATT")
print(att_sunab)


# ---------------------------------------------------------------------------
# TWFE (daily data)
# ---------------------------------------------------------------------------

m_twfe_daily <- feols(
  consumption ~ treated |
    household + date,
  data = dt_daily,
  cluster = ~household
)

summary(m_twfe_daily)

# ---------------------------------------------------------------------------
# 6. TWFE with hourly data as robustness
# ---------------------------------------------------------------------------

m_twfe <- feols(
  consumption ~ treated |
    household + day_id,
  data = dt_hourly,
  cluster = ~household
)

summary(m_twfe)


### weekly fixed effect 


dt_daily <- dt_daily %>%
  mutate(
    week_id = floor(day_id / 7),
    cohort_week = floor(cohort_id_day / 7)
  )


m_sunab_week <- feols(
  consumption ~ sunab(cohort_week, week_id, ref.c = 0) |
    household + week_id,
  data = dt_daily,
  cluster = ~household
)

att_sunab_week <- summary(m_sunab_week, agg = "ATT")
print(att_sunab_week)


# ===========================================================================
# EXTENSIONS: FLEXIBLE VS NON-FLEXIBLE HOURS
# ===========================================================================

# Flexible hours: 07:00-22
dt_hourly <- dt_hourly %>%
  mutate(
    flexible_hour = if_else(hour %in% 7:22, 1L, 0L)
  )

print(table(dt_hourly$flexible_hour))

print(
  dt_hourly %>%
    group_by(hour) %>%
    summarise(mean_kwh = mean(consumption, na.rm = TRUE), .groups = "drop")
)

# ---------------------------------------------------------------------------
# Aggregate daily totals separately for flexible and non-flexible hours
# ---------------------------------------------------------------------------
dt_daily_flex <- dt_hourly %>%
  filter(flexible_hour == 1) %>%
  group_by(household, date) %>%
  summarise(
    consumption = sum(consumption, na.rm = TRUE),
    group = first(group),
    treat_date = first(treat_date),
    .groups = "drop"
  ) %>%
  mutate(
    treated = if_else(group == "App" & date >= treat_date, 1, 0),
    day_id = as.integer(date - min_date) + 1,
    cohort_id_day = case_when(
      group == "App" ~ as.integer(treat_date - min_date) + 1,
      TRUE ~ 0
    ),
    week_id = floor(day_id / 7),
    cohort_week = floor(cohort_id_day / 7)
  )

dt_daily_inflex <- dt_hourly %>%
  filter(flexible_hour == 0) %>%
  group_by(household, date) %>%
  summarise(
    consumption = sum(consumption, na.rm = TRUE),
    group = first(group),
    treat_date = first(treat_date),
    .groups = "drop"
  ) %>%
  mutate(
    treated = if_else(group == "App" & date >= treat_date, 1, 0),
    day_id = as.integer(date - min_date) + 1,
    cohort_id_day = case_when(
      group == "App" ~ as.integer(treat_date - min_date) + 1,
      TRUE ~ 0
    ),
    week_id = floor(day_id / 7),
    cohort_week = floor(cohort_id_day / 7)
  )

print(c(
  daily_all = nrow(dt_daily),
  daily_flex = nrow(dt_daily_flex),
  daily_inflex = nrow(dt_daily_inflex)
))

print(c(
  hh_all = n_distinct(dt_daily$household),
  hh_flex = n_distinct(dt_daily_flex$household),
  hh_inflex = n_distinct(dt_daily_inflex$household)
))

# ---------------------------------------------------------------------------
# Weekly Sun-Abraham + TWFE: flexible hours
# ---------------------------------------------------------------------------
m_sunab_week_flex <- feols(
  consumption ~ sunab(cohort_week, week_id, ref.c = 0) |
    household + week_id,
  data = dt_daily_flex,
  cluster = ~household
)


att_sunab_week_flex <- summary(m_sunab_week_flex, agg = "ATT")
print(att_sunab_week_flex)

m_twfe_week_flex <- feols(
  consumption ~ treated |
    household + week_id,
  data = dt_daily_flex,
  cluster = ~household
)

sum_twfe_week_flex <- summary(m_twfe_week_flex)
print(sum_twfe_week_flex)

# ---------------------------------------------------------------------------
# Weekly Sun-Abraham + TWFE: non-flexible hours
# ---------------------------------------------------------------------------
m_sunab_week_inflex <- feols(
  consumption ~ sunab(cohort_week, week_id, ref.c = 0) |
    household + week_id,
  data = dt_daily_inflex,
  cluster = ~household
)

att_sunab_week_inflex <- summary(m_sunab_week_inflex, agg = "ATT")
print(att_sunab_week_inflex)

m_twfe_week_inflex <- feols(
  consumption ~ treated |
    household + week_id,
  data = dt_daily_inflex,
  cluster = ~household
)

sum_twfe_week_inflex <- summary(m_twfe_week_inflex)
print(sum_twfe_week_inflex)


# ---------------------------------------------------------------------------
# Weekly TWFE 
# ---------------------------------------------------------------------------
dt_daily <- dt_daily %>%
  mutate(
    week_id = floor(day_id / 7),
    cohort_week = floor(cohort_id_day / 7)
  )



m_twfe_week <- feols(
  consumption ~ treated |
    household + week_id,
  data = dt_daily,
  cluster = ~household
)

sum_twfe_week <- summary(m_twfe_week)
print(sum_twfe_week)

# ---------------------------------------------------------------------------
# COMBINED TABLE: TWFE + SUN-ABRAHAM ATT
# ---------------------------------------------------------------------------

# TWFE models together
etable(
  m_twfe_daily,
  m_twfe,              # hourly
  m_twfe_week,
  m_twfe_week_flex,
  m_twfe_week_inflex
)

# Sun-Abraham ATT outputs (printed separately)
print(att_sunab)
print(att_sunab_week)
print(att_sunab_week_flex)
print(att_sunab_week_inflex)

mean_weekly_pre <- mean(dt_daily$consumption[dt_daily$treated == 0])
att_week <- -0.4736 ## att from att_sunab_week
pct_effect <- 100 * att_week / mean_weekly_pre

m_sunab_week_log <- feols(
  log(consumption) ~ sunab(cohort_week, week_id, ref.c = 0) |
    household + week_id,
  data = dt_daily,
  cluster = ~household
)

att_sunab_week_log <- summary(m_sunab_week_log, agg = "ATT")
print(att_sunab_week)
pct_effect

### Event Study 
m_sunab_dynamic <- feols(
  consumption ~ sunab(cohort_week, week_id, ref.c = 0) |
    household + day_id,
  data = dt_daily,
  cluster = ~household
)

m_sunab_dynamic_log <- feols(
  log1p(consumption) ~ sunab(cohort_week, week_id, ref.c = 0) |
    household + day_id,
  data = dt_daily,
  cluster = ~household
)

iplot(
  m_sunab_dynamic,
  main = "Effect of App Access on Electricity Consumption",
  xlab = "Weeks relative to app access",
  ylab = "Change in daily electricity consumption (kWh)",
  pt.join = TRUE,
  col = "black",
  ci.col = "grey70",
  ci.alpha = 0.5,
  ref.line = -1,
  grid = FALSE,
  xlim = c(-10, 30)   # <- restrict view instead of binning
)

abline(h = 0, lty = 2, col = "grey40")


library(broom)
library(dplyr)
library(stringr)
library(ggplot2)

df_kwh <- broom::tidy(m_sunab_dynamic) %>%
  filter(str_detect(term, "week_id::")) %>%
  mutate(
    week = as.numeric(str_extract(term, "-?\\d+")),
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error
  ) %>%
  filter(week >= -10 & week <= 30)


gg_kwh <- ggplot(df_kwh, aes(x = week, y = estimate)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              fill = "grey85", alpha = 0.8) +
  geom_line(color = "black", size = 1) +
  geom_point(color = "black", size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_vline(xintercept = -0.5, linetype = "dotted", color = "grey40") +
  labs(
    title = "",
    x = "Weeks relative to app access",
    y = "Change in daily electricity consumption (kWh)"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold")
  )

gg_kwh
  

dt_weekly <- dt_daily %>%
  mutate(
    week_id = floor(day_id / 7),
    cohort_week = floor(cohort_id_day / 7)
  ) %>%
  group_by(household, week_id) %>%
  summarise(
    consumption = sum(consumption, na.rm = TRUE),
    cohort_week = first(cohort_week),
    .groups = "drop"
  )


m_sunab_weekly <- feols(
  consumption ~ sunab(cohort_week, week_id, ref.c = 0) |
    household + week_id,
  data = dt_weekly,
  cluster = ~household
)


# Appendix 

df_weekly <- broom::tidy(m_sunab_weekly) %>%
  filter(str_detect(term, "week_id::")) %>%
  mutate(
    week = as.numeric(str_extract(term, "-?\\d+")),
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error
  ) %>%
  filter(week >= -10 & week <= 50)


gg_weekly <- ggplot(df_weekly, aes(x = week, y = estimate)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              fill = "grey85", alpha = 0.8) +
  geom_line(color = "black", size = 1) +
  geom_point(color = "black", size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_vline(xintercept = -0.5, linetype = "dotted", color = "grey40") +
  labs(
    title = "Dynamic Effect of App Access on Weekly Electricity Consumption",
    x = "Weeks relative to app access",
    y = "Change in weekly electricity consumption (kWh)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold")
  )

gg_weekly



dt_daily_flex <- dt_hourly %>%
  filter(hour %in% 7:22) %>%
  group_by(household, date) %>%
  summarise(
    consumption = sum(consumption, na.rm = TRUE),
    cohort_week = first(cohort_id_day %/% 7),
    day_id = first(day_id),
    week_id = first(day_id %/% 7),
    .groups = "drop"
  )

dt_daily_inflex <- dt_hourly %>%
  filter(!(hour %in% 7:22)) %>%
  group_by(household, date) %>%
  summarise(
    consumption = sum(consumption, na.rm = TRUE),
    cohort_week = first(cohort_id_day %/% 7),
    day_id = first(day_id),
    week_id = first(day_id %/% 7),
    .groups = "drop"
  )



m_total <- feols(
  consumption ~ sunab(cohort_week, week_id, ref.c = 0) |
    household + day_id,
  data = dt_daily,
  cluster = ~household
)

m_flex <- feols(
  consumption ~ sunab(cohort_week, week_id, ref.c = 0) |
    household + day_id,
  data = dt_daily_flex,
  cluster = ~household
)

m_inflex <- feols(
  consumption ~ sunab(cohort_week, week_id, ref.c = 0) |
    household + day_id,
  data = dt_daily_inflex,
  cluster = ~household
)




extract_sunab <- function(model, label){
  broom::tidy(model) %>%
    filter(str_detect(term, "week_id::")) %>%
    mutate(
      week = as.numeric(str_extract(term, "-?\\d+")),
      conf.low = estimate - 1.96 * std.error,
      conf.high = estimate + 1.96 * std.error,
      type = label
    )
}

df_all <- bind_rows(
  extract_sunab(m_total, "Total"),
  extract_sunab(m_flex, "Flexible"),
  extract_sunab(m_inflex, "Non-flexible")
) %>%
  filter(week >= -10 & week <= 40)


gg_combined <- ggplot(df_all, aes(x = week, y = estimate, color = type)) +
  
  # CI ribbons (only for total to avoid clutter)
  geom_ribbon(
    data = df_all %>% filter(type == "Total"),
    aes(x = week, ymin = conf.low, ymax = conf.high),
    fill = "grey80", alpha = 0.6
  ) +
  
  geom_line(size = 1) +
  geom_point(size = 2) +
  
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_vline(xintercept = -0.5, linetype = "dotted", color = "grey40") +
  
  scale_color_manual(values = c(
    "Total" = "black",
    "Flexible" = "#1b9e77",
    "Non-flexible" = "#d95f02"
  )) +
  
  labs(
    title = "Dynamic Effects by Consumption Type",
    x = "Weeks relative to app access",
    y = "Change in daily electricity consumption (kWh)",
    color = ""
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold"),
    legend.position = "top"
  )

gg_combined


gg_combined2 <- ggplot(df_all, aes(x = week, y = estimate, color = type)) +
  
  # CI only for total (lighter)
  geom_errorbar(
    data = df_all %>% filter(type == "Total"),
    aes(ymin = conf.low, ymax = conf.high),
    width = 0.15,
    color = "black",
    alpha = 0.4
  ) +
  
  # lines
  geom_line(aes(size = type)) +
  
  # points
  geom_point(size = 2) +
  
  # reference lines
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey30") +
  geom_vline(xintercept = -0.5, linetype = "dotted", color = "grey60") +
  
  # colors (softer, publication style)
  scale_color_manual(values = c(
    "Total" = "black",
    "Flexible" = "#2a9d8f",
    "Non-flexible" = "#e76f51"
  )) +
  
  # emphasize total slightly
  scale_size_manual(values = c(
    "Total" = 1.2,
    "Flexible" = 1,
    "Non-flexible" = 1
  )) +
  
  labs(
    title = "",
    x = "Weeks relative to app access",
    y = "Change in daily consumption (kWh)",
    color = NULL
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    
    plot.title = element_text(size = 10, face = "bold"),
    
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  
  # remove unnecessary legends
  guides(
    size = "none",
    linetype = "none"
  )

gg_combined2
