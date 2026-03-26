###############################################################################
# 01_power_simulation.R
#
# Purpose:
# Template for ex-ante power analysis via simulation.
###############################################################################

message("Running power simulation template...")

# Placeholder effect sizes (to be defined per project)
effect_sizes <- c(0.1, 0.2, 0.3)

simulate_power <- function(n, effect_size, alpha = 0.05, reps = 1000) {
  p_values <- replicate(reps, {
    y_control  <- rnorm(n / 2, mean = 0, sd = 1)
    y_treat    <- rnorm(n / 2, mean = effect_size, sd = 1)
    t.test(y_treat, y_control)$p.value
  })
  mean(p_values < alpha)
}

power_results <- tibble(
  effect_size = effect_sizes,
  power = map_dbl(effect_sizes, ~ simulate_power(
    n = params$n_total,
    effect_size = .x,
    alpha = params$alpha
  ))
)

print(power_results)

message("Power simulation completed.")
