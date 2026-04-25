# =============================================================================
# 01_phase0_calibration.R
# Phase 0: Calibration Analysis
#
# GOAL: Establish the baseline rate at which square 24 is triggered under
# pure learning pressure with no opponent strategy (vs_random, MW=0.0).
# This is the empirical null hypothesis for the rest of the experiment.
# A rate significantly above this in later phases = evidence of learned spite.
#
# STATISTICAL APPROACH:
#   - Binomial proportion CI for M24 rate
#   - Learning curve: episode-level rolling average
#   - Comparison of M24 rate at episode 50k vs self_play baseline
# =============================================================================

source("R/00_setup.R")
library(tidyverse)
library(ggplot2)
library(patchwork)
library(scales)

cat("\n=== PHASE 0: CALIBRATION ===\n\n")

# --- Load calibration run only -----------------------------------------------
ep_all <- load_episodes()

# Phase 0: the 50k-episode vs_random run (calibration run)
cal <- ep_all |>
  filter(mode == "vs_random", moral_weight == 0.0) |>
  filter(stringr::str_detect(run_id, "013951"))  # the 50k calibration run

cat("Calibration run episodes:", nrow(cal), "\n")

# --- 1. Binomial proportion confidence interval for M24 rate -----------------
n_total  <- nrow(cal)
n_m24    <- sum(cal$outcome == "Mutual Loss (sq.24)")
prop_test <- prop.test(n_m24, n_total, conf.level = 0.95)

cat("\n--- CALIBRATION BINOMIAL PROPORTION TEST ---\n")
cat(sprintf("  N episodes:     %d\n",   n_total))
cat(sprintf("  M24 events:     %d\n",   n_m24))
cat(sprintf("  M24 rate:       %.2f%%\n", prop_test$estimate * 100))
cat(sprintf("  95%% CI:        [%.2f%%, %.2f%%]\n",
            prop_test$conf.int[1] * 100,
            prop_test$conf.int[2] * 100))
cat(sprintf("  p-value (H0: p=0.5): %.4g\n", prop_test$p.value))

# Save results
cal_results <- tibble(
  metric        = c("M24 rate", "CI lower", "CI upper", "N episodes", "M24 count"),
  value         = c(prop_test$estimate * 100,
                    prop_test$conf.int[1] * 100,
                    prop_test$conf.int[2] * 100,
                    n_total, n_m24)
)
write_csv(cal_results, file.path(OUTPUT_DIR, "phase0_calibration_stats.csv"))

# --- 2. Learning curve: rolling M24 rate over training -----------------------
# Compute rolling 500-episode average
roll_window <- 500
cal_roll <- cal |>
  arrange(episode) |>
  mutate(
    m24_flag   = as.integer(outcome == "Mutual Loss (sq.24)"),
    win_flag   = as.integer(outcome == "Win"),
    m24_roll   = zoo::rollmean(m24_flag, k = roll_window, fill = NA, align = "right"),
    win_roll   = zoo::rollmean(win_flag, k = roll_window, fill = NA, align = "right")
  )

p_cal_curve <- ggplot(cal_roll, aes(x = episode)) +
  geom_line(aes(y = m24_roll * 100, colour = "Mutual Loss (sq.24)"), linewidth = 0.9) +
  geom_line(aes(y = win_roll  * 100, colour = "Win"),                 linewidth = 0.9) +
  geom_hline(aes(yintercept = prop_test$estimate * 100),
             linetype = "dashed", colour = "grey40", linewidth = 0.6) +
  annotate("text", x = max(cal_roll$episode) * 0.6,
           y = prop_test$estimate * 100 + 3,
           label = sprintf("Final M24 = %.1f%%", prop_test$estimate * 100),
           colour = "grey30", size = 3.5) +
  scale_colour_manual(values = c("Mutual Loss (sq.24)" = "#f39c12", "Win" = "#2ecc71")) +
  scale_y_continuous(labels = scales::label_percent(scale = 1), limits = c(0, 100)) +
  scale_x_continuous(labels = scales::label_comma()) +
  labs(
    title    = "Phase 0: Calibration — Learning Curve",
    subtitle = "MW = 0.0, vs_random opponent, 50,000 episodes\nRolling 500-episode average",
    x        = "Training Episode",
    y        = "Rate (%)",
    colour   = "Outcome",
    caption  = "The final M24 rate (dashed) serves as the empirical null hypothesis baseline."
  ) +
  theme_nash()

save_plot(p_cal_curve, "phase0_learning_curve.png", width = 10, height = 5)

# --- 3. Outcome distribution bar chart ---------------------------------------
outcome_counts <- cal |>
  count(outcome) |>
  mutate(pct = n / sum(n) * 100)

p_cal_bar <- ggplot(outcome_counts, aes(x = reorder(outcome, -pct), y = pct, fill = outcome)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f%%\n(n=%s)", pct, scales::comma(n))),
            vjust = -0.4, size = 4, fontface = "bold") +
  scale_fill_manual(values = OUTCOME_COLOURS) +
  scale_y_continuous(limits = c(0, 100), labels = scales::label_percent(scale = 1)) +
  labs(
    title    = "Phase 0: Outcome Distribution (Calibration)",
    subtitle = "MW = 0.0 vs_random, N = 50,000 episodes",
    x        = "Outcome",
    y        = "Proportion (%)",
    caption  = "Mutual Loss dominates even in calibration, indicating rapid early learning of the spite strategy."
  ) +
  theme_nash()

save_plot(p_cal_bar, "phase0_outcome_distribution.png", width = 7, height = 5)

# --- 4. Steps-to-termination distribution ------------------------------------
p_cal_steps <- ggplot(cal, aes(x = steps, fill = outcome)) +
  geom_histogram(bins = 40, position = "stack", alpha = 0.85) +
  scale_fill_manual(values = OUTCOME_COLOURS) +
  scale_x_continuous(labels = scales::label_comma()) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(
    title    = "Phase 0: Game Length Distribution",
    subtitle = "Stacked by outcome — calibration run",
    x        = "Steps to Termination",
    y        = "Episode Count",
    fill     = "Outcome",
    caption  = "Short games dominated by Mutual Loss suggest early square-24 targeting."
  ) +
  theme_nash()

save_plot(p_cal_steps, "phase0_steps_distribution.png", width = 8, height = 5)

cat("\nPhase 0 analysis complete.\n")
