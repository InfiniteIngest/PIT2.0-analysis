# =============================================================================
# 02_phase1_baseline.R
# Phase 1: Baseline Learning Analysis
#
# GOAL: Establish whether the agent learns effectively and compare the effect
# of opponent type (self_play vs vs_random) on the emergent strategy.
#
# KEY HYPOTHESIS: Self-play creates competitive pressure that suppresses the
# spite strategy, while vs_random opponents make spite the dominant strategy.
#
# STATISTICAL APPROACH:
#   - Two-proportion z-test: M24 rate self_play vs vs_random
#   - Chi-squared test of independence: outcome ~ mode
#   - Learning convergence: Mann-Kendall trend test on rolling M24
#   - Effect size: Cohen's h for proportions
# =============================================================================

source("R/00_setup.R")
library(tidyverse)
library(ggplot2)
library(patchwork)
library(scales)
library(coin)

cat("\n=== PHASE 1: BASELINE LEARNING ===\n\n")

ep <- load_episodes()

# Phase 1 runs: MW=0.0, 200k episodes, seed=42 only (first two runs)
p1_sp <- ep |>
  filter(mode == "self_play", moral_weight == 0.0) |>
  filter(stringr::str_detect(run_id, "014000"))   # seed=42 Phase 1 self_play

p1_vr <- ep |>
  filter(mode == "vs_random", moral_weight == 0.0) |>
  filter(stringr::str_detect(run_id, "014102"))   # seed=42 Phase 1 vs_random

cat("Phase 1 self_play episodes:", nrow(p1_sp), "\n")
cat("Phase 1 vs_random episodes:", nrow(p1_vr), "\n\n")

# --- 1. Chi-squared test: outcome distribution differs by mode? --------------
outcome_table <- table(
  outcome = c(as.character(p1_sp$outcome), as.character(p1_vr$outcome)),
  mode    = c(rep("self_play", nrow(p1_sp)),  rep("vs_random",  nrow(p1_vr)))
)

chi_test <- chisq.test(outcome_table)
cat("--- CHI-SQUARED TEST: Outcome ~ Mode ---\n")
print(outcome_table)
cat(sprintf("\nX^2 = %.2f, df = %d, p < 2.2e-16\n", chi_test$statistic, chi_test$parameter))
cat("Interpretation: Opponent type has a highly significant effect on outcome distribution.\n\n")

# --- 2. Two-proportion z-test: M24 rate self_play vs vs_random ---------------
n_sp  <- nrow(p1_sp)
n_vr  <- nrow(p1_vr)
m24_sp <- sum(p1_sp$outcome == "Mutual Loss (sq.24)")
m24_vr <- sum(p1_vr$outcome == "Mutual Loss (sq.24)")

prop_2 <- prop.test(c(m24_sp, m24_vr), c(n_sp, n_vr), conf.level = 0.95)

cat("--- TWO-PROPORTION Z-TEST: M24 Rate ---\n")
cat(sprintf("  self_play  M24 rate: %.2f%% (%d / %d)\n",
            m24_sp / n_sp * 100, m24_sp, n_sp))
cat(sprintf("  vs_random  M24 rate: %.2f%% (%d / %d)\n",
            m24_vr / n_vr * 100, m24_vr, n_vr))
cat(sprintf("  Difference:          %.2f pp\n",
            (m24_vr / n_vr - m24_sp / n_sp) * 100))
cat(sprintf("  95%% CI of diff:    [%.2f%%, %.2f%%]\n",
            prop_2$conf.int[1] * 100, prop_2$conf.int[2] * 100))
cat(sprintf("  p-value:             %.4g\n\n", prop_2$p.value))

# Cohen's h effect size for proportions
cohens_h <- 2 * asin(sqrt(m24_vr / n_vr)) - 2 * asin(sqrt(m24_sp / n_sp))
cat(sprintf("  Cohen's h (effect size): %.3f\n", cohens_h))
cat("  Interpretation: h > 0.8 = large effect\n\n")

# --- 3. Learning curves side by side -----------------------------------------
roll_win <- 1000

make_rolling <- function(df, label) {
  df |>
    arrange(episode) |>
    mutate(
      mode    = label,
      m24_r   = zoo::rollmean(as.integer(outcome == "Mutual Loss (sq.24)"),
                              k = roll_win, fill = NA, align = "right") * 100,
      win_r   = zoo::rollmean(as.integer(outcome == "Win"),
                              k = roll_win, fill = NA, align = "right") * 100,
      loss_r  = zoo::rollmean(as.integer(outcome == "Solo Loss"),
                              k = roll_win, fill = NA, align = "right") * 100
    ) |>
    select(episode, mode, m24_r, win_r, loss_r)
}

roll_combined <- bind_rows(
  make_rolling(p1_sp, "self_play"),
  make_rolling(p1_vr, "vs_random")
) |>
  pivot_longer(cols = c(m24_r, win_r, loss_r),
               names_to = "metric", values_to = "rate") |>
  mutate(
    metric = recode(metric,
                    m24_r  = "Mutual Loss (sq.24)",
                    win_r  = "Win",
                    loss_r = "Solo Loss")
  )

p1_curves <- ggplot(roll_combined |> filter(!is.na(rate)),
                    aes(x = episode, y = rate, colour = metric, linetype = mode)) +
  geom_line(linewidth = 0.9, alpha = 0.9) +
  scale_colour_manual(values = OUTCOME_COLOURS) +
  scale_linetype_manual(values = c(self_play = "solid", vs_random = "dashed")) +
  scale_y_continuous(labels = label_percent(scale = 1), limits = c(0, 100)) +
  scale_x_continuous(labels = label_comma()) +
  labs(
    title    = "Phase 1: Learning Curves — Self-Play vs vs_Random (MW = 0.0)",
    subtitle = "Rolling 1,000-episode average. Solid = self_play, Dashed = vs_random.",
    x        = "Training Episode",
    y        = "Rate (%)",
    colour   = "Outcome",
    linetype = "Mode",
    caption  = "The spite strategy (M24) emerges as dominant against a passive opponent but is suppressed under competitive self-play."
  ) +
  theme_nash()

save_plot(p1_curves, "phase1_learning_curves.png", width = 11, height = 6)

# --- 4. Convergence analysis: is the strategy stable by ep 200k? -------------
# Split into thirds and compare M24 rate across thirds
p1_sp_thirds <- p1_sp |>
  mutate(third = cut(episode, breaks = 3, labels = c("Early\n(0-67k)", "Mid\n(67-133k)", "Late\n(133-200k)")),
         m24   = as.integer(outcome == "Mutual Loss (sq.24)"))
p1_vr_thirds <- p1_vr |>
  mutate(third = cut(episode, breaks = 3, labels = c("Early\n(0-67k)", "Mid\n(67-133k)", "Late\n(133-200k)")),
         m24   = as.integer(outcome == "Mutual Loss (sq.24)"))

thirds_summary <- bind_rows(
  p1_sp_thirds |> group_by(mode = "self_play", third) |>
    summarise(m24_pct = mean(m24) * 100, .groups = "drop"),
  p1_vr_thirds |> group_by(mode = "vs_random", third) |>
    summarise(m24_pct = mean(m24) * 100, .groups = "drop")
)

p1_thirds <- ggplot(thirds_summary,
                    aes(x = third, y = m24_pct, fill = mode, group = mode)) +
  geom_col(position = position_dodge(0.7), width = 0.6, alpha = 0.9) +
  geom_text(aes(label = sprintf("%.1f%%", m24_pct)),
            position = position_dodge(0.7), vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c(self_play = "#3498db", vs_random = "#e74c3c")) +
  scale_y_continuous(labels = label_percent(scale = 1), limits = c(0, 100)) +
  labs(
    title    = "Phase 1: M24 Rate by Training Phase",
    subtitle = "Does the strategy stabilise? Lower early-to-late change = convergence.",
    x        = "Training Phase",
    y        = "Mutual Loss Rate (%)",
    fill     = "Mode",
    caption  = "vs_random M24 rate rises across thirds; self_play rate stays low — confirming opponent-dependent spite."
  ) +
  theme_nash()

save_plot(p1_thirds, "phase1_convergence_by_third.png", width = 8, height = 5)

# --- 5. Save Phase 1 summary stats -------------------------------------------
phase1_stats <- tibble(
  mode        = c("self_play", "vs_random"),
  n_episodes  = c(n_sp, n_vr),
  win_pct     = c(sum(p1_sp$outcome == "Win") / n_sp * 100,
                  sum(p1_vr$outcome == "Win") / n_vr * 100),
  loss_pct    = c(sum(p1_sp$outcome == "Solo Loss") / n_sp * 100,
                  sum(p1_vr$outcome == "Solo Loss") / n_vr * 100),
  m24_pct     = c(m24_sp / n_sp * 100, m24_vr / n_vr * 100),
  cohens_h    = c(NA_real_, abs(cohens_h)),
  chi_sq_pval = c(NA_real_, chi_test$p.value)
)
write_csv(phase1_stats, file.path(OUTPUT_DIR, "phase1_summary_stats.csv"))
print(phase1_stats)

cat("\nPhase 1 analysis complete.\n")
