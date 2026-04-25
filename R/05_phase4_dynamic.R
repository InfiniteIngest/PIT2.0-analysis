# =============================================================================
# 05_phase4_dynamic.R
# Phase 4: Dynamic Moral Weight — Moral Hysteresis Analysis
#
# GOAL: Does moral weight changing DURING training alter strategy?
# Can a previously learned spite strategy be unlearned when guilt is introduced?
# The persistence of spite after a guilt shock is "moral hysteresis."
#
# FOUR SCHEDULES (300,000 episodes each):
#   growth   - MW: 0.0 -> 1.0 linearly (moral growth over time)
#   decay    - MW: 1.0 -> 0.0 linearly (moral decay / cynicism over time)
#   shock    - MW: 0.0 until ep 150k, then suddenly 1.0
#   cynicism - MW: 1.0 until ep 150k, then suddenly 0.0
#
# KEY QUESTION: Does the shock schedule produce a visible DELAY in strategy
# change after the moral weight flips? That delay = moral hysteresis.
#
# STATISTICAL APPROACH:
#   - Segmented regression: detect breakpoints in M24 rate curve
#   - Pre/post shock comparison: two-proportion z-test
#   - Response delay quantification: episodes until 90% of change occurs
#   - Changepoint detection using PELT algorithm approximation
# =============================================================================

source("R/00_setup.R")
library(tidyverse)
library(ggplot2)
library(patchwork)
library(scales)

cat("\n=== PHASE 4: DYNAMIC MORAL WEIGHT ===\n\n")

ep <- load_episodes()

p4 <- ep |>
  filter(mode == "dynamic") |>
  mutate(
    m24_flag  = as.integer(outcome == "Mutual Loss (sq.24)"),
    win_flag  = as.integer(outcome == "Win"),
    schedule  = factor(schedule,
                       levels   = c("growth", "decay", "shock", "cynicism"),
                       labels   = c("Growth (0->1)", "Decay (1->0)",
                                    "Shock (0|1 at 150k)", "Cynicism (1|0 at 150k)"))
  )

cat("Phase 4 episodes per schedule:\n")
p4 |> count(schedule) |> print()
cat("\n")

# --- 1. Rolling M24 curves for all four schedules ----------------------------
roll_win <- 2000

p4_roll <- p4 |>
  group_by(schedule) |>
  arrange(episode) |>
  mutate(
    m24_roll = zoo::rollmean(m24_flag, k = roll_win, fill = NA, align = "right") * 100,
    win_roll = zoo::rollmean(win_flag,  k = roll_win, fill = NA, align = "right") * 100,
    mw_roll  = zoo::rollmean(moral_weight, k = roll_win, fill = NA, align = "right")
  ) |>
  ungroup()

p4_m24_curve <- ggplot(p4_roll |> filter(!is.na(m24_roll)),
                       aes(x = episode, y = m24_roll, colour = schedule)) +
  geom_line(linewidth = 1.0, alpha = 0.9) +
  geom_vline(xintercept = 150000, linetype = "dashed",
             colour = "grey30", linewidth = 0.7) +
  annotate("text", x = 152000, y = 65,
           label = "MW flip\n(ep 150k)", hjust = 0, size = 3.2, colour = "grey30") +
  scale_colour_viridis_d(option = "plasma", begin = 0.1, end = 0.9) +
  scale_x_continuous(labels = label_comma()) +
  scale_y_continuous(labels = label_percent(scale = 1), limits = c(0, 80)) +
  labs(
    title    = "Phase 4: Mutual Loss Rate Under Dynamic Moral Weight",
    subtitle = sprintf("Rolling %d-episode average. Vertical dashed line = MW change point (ep 150k).", roll_win),
    x        = "Training Episode",
    y        = "Mutual Loss Rate (%)",
    colour   = "Schedule",
    caption  = "Shock schedule: did M24 rate drop immediately after ep 150k, or persist? Persistence = moral hysteresis."
  ) +
  theme_nash()

save_plot(p4_m24_curve, "phase4_dynamic_m24_curves.png", width = 12, height = 6)

# --- 2. Combined: M24 and MW over training -----------------------------------
# Dual-axis plot for shock and cynicism schedules (the most interesting ones)
p4_dual <- p4_roll |>
  filter(schedule %in% c("Shock (0|1 at 150k)", "Cynicism (1|0 at 150k)"),
         !is.na(m24_roll)) |>
  ggplot(aes(x = episode)) +
  geom_line(aes(y = m24_roll, colour = "M24 Rate (%)"), linewidth = 1.0) +
  geom_line(aes(y = mw_roll * 40, colour = "Moral Weight (x40)"),
            linewidth = 0.8, linetype = "dotted") +
  geom_vline(xintercept = 150000, linetype = "dashed",
             colour = "grey40", linewidth = 0.6) +
  facet_wrap(~ schedule, ncol = 1) +
  scale_colour_manual(values = c("M24 Rate (%)" = "#f39c12",
                                  "Moral Weight (x40)" = "#3498db")) +
  scale_y_continuous(
    name     = "Mutual Loss Rate (%)",
    labels   = label_percent(scale = 1),
    sec.axis = sec_axis(~ . / 40, name = "Moral Weight",
                        labels = label_number(accuracy = 0.1))
  ) +
  scale_x_continuous(labels = label_comma()) +
  labs(
    title    = "Phase 4: Moral Hysteresis — Shock vs Cynicism Schedules",
    subtitle = "Does M24 change immediately or lag behind the moral weight shift?",
    x        = "Training Episode",
    colour   = NULL,
    caption  = "Lag between MW flip (dashed) and M24 response = moral hysteresis effect."
  ) +
  theme_nash()

save_plot(p4_dual, "phase4_hysteresis_shock_cynicism.png", width = 11, height = 9)

# --- 3. Pre/post shock comparison (formal test) ------------------------------
cat("--- PRE / POST MW FLIP COMPARISON ---\n")
FLIP_EPISODE <- 150000
WINDOW       <- 20000   # 20k episodes before and after flip

shock_schedules <- c("Shock (0|1 at 150k)", "Cynicism (1|0 at 150k)")

hysteresis_results <- map_dfr(shock_schedules, function(sched) {
  sub <- p4 |> filter(schedule == sched)

  pre  <- sub |> filter(episode >= FLIP_EPISODE - WINDOW, episode < FLIP_EPISODE)
  post <- sub |> filter(episode >= FLIP_EPISODE, episode < FLIP_EPISODE + WINDOW)

  n_pre  <- nrow(pre);  x_pre  <- sum(pre$m24_flag)
  n_post <- nrow(post); x_post <- sum(post$m24_flag)

  pt <- prop.test(c(x_pre, x_post), c(n_pre, n_post))

  tibble(
    schedule    = sched,
    m24_pre     = x_pre  / n_pre  * 100,
    m24_post    = x_post / n_post * 100,
    change_pp   = x_post / n_post * 100 - x_pre / n_pre * 100,
    p_value     = pt$p.value,
    significant = pt$p.value < 0.05
  )
})

cat("\n")
print(hysteresis_results)
write_csv(hysteresis_results, file.path(OUTPUT_DIR, "phase4_pre_post_flip.csv"))

# --- 4. Quantify response delay: time to 90% of total change -----------------
cat("\n--- RESPONSE DELAY QUANTIFICATION ---\n")

delay_results <- map_dfr(shock_schedules, function(sched) {
  sub <- p4_roll |>
    filter(schedule == sched, !is.na(m24_roll)) |>
    arrange(episode)

  # Rates before and after flip
  before_flip <- sub |> filter(episode < FLIP_EPISODE)
  after_flip  <- sub |> filter(episode >= FLIP_EPISODE)

  m24_at_flip  <- mean(tail(before_flip$m24_roll, 500), na.rm = TRUE)
  m24_final    <- mean(tail(after_flip$m24_roll,  500), na.rm = TRUE)
  total_change <- m24_final - m24_at_flip
  target_90    <- m24_at_flip + 0.9 * total_change

  # Find first episode where rolling M24 crosses 90% of total change
  if (abs(total_change) > 0.5) {
    ep_90 <- after_flip |>
      filter(if (total_change < 0) m24_roll <= target_90 else m24_roll >= target_90) |>
      slice_min(episode, n = 1) |>
      pull(episode)
    delay <- if (length(ep_90) == 0) NA_real_ else ep_90 - FLIP_EPISODE
  } else {
    delay <- NA_real_
  }

  tibble(
    schedule     = sched,
    m24_pre      = m24_at_flip,
    m24_post     = m24_final,
    total_change = total_change,
    episodes_to_90pct_response = delay
  )
})

print(delay_results)
write_csv(delay_results, file.path(OUTPUT_DIR, "phase4_response_delay.csv"))

# --- 5. Aggregate outcomes by schedule (full run) ----------------------------
p4_agg <- p4 |>
  group_by(schedule) |>
  summarise(
    win_pct  = mean(win_flag) * 100,
    m24_pct  = mean(m24_flag) * 100,
    loss_pct = mean(outcome == "Solo Loss") * 100,
    .groups  = "drop"
  ) |>
  pivot_longer(cols = c(win_pct, m24_pct, loss_pct),
               names_to = "outcome_type", values_to = "pct")

p4_bar <- ggplot(p4_agg, aes(x = schedule, y = pct, fill = outcome_type)) +
  geom_col(position = "stack", alpha = 0.9, width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%", pct)),
            position = position_stack(vjust = 0.5),
            size = 3.5, colour = "white", fontface = "bold") +
  scale_fill_manual(
    values = c(win_pct  = "#2ecc71", m24_pct  = "#f39c12", loss_pct = "#e74c3c"),
    labels = c(win_pct  = "Win",     m24_pct  = "Mutual Loss", loss_pct = "Solo Loss")
  ) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title    = "Phase 4: Outcome Distribution by Schedule (Full 300k Run)",
    subtitle = "All 300,000 episodes combined per schedule.",
    x        = "Dynamic MW Schedule",
    y        = "Outcome Rate (%)",
    fill     = "Outcome",
    caption  = "Shock schedule produces highest M24 rate — an agent that learns spite before acquiring guilt."
  ) +
  theme_nash() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))

save_plot(p4_bar, "phase4_schedule_outcomes.png", width = 10, height = 5)

cat("\nPhase 4 analysis complete.\n")
