# =============================================================================
# 06_phase5_opponents.R
# Phase 5: Opponent Type Comparison Analysis
#
# GOAL: Does opponent sophistication modulate the emergence of spite?
# Three opponent types: self_play (learning), vs_random, vs_uniform (2/3 mechanic).
#
# HYPOTHESIS: A passive opponent (vs_random) provides more opportunity for
# spite because it never threatens to win, making mutual loss strategically
# preferable to losing alone. A strategic opponent (self_play) competes
# effectively enough that winning is achievable, suppressing spite.
#
# GAME THEORY FRAMING:
# This tests whether the Nash Equilibrium changes with the opponent's strategy.
# In Nash's framework, the equilibrium strategy depends on what the opponent does.
# vs_random opponent -> spite is rational (dominates loss)
# self_play opponent -> winning dominates, spite is irrelevant
#
# STATISTICAL APPROACH:
#   - Three-group chi-squared: outcome ~ opponent_type
#   - Pairwise proportion tests (Bonferroni)
#   - Effect of moral_weight within each opponent type
#   - Interaction model: M24 ~ moral_weight * opponent_type
# =============================================================================

source("R/00_setup.R")
library(tidyverse)
library(ggplot2)
library(patchwork)
library(scales)
library(broom)

cat("\n=== PHASE 5: OPPONENT TYPE COMPARISON ===\n\n")

ep <- load_episodes()

# Build Phase 5 dataset: MW=0.0 and MW=1.0, all three opponent types
# Using all available runs for each combination
p5 <- ep |>
  filter(moral_weight %in% c(0.0, 1.0),
         mode %in% c("self_play", "vs_random", "vs_uniform")) |>
  mutate(
    m24_flag  = as.integer(outcome == "Mutual Loss (sq.24)"),
    win_flag  = as.integer(outcome == "Win"),
    mw_label  = sprintf("MW = %.1f", moral_weight),
    mode_label = recode(mode,
                        self_play  = "Self-Play\n(competitive)",
                        vs_random  = "vs Random\n(passive)",
                        vs_uniform = "vs Uniform\n(semi-strategic)")
  )

p5_conv <- last_pct(p5, 0.20)

cat("Phase 5 converged episodes by mode and MW:\n")
p5_conv |> count(mode_label, moral_weight) |> print()
cat("\n")

# --- 1. Summary rates by mode and MW -----------------------------------------
p5_summary <- p5_conv |>
  group_by(mode, mode_label, moral_weight, mw_label) |>
  summarise(
    n        = n(),
    win_pct  = mean(win_flag) * 100,
    m24_pct  = mean(m24_flag) * 100,
    loss_pct = mean(outcome == "Solo Loss") * 100,
    .groups  = "drop"
  )

cat("--- PHASE 5 SUMMARY TABLE ---\n")
print(p5_summary |> select(mode_label, moral_weight, win_pct, loss_pct, m24_pct, n))
write_csv(p5_summary, file.path(OUTPUT_DIR, "phase5_summary.csv"))

# --- 2. Chi-squared test: outcome ~ mode (within each MW) --------------------
cat("\n--- CHI-SQUARED: Outcome ~ Mode ---\n")
for (mw_val in c(0.0, 1.0)) {
  sub <- p5_conv |> filter(moral_weight == mw_val)
  tbl <- table(sub$outcome, sub$mode)
  chi <- chisq.test(tbl)
  cat(sprintf("  MW = %.1f: X^2 = %.2f, df = %d, p = %.4g\n",
              mw_val, chi$statistic, chi$parameter, chi$p.value))
}

# --- 3. Pairwise M24 tests by mode (Bonferroni) ------------------------------
cat("\n--- PAIRWISE M24 RATE TESTS BY MODE ---\n")
modes      <- unique(p5_conv$mode)
mode_pairs <- combn(modes, 2, simplify = FALSE)

pairwise_mode <- map_dfr(c(0.0, 1.0), function(mw_val) {
  map_dfr(mode_pairs, function(pair) {
    g1 <- p5_conv |> filter(mode == pair[1], moral_weight == mw_val)
    g2 <- p5_conv |> filter(mode == pair[2], moral_weight == mw_val)
    if (nrow(g1) == 0 | nrow(g2) == 0) return(NULL)
    n1 <- nrow(g1); x1 <- sum(g1$m24_flag)
    n2 <- nrow(g2); x2 <- sum(g2$m24_flag)
    pt <- prop.test(c(x1, x2), c(n1, n2))
    tibble(
      moral_weight = mw_val,
      mode1 = pair[1], mode2 = pair[2],
      m24_1 = x1 / n1 * 100, m24_2 = x2 / n2 * 100,
      diff  = (x2 / n2 - x1 / n1) * 100,
      p_raw = pt$p.value,
      p_adj = min(pt$p.value * length(mode_pairs) * 2, 1.0)
    )
  })
})

print(pairwise_mode)
write_csv(pairwise_mode, file.path(OUTPUT_DIR, "phase5_pairwise_mode.csv"))

# --- 4. Interaction plot: M24 ~ moral_weight x opponent_type -----------------
p5_interaction <- ggplot(p5_summary,
                         aes(x = moral_weight, y = m24_pct,
                             colour = mode_label, group = mode_label)) +
  geom_line(linewidth = 1.3) +
  geom_point(aes(size = n / 1000), alpha = 0.9) +
  geom_text(aes(label = sprintf("%.1f%%", m24_pct)),
            vjust = -1.2, size = 3.2, fontface = "bold") +
  scale_colour_manual(values = c(
    "Self-Play\n(competitive)"   = "#3498db",
    "vs Random\n(passive)"       = "#e74c3c",
    "vs Uniform\n(semi-strategic)" = "#f39c12"
  )) +
  scale_x_continuous(breaks = c(0, 1)) +
  scale_y_continuous(labels = label_percent(scale = 1), limits = c(0, 85)) +
  scale_size_continuous(name = "N (thousands)", range = c(3, 7)) +
  labs(
    title    = "Phase 5: Mutual Loss Rate — Moral Weight x Opponent Type",
    subtitle = "Interaction between guilt (MW) and opponent strategy on spite emergence.",
    x        = "Moral Weight",
    y        = "Mutual Loss Rate (%) — converged",
    colour   = "Opponent Type",
    caption  = paste(
      "Nash insight: the equilibrium strategy changes with opponent type.",
      "vs_random opponent makes spite the dominant strategy regardless of MW.",
      sep = "\n"
    )
  ) +
  theme_nash()

save_plot(p5_interaction, "phase5_interaction_m24.png", width = 10, height = 6)

# --- 5. Full outcome profile by mode (stacked bar) ----------------------------
p5_stack_data <- p5_summary |>
  select(mode_label, moral_weight, mw_label, win_pct, loss_pct, m24_pct) |>
  pivot_longer(cols = c(win_pct, loss_pct, m24_pct),
               names_to = "outcome_type", values_to = "pct") |>
  mutate(outcome_type = recode(outcome_type,
                               win_pct  = "Win",
                               loss_pct = "Solo Loss",
                               m24_pct  = "Mutual Loss"))

p5_stack <- ggplot(p5_stack_data,
                   aes(x = mode_label, y = pct, fill = outcome_type)) +
  geom_col(position = "stack", alpha = 0.9, width = 0.7) +
  geom_text(aes(label = sprintf("%.0f%%", pct)),
            position = position_stack(vjust = 0.5),
            size = 3.2, colour = "white", fontface = "bold") +
  facet_wrap(~ mw_label) +
  scale_fill_manual(values = c(Win = "#2ecc71", "Solo Loss" = "#e74c3c",
                               "Mutual Loss" = "#f39c12")) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title    = "Phase 5: Full Outcome Profile by Opponent Type and Moral Weight",
    subtitle = "Converged period (last 20% of 200,000 episodes).",
    x        = "Opponent Type",
    y        = "Outcome Rate (%)",
    fill     = "Outcome",
    caption  = "Self-play produces the most competitive environment: highest win rate, lowest mutual loss."
  ) +
  theme_nash() +
  theme(axis.text.x = element_text(size = 9))

save_plot(p5_stack, "phase5_outcome_by_opponent.png", width = 12, height = 6)

# --- 6. Formal interaction model: M24 ~ MW * mode ----------------------------
cat("\n--- INTERACTION MODEL: M24 ~ moral_weight * mode ---\n")
glm_interaction <- glm(
  m24_flag ~ moral_weight * mode,
  data   = p5_conv,
  family = binomial(link = "logit")
)
cat("\nModel summary (logistic regression with interaction):\n")
print(tidy(glm_interaction, exponentiate = TRUE, conf.int = TRUE))

# Save
write_csv(tidy(glm_interaction, exponentiate = TRUE, conf.int = TRUE),
          file.path(OUTPUT_DIR, "phase5_interaction_model.csv"))

cat("\nPhase 5 analysis complete.\n")
