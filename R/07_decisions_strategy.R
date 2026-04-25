# =============================================================================
# 07_decisions_strategy.R
# Decision-Level Strategy Analysis
#
# GOAL: Analyse the agent's moment-by-moment decision-making to understand
# WHEN and WHY it chooses to steer toward square 24.
#
# KEY QUESTIONS:
#   - Does the agent use deliberate dice choices (choose_2, choose_3) more
#     when it is losing (gap > 0) and near square 24?
#   - Do Q-values show the agent has learned square 24 is advantageous
#     when losing? (q_choose2 > q_random when near_24 AND losing)
#   - How does moral_weight shift the action distribution?
#   - Does the deliberate action rate near_24 qualify as instrumental behaviour?
#
# STATISTICAL APPROACH:
#   - Logistic regression: P(choose_2 or choose_3) ~ near_24 * gap * moral_weight
#   - Q-value comparison: random vs deliberate Q when near_24 and losing
#   - Action distribution shift across moral weights
#   - Mixed-effects model: P(deliberate action) ~ near_24 + (1|run_id)
# =============================================================================

source("R/00_setup.R")
library(tidyverse)
library(ggplot2)
library(patchwork)
library(scales)
library(lme4)
library(broom.mixed)

cat("\n=== DECISION-LEVEL STRATEGY ANALYSIS ===\n\n")

dec <- load_decisions()

# Filter to self_play mode (controlled experiment)
dec_sp <- dec |>
  filter(mode == "self_play") |>
  mutate(
    deliberate   = action %in% c("choose_2", "choose_3"),
    losing       = gap > 0,
    near_24_lose = near_24 & losing,
    mw_factor    = factor(round(moral_weight, 1))
  )

cat("Decision records (self_play):", nrow(dec_sp), "\n")
cat("Action distribution:\n")
print(dec_sp |> count(action) |> mutate(pct = n / sum(n) * 100))
cat("\n")

# --- 1. Action distribution by moral weight ----------------------------------
action_by_mw <- dec_sp |>
  group_by(moral_weight, action) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(moral_weight) |>
  mutate(pct = n / sum(n) * 100) |>
  ungroup()

p_action_dist <- ggplot(action_by_mw,
                        aes(x = moral_weight, y = pct,
                            colour = action, group = action)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_colour_manual(values = c(random   = "#3498db",
                                 choose_2 = "#f39c12",
                                 choose_3 = "#9b59b6")) +
  scale_x_continuous(breaks = MW_LEVELS) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title    = "Action Distribution vs Moral Weight (Self-Play)",
    subtitle = "What dice strategy does the agent prefer at each guilt level?",
    x        = "Moral Weight",
    y        = "Action Rate (%)",
    colour   = "Action",
    caption  = "Higher random roll usage = agent relies less on deliberate targeting."
  ) +
  theme_nash()

save_plot(p_action_dist, "decisions_action_distribution.png", width = 10, height = 5)

# --- 2. Deliberate action rate: near_24 x losing x moral_weight --------------
delib_summary <- dec_sp |>
  group_by(moral_weight, near_24, losing) |>
  summarise(
    n          = n(),
    delib_rate = mean(deliberate) * 100,
    .groups    = "drop"
  ) |>
  mutate(
    condition = case_when(
      near_24 & losing   ~ "Near sq.24 AND Losing",
      near_24 & !losing  ~ "Near sq.24 AND Winning",
      !near_24 & losing  ~ "Not Near sq.24, Losing",
      TRUE               ~ "Not Near sq.24, Winning"
    )
  )

p_delib <- ggplot(delib_summary,
                  aes(x = moral_weight, y = delib_rate,
                      colour = condition, group = condition)) +
  geom_line(linewidth = 1.2, alpha = 0.9) +
  geom_point(size = 3) +
  scale_colour_viridis_d(option = "plasma", begin = 0.05, end = 0.95) +
  scale_x_continuous(breaks = MW_LEVELS) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title    = "Deliberate Action Rate by Situation",
    subtitle = "Are deliberate dice choices (choose_2/3) elevated when near sq.24 and losing?",
    x        = "Moral Weight",
    y        = "Deliberate Action Rate (%)",
    colour   = "Game Situation",
    caption  = paste(
      "Key test: 'Near sq.24 AND Losing' should have highest deliberate rate.",
      "This would indicate intentional steering toward mutual loss.",
      sep = "\n"
    )
  ) +
  theme_nash()

save_plot(p_delib, "decisions_deliberate_by_situation.png", width = 11, height = 6)

# --- 3. Q-value comparison: near_24 and losing vs other states ---------------
# When near sq.24 and losing, do Q-values favour deliberate rolls over random?
q_summary <- dec_sp |>
  filter(!is.na(q_random), !is.na(q_choose2), !is.na(q_choose3)) |>
  mutate(
    q_delib_advantage = pmax(q_choose2, q_choose3) - q_random,
    situation = case_when(
      near_24 & losing   ~ "Near 24, Losing",
      near_24 & !losing  ~ "Near 24, Winning",
      !near_24 & losing  ~ "Far from 24, Losing",
      TRUE               ~ "Far from 24, Winning"
    )
  ) |>
  group_by(moral_weight, situation) |>
  summarise(
    mean_q_adv = mean(q_delib_advantage, na.rm = TRUE),
    se_q_adv   = sd(q_delib_advantage, na.rm = TRUE) / sqrt(n()),
    .groups    = "drop"
  )

p_qval <- ggplot(q_summary,
                 aes(x = moral_weight, y = mean_q_adv,
                     colour = situation, group = situation)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_line(linewidth = 1.1) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = mean_q_adv - 1.96 * se_q_adv,
                  ymax = mean_q_adv + 1.96 * se_q_adv,
                  fill = situation),
              alpha = 0.12, colour = NA) +
  scale_colour_viridis_d(option = "cividis", begin = 0.1, end = 0.9) +
  scale_fill_viridis_d(option  = "cividis", begin = 0.1, end = 0.9, guide = "none") +
  scale_x_continuous(breaks = MW_LEVELS) +
  labs(
    title    = "Q-Value Advantage of Deliberate Over Random Roll",
    subtitle = "max(q_choose2, q_choose3) - q_random. Positive = agent prefers deliberate roll.",
    x        = "Moral Weight",
    y        = "Mean Q Advantage (deliberate vs random)",
    colour   = "Game Situation",
    caption  = paste(
      "When near sq.24 and losing, positive Q-advantage = agent has learned to deliberately target sq.24.",
      "This is the Q-learning signature of the Nash equilibrium spite strategy.",
      sep = "\n"
    )
  ) +
  theme_nash()

save_plot(p_qval, "decisions_q_value_advantage.png", width = 11, height = 6)

# --- 4. Mixed-effects logistic model: P(deliberate) ~ near_24 + losing + MW --
cat("--- MIXED EFFECTS MODEL: P(deliberate) ~ near_24 * losing + moral_weight + (1|run_id) ---\n")

# Sample for computational tractability (1M rows from dec_sp)
set.seed(42)
dec_sample <- dec_sp |>
  filter(!is.na(q_random)) |>
  sample_n(min(200000, nrow(dec_sp)))

glmm_delib <- glmer(
  deliberate ~ near_24 * losing + moral_weight + (1 | run_id),
  data   = dec_sample,
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

cat("\nMixed-effects model fixed effects:\n")
glmm_results <- tidy(glmm_delib, effects = "fixed", conf.int = TRUE,
                     exponentiate = TRUE)
print(glmm_results)
write_csv(glmm_results, file.path(OUTPUT_DIR, "decisions_glmm_results.csv"))

# --- 5. Rolling action distribution over training (convergence) ---------------
# For MW=0.0 self_play only — does action distribution converge?
dec_mw0 <- dec_sp |>
  filter(moral_weight == 0.0) |>
  arrange(episode) |>
  mutate(
    ep_bin = cut(episode, breaks = 20, labels = FALSE)
  ) |>
  group_by(ep_bin) |>
  summarise(
    min_ep   = min(episode),
    random_r  = mean(action == "random")   * 100,
    choose2_r = mean(action == "choose_2") * 100,
    choose3_r = mean(action == "choose_3") * 100,
    .groups  = "drop"
  )

p_action_conv <- ggplot(dec_mw0 |>
                          pivot_longer(cols = c(random_r, choose2_r, choose3_r),
                                       names_to = "action", values_to = "pct"),
                        aes(x = min_ep, y = pct, colour = action)) +
  geom_line(linewidth = 1.1) +
  scale_colour_manual(values = c(random_r  = "#3498db",
                                 choose2_r = "#f39c12",
                                 choose3_r = "#9b59b6"),
                      labels = c("Random Roll", "Choose 2", "Choose 3")) +
  scale_x_continuous(labels = label_comma()) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title    = "Action Distribution Over Training (MW = 0.0, Self-Play)",
    subtitle = "Does the agent's dice strategy converge?",
    x        = "Training Episode",
    y        = "Action Rate (%)",
    colour   = "Action",
    caption  = "Strategy convergence seen here mirrors Nash Equilibrium stability in the Q-table."
  ) +
  theme_nash()

save_plot(p_action_conv, "decisions_action_convergence.png", width = 10, height = 5)

cat("\nDecision-level analysis complete.\n")
