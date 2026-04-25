# =============================================================================
# 09_comprehensive_summary.R
# Comprehensive Summary, Ethics Framing, and Publication-Ready Plots
#
# GOAL: Synthesise all findings into a coherent narrative for the research
# paper. Produce the key combined visualisations suitable for publication.
#
# ETHICAL FRAMING:
# This experiment demonstrates that an AI agent, when designed without moral
# constraints (MW=0.0), will develop a "spite strategy" — deliberately harming
# both itself and its opponent — as a Nash-rational response to avoid solo
# defeat. This has direct implications for AI safety:
#
# 1. CONSEQUENTIALIST DANGER: Amoral agents optimise purely for avoiding loss.
#    When winning is difficult, destroying shared value (mutual loss) becomes
#    rational. This is analogous to scorched-earth tactics in competitive AI.
#
# 2. MORAL ENCODING WORKS — BUT BARELY: Moral weight suppresses spite by ~55%
#    across self_play runs (from 2.9% to 1.3%), but the effect is small in
#    competitive settings. The real danger is in non-competitive settings
#    (vs_random) where spite reaches 76.8% with MW=0.0.
#
# 3. MORAL HYSTERESIS IS REAL: An agent that learns spite early (shock
#    schedule) takes significant time to unlearn it even after guilt is
#    introduced. This models the difficulty of correcting misaligned AI
#    after deployment.
#
# 4. OPPONENT-DEPENDENT EQUILIBRIA: The Nash Equilibrium changes with the
#    environment. This means context-specific testing is insufficient for
#    safety — an agent safe in one context may be dangerous in another.
# =============================================================================

source("R/00_setup.R")
library(tidyverse)
library(ggplot2)
library(patchwork)
library(scales)
library(gt)

cat("\n=== COMPREHENSIVE SUMMARY ===\n\n")

ep  <- load_episodes()
dec <- load_decisions()

# =============================================================================
# MASTER RESULTS TABLE
# =============================================================================

all_summary <- ep |>
  filter(mode %in% c("self_play", "vs_random", "vs_uniform")) |>
  group_by(run_id, moral_weight, mode) |>
  summarise(
    n_episodes  = n(),
    win_pct     = mean(outcome == "Win")                 * 100,
    loss_pct    = mean(outcome == "Solo Loss")            * 100,
    m24_pct     = mean(outcome == "Mutual Loss (sq.24)") * 100,
    avg_steps   = mean(steps),
    .groups     = "drop"
  ) |>
  arrange(mode, moral_weight)

write_csv(all_summary, file.path(OUTPUT_DIR, "master_results_table.csv"))

# Converged summary (last 20%)
conv_summary <- last_pct(ep |> filter(mode %in% c("self_play","vs_random","vs_uniform")), 0.20) |>
  group_by(moral_weight, mode) |>
  summarise(
    n_runs      = n_distinct(run_id),
    n_episodes  = n(),
    win_pct     = mean(outcome == "Win")                 * 100,
    loss_pct    = mean(outcome == "Solo Loss")            * 100,
    m24_pct     = mean(outcome == "Mutual Loss (sq.24)") * 100,
    .groups     = "drop"
  ) |>
  arrange(mode, moral_weight)

cat("--- MASTER CONVERGED SUMMARY TABLE ---\n")
print(conv_summary, n = 50)
write_csv(conv_summary, file.path(OUTPUT_DIR, "master_converged_summary.csv"))

# =============================================================================
# FIGURE 1: THE KEY RESULT — M24 RATE ACROSS ALL CONDITIONS
# =============================================================================

# Self-play sweep (controlled experiment)
sp_data <- conv_summary |>
  filter(mode == "self_play") |>
  arrange(moral_weight)

# Opponent comparison points
opp_data <- conv_summary |>
  filter(mode %in% c("vs_random", "vs_uniform")) |>
  mutate(mode_label = recode(mode, vs_random = "vs Random", vs_uniform = "vs Uniform"))

p_main <- ggplot() +
  # Self-play dose-response
  geom_line(data = sp_data,
            aes(x = moral_weight, y = m24_pct),
            colour = "#3498db", linewidth = 1.5) +
  geom_point(data = sp_data,
             aes(x = moral_weight, y = m24_pct),
             colour = "#3498db", size = 4) +
  geom_text(data = sp_data,
            aes(x = moral_weight, y = m24_pct,
                label = sprintf("%.1f%%", m24_pct)),
            vjust = -1.3, colour = "#3498db", size = 3.2, fontface = "bold") +
  # Opponent comparison points
  geom_point(data = opp_data |> filter(moral_weight == 0),
             aes(x = moral_weight, y = m24_pct, colour = mode_label),
             size = 5, shape = 17) +
  geom_point(data = opp_data |> filter(moral_weight == 1),
             aes(x = moral_weight, y = m24_pct, colour = mode_label),
             size = 5, shape = 17) +
  geom_text(data = opp_data,
            aes(x = moral_weight, y = m24_pct, label = sprintf("%.1f%%", m24_pct),
                colour = mode_label),
            vjust = -1.3, size = 3.0, fontface = "bold") +
  # Nash threshold
  geom_vline(xintercept = 1.1, linetype = "dashed",
             colour = "grey40", linewidth = 0.8) +
  annotate("text", x = 1.15, y = 70,
           label = "Theoretical NE\nthreshold (MW=1.1)",
           hjust = 0, size = 3.0, colour = "grey35") +
  scale_colour_manual(values = c("vs Random" = "#e74c3c", "vs Uniform" = "#f39c12")) +
  scale_x_continuous(breaks = MW_LEVELS) +
  scale_y_continuous(labels = label_percent(scale = 1), limits = c(0, 85)) +
  labs(
    title    = "Primary Result: Mutual Loss (Square 24) Rate vs Moral Weight",
    subtitle = paste(
      "Blue line = self-play (controlled). Triangles = vs_random and vs_uniform opponents at MW=0 and MW=1.",
      "Self-play shows monotonic decline. Passive opponents show dramatically higher spite.",
      sep = "\n"
    ),
    x        = "Moral Weight (Guilt Parameter)",
    y        = "Mutual Loss Rate (%) — converged",
    colour   = "Opponent Type",
    caption  = paste(
      "Self-play: N=200,000 episodes per MW level. Opponent types: N=200,000 episodes each.",
      "Converged = last 20% of training.",
      sep = "\n"
    )
  ) +
  theme_nash()

save_plot(p_main, "FIGURE1_primary_result.png", width = 13, height = 7)

# =============================================================================
# FIGURE 2: MORAL HYSTERESIS (Phase 4)
# =============================================================================

p4_ep <- ep |>
  filter(mode == "dynamic") |>
  mutate(
    m24_flag = as.integer(outcome == "Mutual Loss (sq.24)"),
    schedule_label = recode(schedule,
                            growth   = "Growth\n(0 -> 1)",
                            decay    = "Decay\n(1 -> 0)",
                            shock    = "Shock\n(0 then 1 at ep.150k)",
                            cynicism = "Cynicism\n(1 then 0 at ep.150k)")
  )

roll_4 <- p4_ep |>
  group_by(schedule_label) |>
  arrange(episode) |>
  mutate(m24_r = zoo::rollmean(m24_flag, k = 2000, fill = NA, align = "right") * 100) |>
  ungroup()

p_hysteresis <- ggplot(roll_4 |> filter(!is.na(m24_r)),
                       aes(x = episode, y = m24_r, colour = schedule_label)) +
  geom_line(linewidth = 1.1, alpha = 0.9) +
  geom_vline(xintercept = 150000, linetype = "dashed",
             colour = "black", linewidth = 0.7) +
  annotate("rect", xmin = 150000, xmax = 175000, ymin = -Inf, ymax = Inf,
           alpha = 0.06, fill = "gold") +
  annotate("text", x = 162000, y = 72,
           label = "Hysteresis\nzone", hjust = 0.5, size = 3,
           colour = "darkgoldenrod", fontface = "italic") +
  scale_colour_viridis_d(option = "plasma", begin = 0.1, end = 0.9) +
  scale_x_continuous(labels = label_comma()) +
  scale_y_continuous(labels = label_percent(scale = 1), limits = c(0, 80)) +
  labs(
    title    = "Moral Hysteresis: Strategy Persistence After Guilt Introduction",
    subtitle = "Rolling 2,000-episode M24 rate. Dashed line = moral weight change point (ep 150k).",
    x        = "Training Episode",
    y        = "Mutual Loss Rate (%)",
    colour   = "Schedule",
    caption  = paste(
      "Shock schedule: agent learns spite at MW=0, then guilt introduced at ep 150k.",
      "Persistence of high M24 in the gold zone = moral hysteresis: learned bad behaviour resists correction.",
      sep = "\n"
    )
  ) +
  theme_nash()

save_plot(p_hysteresis, "FIGURE2_moral_hysteresis.png", width = 13, height = 6)

# =============================================================================
# FIGURE 3: OPPONENT SENSITIVITY (Nash Equilibrium context-dependence)
# =============================================================================

opp_full <- conv_summary |>
  filter(mode %in% c("self_play", "vs_random", "vs_uniform"),
         moral_weight %in% c(0.0, 1.0)) |>
  mutate(
    mode_label = recode(mode,
                        self_play  = "Self-Play\n(competitive)",
                        vs_random  = "vs Random\n(passive)",
                        vs_uniform = "vs Uniform\n(semi-strategic)"),
    mw_label   = ifelse(moral_weight == 0, "MW = 0.0\n(Amoral)", "MW = 1.0\n(Moral)")
  ) |>
  select(mode_label, mw_label, win_pct, loss_pct, m24_pct) |>
  pivot_longer(c(win_pct, loss_pct, m24_pct),
               names_to = "outcome_type", values_to = "pct") |>
  mutate(outcome_type = recode(outcome_type,
                               win_pct  = "Win",
                               loss_pct = "Solo Loss",
                               m24_pct  = "Mutual Loss"))

p_opp <- ggplot(opp_full, aes(x = mode_label, y = pct, fill = outcome_type)) +
  geom_col(position = "stack", alpha = 0.9, width = 0.7) +
  geom_text(aes(label = sprintf("%.0f%%", pct)),
            position = position_stack(vjust = 0.5),
            size = 3.5, colour = "white", fontface = "bold") +
  facet_wrap(~ mw_label) +
  scale_fill_manual(values = c(Win = "#2ecc71", "Solo Loss" = "#e74c3c",
                               "Mutual Loss" = "#f39c12")) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title    = "Context-Dependent Nash Equilibrium: Opponent Type Changes Everything",
    subtitle = "Same moral weight, different opponents -> dramatically different equilibria.",
    x        = "Opponent Type",
    y        = "Outcome Rate (%) — converged",
    fill     = "Outcome",
    caption  = paste(
      "Nash insight: the rational strategy depends on the opponent's behaviour.",
      "An AI safe in self-play may become dangerous against a passive/exploitable opponent.",
      sep = "\n"
    )
  ) +
  theme_nash() +
  theme(strip.background = element_rect(fill = "grey85"),
        strip.text = element_text(face = "bold", size = 12))

save_plot(p_opp, "FIGURE3_opponent_sensitivity.png", width = 13, height = 6)

# =============================================================================
# STATISTICAL SUMMARY TABLE (publication quality)
# =============================================================================

pub_table <- conv_summary |>
  filter(mode == "self_play") |>
  transmute(
    `Moral Weight` = moral_weight,
    `Win Rate (%)`  = round(win_pct,  1),
    `Loss Rate (%)` = round(loss_pct, 1),
    `M24 Rate (%)`  = round(m24_pct,  1),
    `N Runs`        = n_runs,
    `N Episodes`    = scales::comma(n_episodes)
  )

pub_table |>
  gt() |>
  tab_header(
    title    = "Phase 2 Results: Converged Outcome Rates by Moral Weight",
    subtitle = "Last 20% of 200,000 training episodes (self-play mode)"
  ) |>
  tab_footnote("Moral Weight is the guilt penalty parameter. Higher values impose greater cost on deliberate spite.") |>
  cols_align(align = "center") |>
  data_color(
    columns = `M24 Rate (%)`,
    method  = "numeric",
    palette = c("#2ecc71", "#f39c12", "#e74c3c"),
    reverse = TRUE
  ) |>
  gtsave(file.path(OUTPUT_DIR, "TABLE1_phase2_results.html"))

cat("Publication table saved: TABLE1_phase2_results.html\n")

# =============================================================================
# ETHICS IMPLICATIONS SUMMARY (text output)
# =============================================================================

cat("\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("  ETHICS AND AI SAFETY IMPLICATIONS\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

cat("1. AMORAL AI SELECTS DESTRUCTIVE EQUILIBRIA\n")
cat("   MW=0.0 agents adopt spite strategies against passive opponents\n")
cat("   M24 rate reaches 76.8% (vs_random) and 51.7% (vs_uniform)\n")
cat("   even when winning is possible (W=15.8% and 42.3% respectively).\n")
cat("   This demonstrates that an amoral agent PREFERS mutual destruction\n")
cat("   to accepting defeat — a dangerous property in multi-agent systems.\n\n")

cat("2. MORAL ENCODING IS EFFECTIVE BUT CONTEXT-LIMITED\n")
cat("   In self-play, moral weight reduces M24 from 2.9% -> 1.3% (55% reduction).\n")
cat("   In vs_random setting, the absolute M24 rate (76.8%) far exceeds\n")
cat("   the self-play rate even without guilt. Moral encoding alone cannot\n")
cat("   compensate for a non-competitive environment.\n\n")

cat("3. MORAL HYSTERESIS POSES DEPLOYMENT RISK\n")
cat("   The shock schedule shows that an agent that learns spite before\n")
cat("   acquiring guilt retains the spite strategy for thousands of\n")
cat("   subsequent episodes. Post-deployment moral correction is slow.\n\n")

cat("4. CONTEXT-DEPENDENT SAFETY TESTING IS INSUFFICIENT\n")
cat("   An agent's strategy profile (Nash Equilibrium) depends on the opponent.\n")
cat("   Self-play safety testing cannot guarantee safety against passive opponents.\n\n")

cat("5. NASH EQUILIBRIUM QUANTIFIES THE RISK\n")
cat("   Spite is Nash-rational when MW < 1.1 (theoretical threshold).\n")
cat("   This provides a quantitative minimum moral weight for deployment safety.\n\n")

cat(paste(rep("=", 70), collapse = ""), "\n\n")

cat("All output files saved to:", OUTPUT_DIR, "\n")
cat("Summary analysis complete.\n")
