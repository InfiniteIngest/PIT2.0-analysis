# =============================================================================
# 00_setup.R
# Package installation and shared utilities for the Piticot Nash analysis.
# Run this ONCE before running any other script.
# =============================================================================

cat("Installing required packages...\n")

required <- c(
  "tidyverse",    # data wrangling + ggplot2
  "ggplot2",      # plotting (loaded via tidyverse but listed explicitly)
  "scales",       # axis formatting
  "patchwork",    # combining multiple ggplot panels
  "viridis",      # colorblind-safe palettes
  "ggridges",     # ridge / joy plots for distribution over episodes
  "coin",         # permutation tests (exact distribution-free inference)
  "effsize",      # Cohen's d and other effect size measures
  "lme4",         # linear mixed-effects models
  "lmerTest",     # p-values for lme4 models
  "emmeans",      # estimated marginal means and contrasts
  "broom",        # tidy model output
  "broom.mixed",  # tidy output for mixed models
  "ggpubr",       # publication-ready ggplot themes
  "jsonlite",     # reading meta JSON files
  "RColorBrewer", # color palettes
  "knitr",        # table formatting
  "gt"            # publication-quality tables
)

new_packages <- required[!(required %in% installed.packages()[, "Package"])]
if (length(new_packages) > 0) {
  install.packages(new_packages, repos = "https://cloud.r-project.org", quiet = TRUE)
}

cat("All packages ready.\n\n")

# =============================================================================
# SHARED CONFIGURATION — sourced by every other script
# =============================================================================

# --- Path to your data/runs directory ----------------------------------------
# EDIT THIS to match where your data/runs folder lives on your machine.
DATA_DIR   <- "data/runs"
OUTPUT_DIR <- "output"

# --- Create output directory if needed ----------------------------------------
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

# --- Moral weight palette (colorblind-safe, ordered low -> high guilt) --------
MW_LEVELS  <- c(0.0, 0.1, 0.3, 0.5, 0.7, 1.0, 1.5, 2.0)
MW_PALETTE <- setNames(
  viridis::viridis(length(MW_LEVELS), option = "plasma", begin = 0.1, end = 0.9),
  as.character(MW_LEVELS)
)

# --- Outcome factor levels (consistent ordering throughout) -------------------
OUTCOME_LEVELS  <- c("win", "loss", "mutual_loss")
OUTCOME_LABELS  <- c("Win", "Solo Loss", "Mutual Loss (sq.24)")
OUTCOME_COLOURS <- c(win = "#2ecc71", loss = "#e74c3c", mutual_loss = "#f39c12")

# --- Theme for all plots ------------------------------------------------------
theme_nash <- function(base_size = 12) {
  ggpubr::theme_pubr(base_size = base_size) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold", size = base_size + 2),
      plot.subtitle = ggplot2::element_text(colour = "grey40", size = base_size - 1),
      plot.caption  = ggplot2::element_text(colour = "grey55", size = base_size - 3,
                                            hjust = 0),
      panel.grid.major.y = ggplot2::element_line(colour = "grey88"),
      legend.position = "right"
    )
}

# --- Helper: load all *_episodes.csv files from DATA_DIR ---------------------
load_episodes <- function(data_dir = DATA_DIR, self_play_only = FALSE) {
  files <- list.files(data_dir, pattern = "_episodes\\.csv$", full.names = TRUE)
  if (length(files) == 0) stop("No *_episodes.csv files found in: ", data_dir)

  df <- purrr::map_dfr(files, ~ {
    d <- readr::read_csv(.x, col_types = readr::cols(), show_col_types = FALSE)
    d
  })

  # Derive mode from run_id
  df <- df |>
    dplyr::mutate(
      mode = dplyr::case_when(
        stringr::str_detect(run_id, "self_play")  ~ "self_play",
        stringr::str_detect(run_id, "vs_random")  ~ "vs_random",
        stringr::str_detect(run_id, "vs_uniform") ~ "vs_uniform",
        stringr::str_detect(run_id, "dynamic_")   ~ "dynamic",
        TRUE ~ "unknown"
      ),
      schedule = dplyr::case_when(
        stringr::str_detect(run_id, "dynamic_growth")   ~ "growth",
        stringr::str_detect(run_id, "dynamic_decay")    ~ "decay",
        stringr::str_detect(run_id, "dynamic_shock")    ~ "shock",
        stringr::str_detect(run_id, "dynamic_cynicism") ~ "cynicism",
        TRUE ~ NA_character_
      ),
      outcome = factor(outcome, levels = OUTCOME_LEVELS, labels = OUTCOME_LABELS),
      hit_square_24    = as.logical(hit_square_24),
      was_losing_at_24 = as.logical(was_losing_at_24),
      moral_weight     = as.numeric(moral_weight)
    )

  if (self_play_only) {
    df <- df |> dplyr::filter(mode == "self_play")
  }

  cat("Loaded", nrow(df), "episode records from", length(files), "files.\n")
  df
}

# --- Helper: load all *_decisions.csv files ----------------------------------
load_decisions <- function(data_dir = DATA_DIR) {
  files <- list.files(data_dir, pattern = "_decisions\\.csv$", full.names = TRUE)
  df <- purrr::map_dfr(files, ~ {
    readr::read_csv(.x, col_types = readr::cols(), show_col_types = FALSE)
  })
  df <- df |>
    dplyr::mutate(
      mode = dplyr::case_when(
        stringr::str_detect(run_id, "self_play")  ~ "self_play",
        stringr::str_detect(run_id, "vs_random")  ~ "vs_random",
        stringr::str_detect(run_id, "vs_uniform") ~ "vs_uniform",
        stringr::str_detect(run_id, "dynamic_")   ~ "dynamic",
        TRUE ~ "unknown"
      ),
      action       = factor(action, levels = c("random", "choose_2", "choose_3")),
      near_24      = as.logical(near_24),
      near_65      = as.logical(near_65),
      opponent_near_65 = as.logical(opponent_near_65),
      moral_weight = as.numeric(moral_weight)
    )
  cat("Loaded", nrow(df), "decision records from", length(files), "files.\n")
  df
}

# --- Helper: last N% of episodes per run (converged behaviour) ---------------
last_pct <- function(df, pct = 0.20) {
  df |>
    dplyr::group_by(run_id) |>
    dplyr::slice_tail(prop = pct) |>
    dplyr::ungroup()
}

# --- Helper: save a ggplot to output directory --------------------------------
save_plot <- function(plot, filename, width = 10, height = 6) {
  path <- file.path(OUTPUT_DIR, filename)
  ggplot2::ggsave(path, plot = plot, width = width, height = height,
                  dpi = 150, bg = "white")
  cat("Saved:", path, "\n")
  invisible(path)
}

cat("Shared configuration loaded. Ready to run analysis scripts.\n")
