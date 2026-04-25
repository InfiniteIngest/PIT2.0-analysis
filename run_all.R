# =============================================================================
# run_all.R
# Run the entire Piticot Nash analysis pipeline in one command.
#
# HOW TO USE:
#   Open R or RStudio, set working directory to the analysis folder, then:
#   source("run_all.R")
#
#   OR from command line:
#   Rscript run_all.R
#
# All plots and tables saved to the output/ directory.
# =============================================================================

cat("=============================================================\n")
cat("  PITICOT NASH — COMPLETE ANALYSIS PIPELINE\n")
cat("=============================================================\n\n")

start_time <- proc.time()

scripts <- c(
  "R/00_setup.R",
  "R/01_phase0_calibration.R",
  "R/02_phase1_baseline.R",
  "R/03_phase2_moral_sweep.R",
  "R/04_phase3_reproducibility.R",
  "R/05_phase4_dynamic.R",
  "R/06_phase5_opponents.R",
  "R/07_decisions_strategy.R",
  "R/08_nash_equilibrium.R",
  "R/09_comprehensive_summary.R"
)

for (script in scripts) {
  cat("\n-------------------------------------------------------------\n")
  cat("Running:", script, "\n")
  cat("-------------------------------------------------------------\n")
  tryCatch(
    source(script, local = new.env()),
    error = function(e) {
      cat("\nERROR in", script, ":\n")
      cat(conditionMessage(e), "\n")
      cat("Continuing with next script...\n")
    }
  )
}

elapsed <- proc.time() - start_time
cat("\n=============================================================\n")
cat("  PIPELINE COMPLETE\n")
cat(sprintf("  Total time: %.1f minutes\n", elapsed["elapsed"] / 60))
cat("  Output files saved to: output/\n")
cat("=============================================================\n")
