#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(here); library(fs); library(glue); library(readr); library(dplyr); library(rlang)
})
source(here("scripts","setup.R"))

run <- function(label, path) {
  message("\n--- ", label, " ---")
  tryCatch(source(here(path), local = TRUE),
           error = function(e) { stop(sprintf("[%s] %s", label, conditionMessage(e))) })
}

# 00 — Parsers
run("00 Pink Sheet",          "scripts/00_parse_pink_sheet.R")
run("00 FRED/JLN Uncertainty","scripts/00_parse_fred_data.R")

# Ensure per-index CSVs for step 02
dir_create(here("data","uncertainty"))
make_unc_inputs <- function() {
  p_long <- here("data","raw","uncertainty_fred_long.csv")
  p_wide <- here("data","raw","uncertainty_fred_wide.csv")
  if (file_exists(p_long)) {
    df <- read_csv(p_long, show_col_types = FALSE)
    if (all(c("date","index","value") %in% names(df))) {
      for (k in c("vix","vxo","jln")) {
        sub <- df %>% filter(tolower(index) == k) %>% select(date, value)
        if (nrow(sub)) write_csv(sub, here("data","uncertainty", paste0(k,".csv")))
      }
      return(invisible())
    }
  }
  if (file_exists(p_wide)) {
    dfw <- read_csv(p_wide, show_col_types = FALSE)
    for (k in c("vix","vxo","jln")) if (k %in% names(dfw)) {
      write_csv(dfw %>% select(date, value = !!sym(k)),
                here("data","uncertainty", paste0(k,".csv")))
    }
  }
}
make_unc_inputs()

# 01 — Filter panel
run("01 Filter commodity panel", "scripts/01_filter_commodity_panel.R")

# 02 — Merge uncertainty indexes
run("02 Merge uncertainty", "scripts/02_merge_uncertainty_index.R")

# Config guards
if (!file_exists(here("config","paths.yml"))) stop("Missing config/paths.yml. Create it (see sample in README).")
if (!file_exists(here("config","tvar.yaml"))) stop("Missing config/tvar.yaml. Create it (see sample in README).")

# 03–09
run("03 Prepare TVAR input",       "scripts/03_prepare_panel_for_tvar.R")
run("04 Estimate TVAR",            "scripts/04_estimate_tvar.R")
run("05 Analyze TVAR + IRFs",      "scripts/05_analyze_tvar_model.R")
run("06 Regime dynamics",          "scripts/06_analyze_regime_dynamics.R")
run("07 Scenario simulations",     "scripts/07_shock_scenario_simulations.R")
run("08 Forecast decomposition",   "scripts/08_forecast_decomposition.R")
run("09 Package & export release", "scripts/09_package_export.R")

message("\nAll steps completed successfully.")
