# scripts/00_parse_fred_data.R
# Parse + standardize uncertainty indexes from local CSVs (FRED/JLN): VIX, VXO, JLN

source("scripts/setup.R")

# Load all modular FRED/JLN parse functions
source(here::here("functions/parse_fred/load/load_fred_csv.R"))
source(here::here("functions/parse_fred/clean/standardize_fred_indexes.R"))
source(here::here("functions/parse_fred/validate/validate_fred_output.R"))
source(here::here("functions/parse_fred/save/save_fred_uncertainty_csvs.R"))
source(here::here("functions/parse_fred/diagnostics/check_index_coverage.R"))
source(here::here("functions/parse_fred/logs/log_fred_parse_activitiy.R"))
source(here::here("functions/parse_fred/logs/log_fred_metadata_json.R"))

run_parse_fred <- function(
    vix_path = here::here("data","raw","vix.csv"),
    vxo_path = here::here("data","raw","vxo.csv"),
    jln_path = here::here("data","raw","jln_macro_uncertainty.csv"),
    out_dir  = here::here("data","raw"),
    verbose  = TRUE
) {
  dir.create(here::here("figures","parse_fred"), recursive = TRUE, showWarnings = FALSE)
  dir.create(here::here("logs","fred"), recursive = TRUE, showWarnings = FALSE)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  if (verbose) cat("\n Parsing FRED/JLN uncertainty CSVs...\n")
  stopifnot(file.exists(vix_path), file.exists(vxo_path), file.exists(jln_path))
  
  vix <- load_fred_csv(vix_path, index_name = "vix", verbose = verbose)
  vxo <- load_fred_csv(vxo_path, index_name = "vxo", verbose = verbose)
  jln <- load_fred_csv(jln_path, index_name = "jln", verbose = verbose)
  
  df <- standardize_fred_indexes(list(vix=vix, vxo=vxo, jln=jln), verbose = verbose)
  
  validate_fred_output(df, verbose = verbose)
  check_index_coverage(df, save_plot_path = here::here("figures","parse_fred","coverage.png"))
  
  paths <- save_fred_uncertainty_csvs(df, out_dir = out_dir, verbose = verbose)
  
  log_fred_metadata_json(df, inputs = c(vix_path, vxo_path, jln_path),
                         out_dir = here::here("logs","fred"), verbose = verbose)
  log_fred_parse_activity(inputs = c(vix_path, vxo_path, jln_path),
                          output_files = paths, verbose = verbose)
  
  if (verbose) cat("\n Done parsing FRED/JLN. Wrote:", paste(paths, collapse = ", "), "\n")
  invisible(paths)
}

run_parse_fred()
