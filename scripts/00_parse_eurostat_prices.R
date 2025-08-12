# scripts/00_parse_eurostat.R
# Parse + standardize ECB CISS from local CSV (Eurostat/ECB SDW extract)

source("scripts/setup.R")

# Load modular Eurostat CISS parse functions
source(here::here("functions/parse_eurostat/load/load_ciss_csv.R"))
source(here::here("functions/parse_eurostat/clean/clean_ciss_schema.R"))
source(here::here("functions/parse_eurostat/validate/validate_ciss_output.R"))
source(here::here("functions/parse_eurostat/save/save_ciss_csv.R"))
source(here::here("functions/parse_eurostat/diagnostics/check_ciss_coverage.R"))
source(here::here("functions/parse_eurostat/logs/log_eurostat_parse_activity.R"))
source(here::here("functions/parse_eurostat/logs/log_eurostat_metadata_json.R"))

run_parse_ciss <- function(
    ciss_path = here::here("data","raw","ecb_ciss.csv"),
    out_dir   = here::here("data","raw"),
    verbose   = TRUE
) {
  dir.create(here::here("figures","parse_eurostat"), recursive = TRUE, showWarnings = FALSE)
  dir.create(here::here("logs","eurostat"), recursive = TRUE, showWarnings = FALSE)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  if (!file.exists(ciss_path)) stop("CISS CSV not found: ", ciss_path)
  if (verbose) cat("\n Parsing ECB CISS...\n")
  
  raw  <- load_ciss_csv(ciss_path, verbose = verbose)
  ciss <- clean_ciss_schema(raw, verbose = verbose)
  validate_ciss_output(ciss, verbose = verbose)
  check_ciss_coverage(ciss, save_plot_path = here::here("figures","parse_eurostat","ciss_coverage.png"))
  paths <- save_ciss_csv(ciss, out_dir = out_dir, verbose = verbose)
  
  log_eurostat_metadata_json(ciss, input = ciss_path, out_dir = here::here("logs","eurostat"), verbose = verbose)
  log_eurostat_parse_activity(input = ciss_path, output_files = paths, verbose = verbose)
  
  if (verbose) cat("\n Done parsing CISS. Wrote:", paste(paths, collapse = ", "), "\n")
  invisible(paths)
}

run_parse_ciss()
