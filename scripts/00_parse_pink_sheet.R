# scripts/00_parse_pink_sheet.R
# Orchestrates parsing of World Bank Pink Sheet data
# into standardized, clean, and validated CSV + log outputs

# Load setup and function paths
source("scripts/setup.R")

# Load all modular parse functions
source("functions/parse/load/load_pink_sheet_excel.R")
source("functions/parse/clean/validate_column_types.R")
source("functions/parse/clean/standardize_commodity_names.R")
source("functions/parse/clean/reshape_to_long_format.R")
source("functions/parse/save/save_commodity_csvs.R")
source("functions/parse/diagnostics/generate_missingness_heatmap.R")
source("functions/parse/diagnostics/extract_date_range.R")
source("functions/parse/diagnostics/check_missingness_stats.R")
source("functions/parse/logs/log_parse_activity.R")
source("functions/parse/logs/log_parse_metadata_json.R")

# Main pipeline wrapper
run_parse_pipeline <- function(excel_path = "data/external/Monthly_Prices_Pink_Sheet.xlsx",
                               sheet = "Monthly Prices",
                               output_dir = "data/raw/",
                               verbose = TRUE) {
  start_time <- Sys.time()
  
  if (verbose) cat("\n Starting Pink Sheet Parse Pipeline...\n")
  
  # 1. Load raw Excel data
  df_raw <- load_pink_sheet_excel(excel_path, sheet, verbose = verbose)
  
  # 2. Validate column types
  df_valid <- validate_column_types(df_raw, verbose = verbose)
  
  # 3. Standardize names
  df_named <- standardize_commodity_names(df_valid, verbose = verbose)
  
  # 4. Reshape to long format
  df_long <- reshape_to_long_format(df_named, verbose = verbose)
  
  # 5. Validate long-format structure
  validate_parsed_output(df_long, verbose = verbose)
  
  # 6. Run diagnostics
  generate_missingness_heatmap(
    df_long,
    save_plot_path = "figures/parse/missingness_heatmap.png"
  )
  check_missingness_stats(df_long, verbose = verbose)
  extract_date_range(df_long, verbose = verbose)
  
  # 7. Save cleaned long + wide CSVs
  output_files <- save_commodity_csvs(
    df_long = df_long,
    dir_out = output_dir,
    verbose = verbose,
    return_paths = TRUE
  )
  
  # 8. Log structured JSON metadata
  log_parse_metadata_json(
    df = df_long,
    input_file = excel_path,
    output_dir = output_dir,
    out_json = NULL,  # auto-timestamped
    verbose = verbose
  )
  
  # 9. Log CSV activity log
  log_parse_activity(
    input_file = excel_path,
    output_files = output_files,
    note = "Initial Pink Sheet parse with diagnostics",
    tags = c("pink_sheet", "parse", "v1"),
    log_path = "logs/parse_log.csv",
    verbose = verbose
  )
  
  # Final message
  if (verbose) {
    cat("\n Finished Parse Pipeline in",
        round(difftime(Sys.time(), start_time, units = "secs"), 2), "seconds\n")
  }
  
  return(invisible(TRUE))
}

# Execute the pipeline
run_parse_pipeline()
