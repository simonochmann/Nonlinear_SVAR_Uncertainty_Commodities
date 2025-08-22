# scripts/00_parse_pink_sheet.R
# Orchestrates parsing of World Bank Pink Sheet data
# into standardized, clean, and validated CSV + log outputs

# Load setup and function paths (use here())
source(here::here("scripts", "setup.R"))

# Load all modular parse functions
source(here::here("functions/parse/load/load_pink_sheet_excel.R"))
source(here::here("functions/parse/clean/validate_column_types.R"))
source(here::here("functions/parse/clean/standardize_commodity_names.R"))
source(here::here("functions/parse/clean/reshape_to_long_format.R"))
source(here::here("functions/parse/save/save_commodity_csvs.R"))
source(here::here("functions/parse/diagnostics/generate_missingness_heatmap.R"))
source(here::here("functions/parse/diagnostics/extract_date_range.R"))
source(here::here("functions/parse/diagnostics/check_missingness_stats.R"))
source(here::here("functions/parse/logs/log_parse_activity.R"))
source(here::here("functions/parse/logs/log_parse_metadata_json.R"))
source(here::here("functions/parse/validate/validate_parsed_output.R"))

# Main pipeline wrapper
run_parse_pipeline <- function(
    excel_path = here::here("data", "external", "Monthly_Prices_Pink_Sheet.xlsx"),
    sheet      = "Monthly Prices",
    output_dir = here::here("data", "raw"),
    verbose    = TRUE
) {
  start_time <- Sys.time()
  
  # Ensure expected directories exist
  dir.create(here::here("figures", "parse"), recursive = TRUE, showWarnings = FALSE)
  dir.create(here::here("logs"), recursive = TRUE, showWarnings = FALSE)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  if (verbose) cat("\nStarting Pink Sheet Parse Pipeline...\n")
  
  # Guard file existence 
  if (!file.exists(excel_path)) {
    stop("Pink Sheet not found: ", excel_path)
  }
  
  # 1. Load raw Excel data 
  df_raw <- tryCatch(
    load_pink_sheet_excel(excel_path, sheet, verbose = verbose),
    error = function(e) {
      stop("Failed to load Pink Sheet (", basename(excel_path), "): ", conditionMessage(e))
    }
  )
  
  # 2. Validate column types
  df_valid <- validate_column_types(df_raw, allow_NA = TRUE, verbose = TRUE)
  
  # 3. Standardize names
  df_named <- standardize_commodity_names(df_valid, verbose = verbose)
  
  # 4. Reshape to long format (no external CSV dependency)
  df_long <- tryCatch(
    reshape_to_long_format(df_named, verbose = verbose),
    error = function(e) stop("reshape_to_long_format() failed: ", conditionMessage(e))
  )
  
  # Optionally check study-set coverage (keep this nice reporting)
  targets <- c("crude_wti","ngas_us","gold","platinum","silver",
               "cocoa","coffee_arabic","maize","cotton_a_indx","soybeans",
               "sugar_wld","wheat_us_hrw","aluminium","copper","lead",
               "nickel","tin","zinc")
  missing <- setdiff(targets, tolower(unique(df_long$commodity)))
  cat("Missing targets:", paste(missing, collapse = ", "), "\n")
  
  # 5. Validate long-format structure
  validate_parsed_output(df_long, verbose = verbose)
  
  # 6. Run diagnostics (Fix #6: use here() for plot path)
  generate_missingness_heatmap(
    df_long,
    save_plot_path = here::here("figures", "parse", "missingness_heatmap.png")
  )
  check_missingness_stats(df_long, verbose = verbose)
  # Echo date range to console
  dr <- extract_date_range(df_long, verbose = FALSE)
  if (verbose && !is.null(dr)) {
    cat("Detected date range: ", paste0(dr$start, " to ", dr$end), "\n")
  }
  
  # 7. Save cleaned long + wide CSVs (Fix #2/#4 already applied globally)
  output_files <- save_commodity_csvs(
    df_long      = df_long,
    dir_out      = output_dir,
    verbose      = verbose,
    return_paths = TRUE
  )
  
  # 8. Log structured JSON metadata
  log_parse_metadata_json(
    df         = df_long,
    input_file = excel_path,
    output_dir = output_dir,
    out_json   = NULL,  
    verbose    = verbose
  )
  
  # 9. Log CSV activity log (Fix #7: here() for path)
  log_parse_activity(
    input_file   = excel_path,
    output_files = output_files,
    note         = "Initial Pink Sheet parse with diagnostics",
    tags         = c("pink_sheet", "parse", "v1"),
    log_path     = here::here("logs", "parse_log.csv"),
    verbose      = verbose
  )
  
  # Final message
  if (verbose) {
    cat("\nFinished Parse Pipeline in",
        round(difftime(Sys.time(), start_time, units = "secs"), 2), "seconds\n")
  }
  
  invisible(TRUE)
}

# Execute the pipeline
run_parse_pipeline()