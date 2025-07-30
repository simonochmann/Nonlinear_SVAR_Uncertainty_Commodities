#' Validate TVAR Input Panel
#'
#' Ensures the input panel for TVAR is well-structured and ready:
#' wide-format time series with `date` column and ≥2 numeric variables,
#' no NAs, and regular frequency. Returns full diagnostics.
#'
#' @param df A tibble with columns: `date`, `var1`, ..., `varN`.
#' @param min_vars Minimum number of numeric variables (excl. date). Default: 2.
#' @param expected_frequency Optional. One of "monthly", "weekly", "daily". Default: NULL (no check).
#' @param log_path Optional file path to write validation log (as .json). Default: NULL.
#' @param verbose Logical. Print summary? Default: TRUE.
#'
#' @return Invisible list with metadata: n_obs, n_vars, start_date, end_date, frequency, NA_report
#' @export
validate_tvar_input <- function(df,
                                min_vars = 2,
                                expected_frequency = NULL,
                                log_path = NULL,
                                verbose = TRUE) {
  stopifnot(inherits(df, "data.frame"))
  stopifnot("date" %in% names(df))
  
  # Ensure date is Date type
  if (!inherits(df$date, "Date")) {
    stop("Column `date` must be of class 'Date'.")
  }
  
  # Identify numeric variables
  numeric_cols <- names(df)[sapply(df, is.numeric)]
  numeric_vars <- setdiff(numeric_cols, "date")
  
  if (length(numeric_vars) < min_vars) {
    stop(glue::glue("Panel must contain at least {min_vars} numeric variables. Found: {length(numeric_vars)}."))
  }
  
  # NA summary
  na_report <- purrr::map_int(df[numeric_vars], ~ sum(is.na(.x)))
  if (any(na_report > 0)) {
    na_names <- names(na_report[na_report > 0])
    stop(glue::glue("NA values found in: {paste(na_names, collapse = ', ')}"))
  }
  
  # Date frequency check
  date_diffs <- as.numeric(diff(df$date))
  freq_mode <- as.integer(stats::median(date_diffs))  # robust to outliers
  
  freq_label <- dplyr::case_when(
    freq_mode %in% 28:31 ~ "monthly",
    freq_mode %in% 6:8 ~ "weekly",
    freq_mode == 1 ~ "daily",
    TRUE ~ paste0(freq_mode, "-day interval")
  )
  
  # Check expected frequency if supplied
  if (!is.null(expected_frequency)) {
    freq_match <- switch(expected_frequency,
                         "monthly" = freq_mode %in% 28:31,
                         "weekly"  = freq_mode %in% 6:8,
                         "daily"   = freq_mode == 1,
                         FALSE)
    if (!freq_match) {
      stop(glue::glue("Expected frequency '{expected_frequency}' does not match detected mode: {freq_mode} days"))
    }
  }
  
  # Summary output
  diagnostics <- list(
    n_obs       = nrow(df),
    n_vars      = length(numeric_vars),
    start_date  = min(df$date),
    end_date    = max(df$date),
    frequency   = freq_label,
    mode_diff   = freq_mode,
    na_report   = na_report
  )
  
  if (verbose) {
    cli::cli_alert_success("TVAR input panel validated.")
    cli::cli_text("Obs: {diagnostics$n_obs} | Vars: {diagnostics$n_vars}")
    cli::cli_text("Range: {diagnostics$start_date} → {diagnostics$end_date}")
    cli::cli_text("Frequency: {diagnostics$frequency} (mode: {diagnostics$mode_diff} days)")
  }
  
  # Optional JSON log
  if (!is.null(log_path)) {
    jsonlite::write_json(diagnostics, path = log_path, pretty = TRUE, auto_unbox = TRUE)
    if (verbose) cli::cli_alert_info("Validation log saved to {log_path}")
  }
  
  return(invisible(diagnostics))
}
