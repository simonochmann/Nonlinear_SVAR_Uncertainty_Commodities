#' Check Missingness Statistics by Commodity
#'
#' Computes diagnostics on missing values per commodity: count, percent, flag, and date span.
#'
#' @param df A tibble with columns: `date`, `commodity`, `price`.
#' @param coverage_threshold Minimum % of non-missing values to pass (default = 0.8).
#' @param verbose Logical. If TRUE, prints the summary table (default: TRUE).
#' @param return_type "tibble" (default), "list", or "both".
#' @param log_file Optional path to log flagged series.
#'
#' @return A tibble of summary stats, or a list including flagged commodities.
#' @export
#'
#' @examples
#' check_missingness_stats(df_long)

check_missingness_stats <- function(df,
                                    coverage_threshold = 0.8,
                                    verbose = TRUE,
                                    return_type = c("tibble", "list", "both"),
                                    log_file = NULL) {
  return_type <- match.arg(return_type)
  
  # Safety checks
  if (!inherits(df, "data.frame")) stop("`df` must be a data.frame or tibble.")
  required_cols <- c("date", "commodity", "price")
  if (!all(required_cols %in% names(df))) stop("Missing required columns: ", paste(setdiff(required_cols, names(df)), collapse = ", "))
  
  df <- tibble::as_tibble(df)
  
  # Compute stats
  stats_tbl <- df %>%
    dplyr::group_by(commodity) %>%
    dplyr::summarise(
      n_total = dplyr::n(),
      n_missing = sum(is.na(price)),
      pct_missing = round(n_missing / n_total, 4),
      flag_low_coverage = pct_missing > (1 - coverage_threshold),
      date_start = min(date, na.rm = TRUE),
      date_end   = max(date, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(desc(pct_missing))
  
  # Flagged list for downstream use
  flagged_commodities <- stats_tbl %>%
    dplyr::filter(flag_low_coverage) %>%
    dplyr::pull(commodity)
  
  # Optional logging
  if (!is.null(log_file)) {
    dir.create(dirname(log_file), recursive = TRUE, showWarnings = FALSE)
    log_msgs <- stats_tbl %>%
      dplyr::filter(flag_low_coverage) %>%
      dplyr::mutate(
        log_msg = glue::glue("[{Sys.time()}] {commodity}: {round(100 * (1 - pct_missing), 1)}% coverage ({n_missing} missing)")
      ) %>%
      dplyr::pull(log_msg)
    writeLines(log_msgs, log_file, sep = "\n", useBytes = TRUE)
  }
  
  # Optional console output
  if (verbose) {
    cat(glue::glue("Missingness Summary ({nrow(stats_tbl)} commodities)\n"))
    print(stats_tbl, n = Inf)
    if (length(flagged_commodities) > 0) {
      cat(glue::glue("\n Commodities below {coverage_threshold*100}% coverage:\n"))
      print(flagged_commodities)
    }
  }
  
  # Return
  if (return_type == "tibble") return(stats_tbl)
  if (return_type == "list") return(list(stats = stats_tbl, flagged = flagged_commodities))
  if (return_type == "both") return(list(stats = stats_tbl, flagged = flagged_commodities))
}
