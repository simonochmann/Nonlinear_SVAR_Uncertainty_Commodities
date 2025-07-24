#' Extract and Return Date Range of Dataset
#'
#' Computes the start and end date from a `date` column in a long-format tibble.
#' Adds optional logging, warning for excessive missingness, and pipeline integration.
#'
#' @param df A tibble or data.frame with a `date` column.
#' @param verbose Logical. If TRUE, prints human-readable output (default: TRUE).
#' @param return_type Output type: "tibble" (default) or "list".
#' @param log_file Optional file path. If supplied, appends the range and timestamp.
#'
#' @return A tibble or list with `start`, `end`, and `n_obs` fields.
#' @export
#'
#' @examples
#' extract_date_range(df_long)
#' extract_date_range(df_long, return_type = "list", verbose = FALSE)

extract_date_range <- function(df,
                               verbose = TRUE,
                               return_type = c("tibble", "list"),
                               log_file = NULL) {
  stopifnot("date" %in% names(df))
  return_type <- match.arg(return_type)
  
  # Ensure date format
  df$date <- as.Date(df$date)
  valid_dates <- df %>% dplyr::filter(!is.na(date))
  
  # Warn or fail if mostly missing
  if (nrow(valid_dates) < 0.05 * nrow(df)) {
    stop("extract_date_range(): Over 95% of 'date' column is missing.")
  }
  
  # Compute range
  start_date <- min(valid_dates$date)
  end_date   <- max(valid_dates$date)
  n_obs      <- nrow(valid_dates)
  
  # Construct result
  result_tbl <- tibble::tibble(
    start = start_date,
    end   = end_date,
    n_obs = n_obs
  )
  
  # Optional message
  if (verbose) {
    cat(glue::glue("Date Range: {start_date} → {end_date} ({n_obs} non-NA rows)\n"))
  }
  
  # Optional logging
  if (!is.null(log_file)) {
    msg <- glue::glue("{Sys.time()} | Parsed date range: {start_date} → {end_date} | n = {n_obs}")
    write(msg, file = log_file, append = TRUE)
  }
  
  # Return in requested format
  if (return_type == "tibble") {
    return(result_tbl)
  } else {
    return(list(start = start_date, end = end_date, n_obs = n_obs))
  }
}
