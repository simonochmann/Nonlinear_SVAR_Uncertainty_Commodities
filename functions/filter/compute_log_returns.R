#' Compute Log Returns for Commodity Panel (Elite Version)
#'
#' Calculates log returns (log(price_t / price_{t-1})) per commodity,
#' with robust NA handling, strict validation, and optional return of metadata.
#'
#' @param df A data.frame or tibble with `commodity`, `date`, and `price` columns.
#' @param return_metadata Logical. If TRUE, return a list with data and diagnostics. Default = FALSE.
#' @param verbose Logical. Print warnings or summary. Default = TRUE.
#'
#' @return A tibble with added `log_return` column, or list(data = ..., meta = ...) if return_metadata = TRUE.
#' @export
compute_log_returns <- function(df, return_metadata = FALSE, verbose = TRUE) {
  stopifnot(is.data.frame(df))
  
  # Validate Required Columns
  required_cols <- c("commodity", "date", "price")
  missing_cols <- setdiff(required_cols, colnames(df))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Type Checks
  if (!inherits(df$date, "Date")) stop("`date` column must be of class Date.")
  if (!is.numeric(df$price)) stop("`price` column must be numeric.")
  
  # Warn on Zero or Negative Prices 
  if (any(df$price <= 0, na.rm = TRUE) && verbose) {
    warning("Some price values are ≤ 0 — log returns may be undefined.")
  }
  
  # Compute Log Returns 
  df_out <- df %>%
    dplyr::arrange(commodity, date) %>%
    dplyr::group_by(commodity) %>%
    dplyr::mutate(
      log_return = dplyr::case_when(
        is.na(price) | is.na(dplyr::lag(price)) ~ NA_real_,
        dplyr::lag(price) <= 0 ~ NA_real_,
        TRUE ~ log(price / dplyr::lag(price))
      )
    ) %>%
    dplyr::ungroup()
  
  # Metadata Summary 
  if (return_metadata) {
    meta <- list(
      n_obs = nrow(df_out),
      n_commodities = dplyr::n_distinct(df_out$commodity),
      pct_missing_log_return = round(mean(is.na(df_out$log_return)) * 100, 2),
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
    )
    if (verbose) {
      cat("Log return calculation complete.\n")
      cat("Missing log returns: ", meta$pct_missing_log_return, "%\n")
    }
    return(list(data = df_out, meta = meta))
  }
  
  return(df_out)
}
