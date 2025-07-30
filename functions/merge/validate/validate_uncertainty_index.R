#' Validate a Cleaned Uncertainty Index Object
#'
#' Performs comprehensive structural, statistical, and temporal validation
#' of a cleaned uncertainty index dataframe with columns `date` and `value`.
#'
#' @param df A tibble from `clean_uncertainty_index()`.
#' @param allow_negative Logical. Allow negative values? Default: FALSE.
#' @param verbose Logical. Print check results? Default: TRUE.
#'
#' @return Invisibly TRUE if valid. Adds metadata as attributes.
#' @export
#'
#' @examples
#' validate_uncertainty_index(df_vix)
validate_uncertainty_index <- function(df,
                                       allow_negative = FALSE,
                                       verbose = TRUE) {
  # Structure checks
  if (!tibble::is_tibble(df)) stop("Input is not a tibble.")
  if (!all(c("date", "value") %in% names(df))) stop("Missing `date` and/or `value` columns.")
  if (!inherits(df$date, "Date")) stop("`date` column must be class <Date>.")
  if (!is.numeric(df$value)) stop("`value` column must be numeric.")
  
  # Row count check 
  if (nrow(df) < 10) stop("Too few observations (< 10).")
  
  # Monotonic time check 
  if (any(diff(df$date) <= 0)) stop("Dates are not strictly increasing.")
  
  # Duplicate dates 
  if (any(duplicated(df$date))) stop("Duplicate dates detected.")
  
  # Missing & infinite values
  na_share <- mean(is.na(df$value))
  inf_share <- mean(is.infinite(df$value))
  if (na_share > 0) stop(glue::glue("{round(na_share * 100, 2)}% of values are NA."))
  if (inf_share > 0) stop(glue::glue("{round(inf_share * 100, 2)}% of values are Inf."))
  
  # Negative values 
  neg_share <- mean(df$value < 0, na.rm = TRUE)
  if (!allow_negative && neg_share > 0) {
    stop(glue::glue("Negative values detected ({round(neg_share * 100, 2)}%), but `allow_negative = FALSE`."))
  }
  
  # Constant or near-constant values 
  if (sd(df$value) < 1e-6) {
    warning("Uncertainty index has near-zero variance — possible constant series.")
  }
  
  # Frequency & Gaps
  date_diffs <- as.integer(diff(df$date))
  freq_table <- sort(table(date_diffs), decreasing = TRUE)
  freq_mode <- as.integer(names(freq_table)[1])
  gap_detected <- length(freq_table) > 1 || any(date_diffs > freq_mode)
  
  # Value outlier check 
  q_vals <- quantile(df$value, probs = c(0.01, 0.5, 0.99), na.rm = TRUE)
  outlier_range <- q_vals["99%"] - q_vals["1%"]
  extreme_value_ratio <- max(abs(df$value)) / (outlier_range + 1e-5)
  if (extreme_value_ratio > 10) {
    warning("Detected extreme values (value range >10× interquantile spread). Check for spikes.")
  }
  
  # Metadata summary 
  metadata <- list(
    n_obs = nrow(df),
    start_date = min(df$date),
    end_date = max(df$date),
    date_range_days = as.numeric(max(df$date) - min(df$date)),
    freq_mode_days = freq_mode,
    has_gaps = gap_detected,
    na_pct = round(na_share * 100, 2),
    neg_pct = round(neg_share * 100, 2),
    outlier_score = round(extreme_value_ratio, 2)
  )
  
  # Print summary 
  if (verbose) {
    message("Uncertainty index validation passed.")
    message(glue::glue("Observations: {metadata$n_obs}"))
    message(glue::glue("Date range: {metadata$start_date} → {metadata$end_date}"))
    message(glue::glue("Frequency mode: {metadata$freq_mode_days} days"))
    if (gap_detected) {
      message(glue::glue("Gaps or irregular frequency detected: {toString(names(freq_table))}"))
    }
    message(glue::glue("Negative values: {metadata$neg_pct}%"))
    message(glue::glue("Outlier score: {metadata$outlier_score}"))
  }
  
  # Attach attributes for downstream use 
  attr(df, "validation_metadata") <- metadata
  
  return(invisible(TRUE))
}
