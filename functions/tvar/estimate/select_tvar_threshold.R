#' Select and Prepare Threshold Variable for TVAR Estimation
#'
#' @param df A data.frame with a 'date' column and numeric variables.
#' @param threshold_var Name of the threshold variable (character).
#' @param lag Integer lag applied to threshold variable (default: 0).
#' @param scale Logical, whether to z-score standardize the threshold variable (default: TRUE).
#' @param threshold_method Character. One of: "raw" (default), "quantile", "fixed", "rolling_mean"
#' @param method_param Parameter for the method: e.g. quantile level (0.5), fixed value (0), or rolling window size.
#' @param verbose Logical, print internal steps (default: FALSE).
#'
#' @return A list with threshold values and metadata: values, mean, sd, split_rule, valid_split.
#' @export
select_tvar_threshold <- function(df,
                                  threshold_var,
                                  lag = 0,
                                  scale = TRUE,
                                  threshold_method = "raw",
                                  method_param = NULL,
                                  verbose = FALSE) {
  
  stopifnot("date" %in% names(df))
  stopifnot(threshold_var %in% names(df))
  
  threshold <- df[[threshold_var]]
  
  if (verbose) message("Raw threshold (head): ", paste(head(round(threshold, 2)), collapse = ", "))
  
  # Apply lag
  if (lag > 0) {
    threshold <- dplyr::lag(threshold, lag)
    if (verbose) message("→ Applied lag of ", lag, " to threshold variable.")
  }
  
  # Drop NA from lag
  threshold <- zoo::na.trim(threshold)
  
  # Optional scale
  if (scale) {
    threshold <- scale(threshold)[, 1]
    if (verbose) message("→ Scaled threshold variable (z-score).")
  }
  
  # Apply method-specific transformation or diagnostic
  split_rule <- NULL
  if (threshold_method == "quantile") {
    q <- ifelse(is.null(method_param), 0.5, method_param)
    split_rule <- quantile(threshold, q, na.rm = TRUE)
  } else if (threshold_method == "fixed") {
    split_rule <- ifelse(is.null(method_param), 0, method_param)
  } else if (threshold_method == "rolling_mean") {
    window <- ifelse(is.null(method_param), 6, method_param)
    split_rule <- zoo::rollmean(threshold, k = window, fill = NA, align = "right")
  }
  
  # Validate regime split (if applicable)
  valid_split <- TRUE
  if (!is.null(split_rule)) {
    above <- sum(threshold > split_rule, na.rm = TRUE)
    below <- sum(threshold <= split_rule, na.rm = TRUE)
    valid_split <- above > 0 && below > 0
    if (verbose) {
      message("→ Regime split at: ", round(split_rule, 3))
      message("Regime 1 (<=): ", below)
      message("Regime 2 (>): ", above)
      if (!valid_split) warning("Regime split may not be meaningful: one side is empty.")
    }
  }
  
  return(list(
    values       = threshold,
    mean         = mean(threshold, na.rm = TRUE),
    sd           = sd(threshold, na.rm = TRUE),
    split_rule   = split_rule,
    valid_split  = valid_split
  ))
}
