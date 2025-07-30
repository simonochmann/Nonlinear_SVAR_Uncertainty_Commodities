#' Check Metadata Consistency of a Cleaned Uncertainty Index
#'
#' Performs a comprehensive set of metadata checks:
#' - Structure, types, missing values, duplicates
#' - Time coverage, gaps, and frequency
#' - Basic content integrity (negatives, constancy, etc.)
#'
#' @param df A tibble with `date` and `value`.
#' @param index_name Character, used for logging.
#'
#' @return A named list of validation metadata and results.
#' @export
check_uncertainty_metadata <- function(df, index_name = "Unnamed Index") {
  results <- list(index = index_name)
  warnings <- c()
  errors <- c()
  
  # 1. Structural Checks
  if (!tibble::is_tibble(df)) errors <- c(errors, "Data is not a tibble.")
  if (!all(c("date", "value") %in% names(df))) errors <- c(errors, "Missing required columns.")
  if (!inherits(df$date, "Date")) errors <- c(errors, "'date' is not class Date.")
  if (!is.numeric(df$value)) errors <- c(errors, "'value' is not numeric.")
  
  # 2. Data Integrity
  if (anyDuplicated(df$date)) warnings <- c(warnings, "Duplicated dates detected.")
  if (anyNA(df)) warnings <- c(warnings, glue::glue("Contains {sum(is.na(df))} NA values."))
  if (length(unique(df$value)) == 1) warnings <- c(warnings, "Only one unique value in 'value' column.")
  if (any(df$value < 0, na.rm = TRUE)) warnings <- c(warnings, "Negative values found (check scale).")
  
  # 3. Time Properties
  n_obs <- nrow(df)
  start <- min(df$date, na.rm = TRUE)
  end   <- max(df$date, na.rm = TRUE)
  freq  <- median(diff(sort(df$date)))
  n_gap <- sum(diff(sort(df$date)) > freq)
  
  if (n_gap > 0) warnings <- c(warnings, glue::glue("Detected {n_gap} date gaps."))
  
  if (year(start) < 1980) warnings <- c(warnings, "Unusually early start date.")
  if (year(end) > 2025) warnings <- c(warnings, "End date exceeds 2025.")
  
  # 4. Summary
  results$passed   <- length(errors) == 0
  results$errors   <- errors
  results$warnings <- warnings
  results$n_obs    <- n_obs
  results$start    <- start
  results$end      <- end
  results$freq     <- freq
  results$n_na     <- sum(is.na(df$value))
  results$n_unique <- length(unique(df$value))
  
  return(results)
}
