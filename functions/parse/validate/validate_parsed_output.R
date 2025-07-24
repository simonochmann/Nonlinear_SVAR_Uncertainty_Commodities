#' Validate Parsed Long Format Commodity Data (Enhanced)
#'
#' Ensures structure, types, and integrity of long-format commodity data.
#' Includes optional metadata return for logging/auditing pipelines.
#'
#' @param df Data.frame or tibble with columns `date`, `commodity`, `price`.
#' @param assert Logical. If TRUE (default), throws on error. If FALSE, returns issue list.
#' @param verbose Logical. Print summary to console.
#'
#' @return TRUE invisibly if valid (when assert = TRUE), or list of issues (when assert = FALSE)
#' @export
validate_parsed_output <- function(df,
                                   assert = TRUE,
                                   verbose = TRUE) {
  stopifnot(is.data.frame(df))
  
  issues <- list()
  required_cols <- c("date", "commodity", "price")
  
  # 1. Column presence
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    issues$missing_cols <- missing_cols
  }
  
  # 2. Column types
  if (!"Date" %in% class(df$date)) issues$date_class <- class(df$date)
  if (!is.character(df$commodity)) issues$commodity_class <- class(df$commodity)
  if (!is.numeric(df$price)) issues$price_class <- class(df$price)
  
  # 3. Observation health
  if (nrow(df) < 1000) issues$low_obs <- nrow(df)
  if (length(unique(df$commodity)) < 20) issues$low_commodities <- length(unique(df$commodity))
  
  # 4. NA presence
  if (any(is.na(df$date))) issues$na_date <- sum(is.na(df$date))
  if (any(is.na(df$commodity))) issues$na_commodity <- sum(is.na(df$commodity))
  if (any(is.na(df$price))) issues$na_price <- sum(is.na(df$price))
  
  # 5. Out-of-range dates (optional future-proofing)
  if (min(df$date, na.rm = TRUE) < as.Date("1960-01-01")) issues$early_date <- min(df$date)
  if (max(df$date, na.rm = TRUE) > Sys.Date() + 60) issues$future_date <- max(df$date)
  
  # 6. Final logic
  if (length(issues) > 0) {
    if (assert) {
      stop("Parsed output validation failed. Issues:\n", paste(names(issues), collapse = ", "))
    } else {
      return(issues)
    }
  }
  
  if (verbose) {
    cat("Parsed output validated successfully.\n")
    cat(" Rows:", nrow(df),
        " | Commodities:", length(unique(df$commodity)),
        " | Date range:", format(min(df$date)), "to", format(max(df$date)), "\n")
  }
  
  invisible(TRUE)
}
