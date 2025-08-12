validate_fred_output <- function(df, verbose = TRUE) {
  stopifnot("date" %in% names(df))
  if (!inherits(df$date, "Date")) stop("date must be Date")
  if (nrow(df) == 0) stop("No rows in FRED/JLN panel")
  
  if (verbose) {
    cat(" validate_fred_output(): OK | cols:", ncol(df), "rows:", nrow(df), "\n")
    cat(" Date range:", format(min(df$date)), "to", format(max(df$date)), "\n")
  }
  invisible(TRUE)
}
