validate_ciss_output <- function(df, verbose = TRUE) {
  stopifnot(all(c("date","ciss") %in% names(df)))
  if (!inherits(df$date, "Date")) stop("date must be Date")
  if (nrow(df) == 0) stop("CISS panel is empty")
  if (verbose) {
    cat(" validate_ciss_output(): OK | rows:", nrow(df), "\n")
    cat(" Date range:", format(min(df$date)), "to", format(max(df$date)), "\n")
  }
  invisible(TRUE)
}
