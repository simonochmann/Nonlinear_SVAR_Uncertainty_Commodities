#' Load Pink Sheet Excel Data (final version)
#'
#' Reads and processes the World Bank Pink Sheet, skipping metadata rows,
#' renaming the date column, parsing date values, and validating format.
#'
#' @param path Path to the Excel file
#' @param sheet Sheet name to read (default: "Monthly Prices")
#' @param fallback If TRUE, tries multiple skip values if initial read fails
#' @param verbose Print detailed output (default: TRUE)
#'
#' @return A tibble with cleaned column names and parsed dates
#' @export
load_pink_sheet_excel <- function(path, 
                                  sheet = "Monthly Prices", 
                                  fallback = TRUE, 
                                  verbose = TRUE) {
  # Helper: attempt reading with given skip
  try_read <- function(skip_val) {
    try(readxl::read_excel(path, sheet = sheet, skip = skip_val), silent = TRUE)
  }
  
  if (verbose) cat("📥 Loading Pink Sheet:", basename(path), "\n")
  
  # Primary attempt: skip = 6
  df <- try_read(6)
  
  if (inherits(df, "try-error") || !"...1" %in% names(df)) {
    if (fallback) {
      if (verbose) cat("skip = 6 failed. Attempting fallback autodetect...\n")
      for (i in 0:8) {
        df_try <- try_read(i)
        if (!inherits(df_try, "try-error") &&
            any(grepl("CRUDE", names(df_try), ignore.case = TRUE))) {
          df <- df_try
          if (verbose) cat("Auto-detected skip =", i, "\n")
          break
        }
      }
    } else {
      stop("Could not read Pink Sheet with skip = 6, and fallback is disabled.")
    }
  } else {
    if (verbose) cat("Successfully read sheet with skip = 6\n")
  }
  
  # Validate and rename first column
  if ("...1" %in% names(df)) {
    names(df)[names(df) == "...1"] <- "date"
  }
  
  # Coerce to tibble
  df <- tibble::as_tibble(df)
  
  # Attempt to parse monthly dates (e.g., "1960M01")
  df$date <- suppressWarnings(readr::parse_date(df$date, format = "%YM%m"))
  
  # Check all dates parsed successfully
  if (anyNA(df$date)) {
    stop("Failed to parse some dates. Please check date format (expected '1960M01').")
  }
  
  # Sort chronologically
  df <- dplyr::arrange(df, date)
  
  if (verbose) {
    cat("️Rows:", nrow(df), "   Commodities:", ncol(df) - 1, "\n")
    cat("Date range:", format(min(df$date)), "to", format(max(df$date)), "\n")
  }
  
  return(df)
}
