#' Load ECB/Eurostat CISS CSV robustly and return {date, ciss} monthly
load_ciss_csv <- function(path, verbose = TRUE) {
  suppressPackageStartupMessages({
    library(readr); library(dplyr); library(stringr); library(lubridate)
  })
  stopifnot(file.exists(path))
  
  clean_nms <- function(x) {
    x <- gsub('^"|"$', '', x)
    x <- gsub("\\s+", " ", x)
    x <- trimws(x)
    tolower(x)
  }
  
  # First read
  df <- read_csv(path, show_col_types = FALSE, guess_max = 200000)
  
  # If it's a one-column mess, try to split the embedded CSV
  if (ncol(df) == 1) {
    if (verbose) cat(" Detected single-column ECB export — reparsing inner CSV...\n")
    # Read file as lines
    raw_lines <- read_lines(path)
    # Now re-read using read_csv on the text, letting readr handle the quotes
    df <- read_csv(paste(raw_lines, collapse = "\n"), show_col_types = FALSE, guess_max = 200000)
  }
  
  names(df) <- clean_nms(names(df))
  if (verbose) cat(" Columns detected in CISS CSV:", paste(names(df), collapse = " | "), "\n")
  
  # Detect date column
  date_candidates <- c("date","time","time period","time_period","period","obs_time")
  date_col <- intersect(date_candidates, names(df))
  if (length(date_col) == 0) stop("No date column found.")
  
  # Detect value column
  value_col <- names(df)[str_detect(names(df), "ciss")]
  if (length(value_col) == 0) stop("No column name contains 'ciss'.")
  
  # Normalise date to month start
  to_month <- function(x) {
    if (inherits(x, "Date")) return(floor_date(x, "month"))
    suppressWarnings(d <- as.Date(x))
    if (all(is.na(d)) && is.character(x)) suppressWarnings(d <- as.Date(paste0(x, "-01")))
    floor_date(d, "month")
  }
  
  out <- df %>%
    transmute(
      date = to_month(.data[[date_col]]),
      ciss = parse_number(as.character(.data[[value_col]]))
    ) %>%
    filter(!is.na(date) & !is.na(ciss)) %>%
    arrange(date)
  
  if (verbose) {
    cat(" Loaded CISS | rows:", nrow(out), "|",
        format(min(out$date, na.rm=TRUE)), "→", format(max(out$date, na.rm=TRUE)),
        "| cols used:", date_col, "/", value_col, "\n")
  }
  out
}
