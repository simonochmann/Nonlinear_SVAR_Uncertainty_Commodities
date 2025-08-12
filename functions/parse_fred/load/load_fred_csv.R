#' Load a single FRED/JLN CSV and coerce to {date, value} monthly
#' Tries known schemas first; then auto-detects date/value columns.
load_fred_csv <- function(path, index_name, verbose = TRUE) {
  suppressPackageStartupMessages({ library(readr); library(dplyr); library(stringr); library(lubridate) })
  stopifnot(file.exists(path))
  df <- readr::read_csv(path, show_col_types = FALSE)
  
  # Known schema hints
  known_date_names  <- c("DATE", "date", "observation_date")
  known_value_names <- switch(
    tolower(index_name),
    "vix" = c("VIXCLS", "Close", "vix", "value"),
    "vxo" = c("VXOCLS", "vxo", "value"),
    "jln" = c("MU", "JLN", "JLN_Macro_Uncertainty", "value"),
    c("value") # default
  )
  
  # Pick date column
  date_col <- intersect(known_date_names, names(df))
  if (length(date_col) == 0) {
    date_col <- names(df)[stringr::str_detect(tolower(names(df)), "date|time|period")][1]
  } else date_col <- date_col[1]
  if (is.na(date_col)) stop("No date-like column found in: ", path)
  
  # Pick value column
  value_col <- intersect(known_value_names, names(df))
  if (length(value_col) == 0) {
    # fallback: first numeric not date
    nums <- names(df)[vapply(df, is.numeric, logical(1))]
    value_col <- setdiff(nums, date_col)[1]
    if (is.na(value_col)) {
      # last resort: parse_number on first non-date col
      cand <- setdiff(names(df), date_col)[1]
      df[[cand]] <- readr::parse_number(as.character(df[[cand]]))
      value_col <- cand
    }
  } else value_col <- value_col[1]
  
  # Coerce to monthly
  to_month <- function(x) as.Date(lubridate::floor_date(as.Date(x), "month"))
  out <- df %>%
    transmute(
      date = to_month(.data[[date_col]]),
      value = as.numeric(.data[[value_col]])
    ) %>%
    group_by(date) %>%
    summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
    mutate(index = tolower(index_name))
  
  if (verbose) {
    cat(sprintf("  • Loaded %-4s | rows: %d | %s → %s | cols used: %s/%s\n",
                toupper(index_name), nrow(out),
                min(out$date, na.rm=TRUE), max(out$date, na.rm=TRUE),
                date_col, value_col))
  }
  out
}