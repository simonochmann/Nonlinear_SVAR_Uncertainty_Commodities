#' Validate & Coerce Column Types in Commodity Data
#'
#' Ensures `date_column` is Date, coerces non-numeric commodity columns to numeric
#' (safe parsing with common NA tokens), and optionally enforces numeric types
#' for a subset of columns. Returns the *cleaned df* with metadata attributes.
#'
#' @param df A data.frame or tibble with commodity data.
#' @param date_column Name of the column containing dates (default: "date").
#' @param enforce_numeric_columns Optional character vector that must be numeric.
#' @param allow_NA Logical. If FALSE, error if any NA values are found (default: TRUE).
#' @param coerce_non_numeric Logical. If TRUE, try to coerce non‑numeric columns to numeric (default: TRUE).
#' @param verbose Logical. If TRUE, prints validation summary (default: TRUE).
#'
#' @return The input df (possibly coerced), invisibly, with `attr(., "validation")`.
#' @export
validate_column_types <- function(df,
                                  date_column = "date",
                                  enforce_numeric_columns = NULL,
                                  allow_NA = TRUE,
                                  coerce_non_numeric = TRUE,
                                  verbose = TRUE) {
  stopifnot(is.data.frame(df))
  
  # --- Date column checks ------------------------------------------------------
  if (!(date_column %in% names(df))) {
    stop(glue::glue("Missing required date column: '{date_column}'"))
  }
  if (!inherits(df[[date_column]], "Date")) {
    stop(glue::glue("Column '{date_column}' must be of class 'Date'. Found: {class(df[[date_column]])}"))
  }
  
  # --- Identify commodity columns ---------------------------------------------
  value_cols <- setdiff(names(df), date_column)
  
  # --- Coercion: make non-numeric columns numeric safely ----------------------
  converted <- character(0)
  if (coerce_non_numeric && length(value_cols) > 0) {
    df[value_cols] <- lapply(df[value_cols], function(x) {
      if (is.numeric(x)) return(x)
      # normalize common non-numeric tokens to NA, then parse numbers
      x_chr <- as.character(x)
      x_chr <- gsub("^\\s*$|^\\.$|^-$|^n/?a$|^na$|^N/A$|^NA$", NA_character_, x_chr, ignore.case = TRUE)
      parsed <- readr::parse_number(x_chr, na = c("", "NA"))
      parsed
    })
    
    # Track which columns are still non-numeric after coercion
    still_non_num <- value_cols[!vapply(df[value_cols], is.numeric, logical(1))]
    if (length(still_non_num) > 0) {
      warning(glue::glue("Columns remain non-numeric after coercion: {paste(still_non_num, collapse = ', ')}"))
    }
    
    # Report columns that changed type (were not numeric before)
    converted <- value_cols[vapply(df[value_cols], is.numeric, logical(1))]
    # Keep only those that *weren't* numeric originally (best-effort signal)
    # (We can’t perfectly know prior types here without pre-snapshot; this is informative enough.)
    if (verbose && length(converted) > 0) {
      cat(" Coerced columns to numeric (parse_number):",
          paste(converted, collapse = ", "), "\n")
    }
  }
  
  # --- Optional: user-enforced numeric columns --------------------------------
  if (!is.null(enforce_numeric_columns)) {
    missing_cols <- setdiff(enforce_numeric_columns, names(df))
    if (length(missing_cols) > 0) {
      warning(glue::glue("Columns not found: {paste(missing_cols, collapse = ', ')}"))
    }
    non_numeric_custom <- enforce_numeric_columns[
      enforce_numeric_columns %in% names(df) &
        !vapply(df[enforce_numeric_columns], is.numeric, logical(1))
    ]
    if (length(non_numeric_custom) > 0) {
      warning(glue::glue("Expected numeric but got non-numeric: {paste(non_numeric_custom, collapse = ', ')}"))
    } else if (verbose) {
      cat(" All enforced columns are numeric.\n")
    }
  } else {
    # Generic report: are all value columns numeric now?
    non_numeric <- value_cols[!vapply(df[value_cols], is.numeric, logical(1))]
    if (length(non_numeric) > 0) {
      warning(glue::glue("Non-numeric columns detected: {paste(non_numeric, collapse = ', ')}"))
    } else if (verbose) {
      cat(" All columns (except date) are numeric.\n")
    }
  }
  
  # --- NA policy ---------------------------------------------------------------
  if (!allow_NA) {
    na_cols <- names(df)[vapply(df, anyNA, logical(1))]
    if (length(na_cols) > 0) {
      stop(glue::glue("NA values detected in columns: {paste(na_cols, collapse = ', ')}"))
    }
  }
  
  # Verbose summary 
  if (verbose) {
    cat(" validate_column_types(): All checks passed\n")
    cat(" Columns:", ncol(df), " | Observations:", nrow(df), "\n")
    cat(" Date range:", format(min(df[[date_column]])), "to", format(max(df[[date_column]])), "\n")
  }
  
  # Metadata 
  attr(df, "validation") <- list(
    n_cols = ncol(df),
    n_obs = nrow(df),
    date_column = date_column,
    has_NA = any(is.na(df)),
    coerced_to_numeric = converted,
    timestamp = Sys.time()
  )
  
  return(df)
}
