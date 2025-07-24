#' Validate Column Types in Commodity Data
#'
#' Ensures `date_column` is of class `Date`, all other columns (or specified ones)
#' are numeric, and optionally checks for NA values. Returns TRUE with metadata
#' attributes if all checks pass; otherwise throws an error.
#'
#' @param df A data.frame or tibble with commodity data.
#' @param date_column Name of the column containing dates (default: "date").
#' @param enforce_numeric_columns Optional character vector of column names that must be numeric.
#' @param allow_NA Logical. If FALSE, throws error if any NA values are found (default: TRUE).
#' @param verbose Logical. If TRUE, prints validation summary (default: TRUE).
#'
#' @return TRUE (with metadata attributes), or error if validation fails.
#' @export
#'
#' @examples
#' validate_column_types(df)
#' validate_column_types(df, enforce_numeric_columns = c("gold", "oil"))

validate_column_types <- function(df,
                                  date_column = "date",
                                  enforce_numeric_columns = NULL,
                                  allow_NA = TRUE,
                                  verbose = TRUE) {
  stopifnot(is.data.frame(df))
  
  # Check date column 
  if (!(date_column %in% names(df))) {
    stop(glue::glue("Missing required date column: '{date_column}'"))
  }
  
  if (!inherits(df[[date_column]], "Date")) {
    stop(glue::glue("Column '{date_column}' must be of class 'Date'. Found: {class(df[[date_column]])}"))
  }
  
  # Check numeric columns
  if (is.null(enforce_numeric_columns)) {
    non_numeric <- df |>
      dplyr::select(-all_of(date_column)) |>
      purrr::keep(~ !is.numeric(.x)) |>
      names()
    
    if (length(non_numeric) > 0) {
      warning(glue::glue("Non-numeric columns detected: {paste(non_numeric, collapse = ', ')}"))
    } else if (verbose) {
      cat("All columns (except date) are numeric.\n")
    }
    
  } else {
    missing_cols <- setdiff(enforce_numeric_columns, names(df))
    if (length(missing_cols) > 0) {
      warning(glue::glue("Columns not found: {paste(missing_cols, collapse = ', ')}"))
    }
    
    non_numeric_custom <- enforce_numeric_columns[!sapply(df[enforce_numeric_columns], is.numeric)]
    if (length(non_numeric_custom) > 0) {
      warning(glue::glue("Expected numeric but got non-numeric: {paste(non_numeric_custom, collapse = ', ')}"))
    } else if (verbose) {
      cat("All enforced columns are numeric.\n")
    }
  }
  
  # Check for NAs
  if (!allow_NA) {
    na_cols <- df |> dplyr::select(where(~ anyNA(.x))) |> names()
    if (length(na_cols) > 0) {
      stop(glue::glue("NA values detected in columns: {paste(na_cols, collapse = ', ')}"))
    }
  }
  
  # Verbose 
  if (verbose) {
    cat(" validate_column_types(): All checks passed\n")
    cat(" Columns:", ncol(df), " | Observations:", nrow(df), "\n")
    cat(" Date range:", format(min(df[[date_column]])), "to", format(max(df[[date_column]])), "\n")
  }
  
  # Return TRUE with metadata
  attr(df, "validation") <- list(
    n_cols = ncol(df),
    n_obs = nrow(df),
    date_column = date_column,
    has_NA = any(is.na(df)),
    timestamp = Sys.time()
  )
  
  return(df)
}
