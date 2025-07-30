#' Standardize Panel Variables for TVAR
#'
#' Cleans and standardizes the input panel:
#' - Converts column names to snake_case
#' - Removes duplicate rows
#' - Coerces all non-date columns to numeric
#' - Sorts by date
#' - Optionally logs coercion warnings and returns metadata
#'
#' @param df A tibble with a `date` column and time series variables.
#' @param verbose Show messages? Default: TRUE.
#'
#' @return A list with:
#' - `data`: Cleaned tibble
#' - `meta`: List with n_vars, colnames, date_range
#' @export
standardize_panel_variables <- function(df, verbose = TRUE) {
  stopifnot("date" %in% names(df))
  
  # 1. Snake_case column names
  old_names <- names(df)
  df <- df %>% rename_with(janitor::make_clean_names)
  new_names <- names(df)
  
  # 2. Sort + drop duplicates
  df <- df %>%
    distinct() %>%
    arrange(date)
  
  # 3. Coerce non-date columns to numeric
  vars <- setdiff(names(df), "date")
  df[vars] <- lapply(df[vars], function(col) suppressWarnings(as.numeric(col)))
  
  # 4. Check coercion issues
  non_numeric <- sapply(df[vars], function(x) all(is.na(x)))
  if (any(non_numeric)) {
    warn_vars <- names(non_numeric)[non_numeric]
    warning("The following columns could not be coerced to numeric (all NA): ",
            paste(warn_vars, collapse = ", "))
  }
  
  # 5. Return
  meta <- list(
    n_vars     = length(vars),
    colnames   = names(df),
    date_range = range(df$date)
  )
  
  if (verbose) {
    message(glue::glue("Panel standardized: {meta$n_vars} variables"))
    message(glue::glue("Date range: {meta$date_range[1]} → {meta$date_range[2]}"))
  }
  
  return(list(
    data = df,
    meta = meta
  ))
}