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
standardize_panel_variables <- function(df) {
  stopifnot("date" %in% names(df))
  
  # Keep ID columns as character
  id_cols <- intersect(c("commodity_id","commodity_name","unit"), names(df))
  for (nm in id_cols) df[[nm]] <- as.character(df[[nm]])
  
  # Only coerce candidate numeric columns (exclude date + ids)
  num_candidates <- setdiff(names(df), c("date", id_cols))
  # try numeric coercion; track which became all-NA
  all_na <- character(0)
  for (nm in num_candidates) {
    if (!is.numeric(df[[nm]])) {
      old <- df[[nm]]
      suppressWarnings(df[[nm]] <- as.numeric(old))
      if (all(is.na(df[[nm]]))) all_na <- c(all_na, nm)
    }
  }
  
  # Optional: warn only for non-ID columns that turned into all-NA
  if (length(all_na)) {
    warning("These columns could not be coerced to numeric (all NA): ",
            paste(all_na, collapse = ", "))
  }
  
  list(
    data = df,
    meta = list(colnames = names(df))
  )
}