#' Filter Common Sample Across Panel Variables
#'
#' Filters panel rows where the number of non-missing variables meets a minimum threshold.
#' Useful to align panel to a common sample for regime-switching or multivariate models.
#'
#' @param df A tibble with a `date` column and one or more numeric variables.
#' @param min_non_na Minimum number of non-NA values required per row. Default = all columns.
#' @param verbose Logical. If TRUE, print summary stats and per-column NA share. Default = TRUE.
#'
#' @return A list with:
#'   - `data`: Filtered tibble
#'   - `meta`: List with metadata (rows_before, rows_after, pct_retained, date_range, n_variables, na_share_table)
#' @export
filter_common_sample <- function(df, min_non_na = NULL, verbose = TRUE) {
  stopifnot("date" %in% names(df))
  stopifnot(inherits(df$date, "Date"))
  
  # Variable columns only
  var_cols <- setdiff(names(df), "date")
  stopifnot(all(sapply(df[var_cols], is.numeric)))
  
  # Default: require all variables to be non-NA
  if (is.null(min_non_na)) min_non_na <- length(var_cols)
  
  # Count non-missing values
  non_na_count <- rowSums(!is.na(df[var_cols]))
  keep_idx <- which(non_na_count >= min_non_na)
  
  # Filter panel
  filtered_df <- df[keep_idx, , drop = FALSE]
  
  # Metadata
  na_share <- round(colMeans(is.na(df[var_cols])), 3)
  rows_before <- nrow(df)
  rows_after  <- nrow(filtered_df)
  pct_retained <- round(100 * rows_after / rows_before, 2)
  date_rng <- range(filtered_df$date)
  
  meta <- list(
    rows_before = rows_before,
    rows_after = rows_after,
    pct_retained = pct_retained,
    date_range = date_rng,
    n_variables = length(var_cols),
    min_non_na_threshold = min_non_na,
    na_share_table = na_share
  )
  
  # Messaging
  if (verbose) {
    message(glue::glue("Filtered common sample ({meta$rows_after}/{meta$rows_before} rows retained, {meta$pct_retained}%)"))
    message(glue::glue("Date range: {meta$date_range[1]} → {meta$date_range[2]}"))
    message(glue::glue("Min required non-NA vars per row: {min_non_na}/{meta$n_variables}"))
    message("NA share per variable:")
    print(meta$na_share_table)
  }
  
  return(list(
    data = filtered_df,
    meta = meta
  ))
}
