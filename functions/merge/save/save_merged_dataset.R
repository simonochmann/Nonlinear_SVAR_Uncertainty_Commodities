#' Save Merged Commodity Panel with Uncertainty Index
#'
#' Saves a wide-format merged dataset (commodities + uncertainty index) as CSV.
#' Designed for panel like: `date`, `commodity_1`, ..., `uncertainty_index`.
#'
#' @param df A tibble with at least a `date` column and one uncertainty index column.
#' @param file_path Full path to output file.
#' @param verbose Show save messages? Default: TRUE.
#'
#' @return Invisible file path.
#' @export
save_merged_dataset <- function(df, file_path, verbose = TRUE) {
  stopifnot("date" %in% names(df))
  readr::write_csv(df, file_path)
  if (verbose) message(glue::glue("Saved merged dataset to {file_path}"))
  return(invisible(file_path))
}