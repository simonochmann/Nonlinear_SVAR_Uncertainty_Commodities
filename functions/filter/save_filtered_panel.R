#' Save Filtered Commodity Panel
#'
#' Saves cleaned commodity price panel to both long and wide CSVs,
#' with robust path creation, UTF-8 encoding, and overwrite safety.
#'
#' @param df A tibble with columns `commodity`, `date`, and `price`.
#' @param out_dir Directory to save CSVs. Created if doesn't exist.
#' @param overwrite Logical. If FALSE (default), raises error if file exists.
#' @param return_paths Logical. If TRUE, returns list of saved file paths.
#'
#' @return Invisibly returns TRUE or list of file paths if `return_paths = TRUE`.
#' @export
save_filtered_panel <- function(df,
                                out_dir = "data/filtered_panel",
                                overwrite = FALSE,
                                return_paths = FALSE) {
  stopifnot(is.data.frame(df))
  required_cols <- c("commodity", "date", "price")
  missing_cols <- setdiff(required_cols, colnames(df))
  if (length(missing_cols) > 0) {
    stop("save_filtered_panel(): Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  if (!inherits(df$date, "Date")) {
    stop("`date` column must be of class Date.")
  }
  
  # Prepare Paths 
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  long_path <- file.path(out_dir, "filtered_panel_long.csv")
  wide_path <- file.path(out_dir, "filtered_panel_wide.csv")
  
  if (!overwrite) {
    if (file.exists(long_path) || file.exists(wide_path)) {
      stop("Output file(s) already exist. Set overwrite = TRUE to allow replacement.")
    }
  }
  
  # Write Long Format
  readr::write_csv(df, long_path)
  
  # Convert to Wide Format
  df_wide <- df %>%
    tidyr::pivot_wider(names_from = commodity, values_from = price)
  
  readr::write_csv(df_wide, wide_path)
  
  # Return Paths
  if (return_paths) {
    return(list(
      long_csv = normalizePath(long_path, winslash = "/", mustWork = FALSE),
      wide_csv = normalizePath(wide_path, winslash = "/", mustWork = FALSE)
    ))
  }
  
  invisible(TRUE)
}
