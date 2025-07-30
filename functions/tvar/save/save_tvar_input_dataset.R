#' Save Final TVAR Input Dataset (CSV + Hash)
#'
#' Saves the cleaned and filtered TVAR input panel to disk, verifies save integrity via SHA-256 hash,
#' and returns metadata for reproducibility and version tracking.
#'
#' @param df A tibble with a `date` column and time series variables.
#' @param path Full output path for the CSV file.
#' @param hash Logical. If TRUE, returns a SHA-256 hash for the saved file. Default = TRUE.
#' @param overwrite Logical. If FALSE, abort if file already exists. Default = TRUE.
#' @param verbose Logical. If TRUE, display success message and metadata. Default = TRUE.
#'
#' @return A list with:
#'   - `path`: File path
#'   - `sha256`: File hash (if `hash = TRUE`)
#'   - `n_obs`: Number of rows
#'   - `n_vars`: Number of variables (excluding `date`)
#'   - `date_range`: Range of dates
#' @export
save_tvar_input_dataset <- function(df, path, hash = TRUE, overwrite = TRUE, verbose = TRUE) {
  # Validate input
  stopifnot(is.data.frame(df), "date" %in% names(df))
  stopifnot(inherits(df$date, "Date"))
  stopifnot(dir.exists(dirname(path)))
  
  # Prevent overwrite unless allowed
  if (!overwrite && file.exists(path)) {
    stop(glue::glue("File already exists: {path} (overwrite = FALSE)"))
  }
  
  # Save
  readr::write_csv(df, file = path)
  
  # Hash
  sha256 <- if (hash) openssl::sha256(file(path)) else NULL
  
  # Metadata
  meta <- list(
    path = path,
    sha256 = sha256,
    n_obs = nrow(df),
    n_vars = ncol(df) - 1,
    date_range = range(df$date)
  )
  
  # Message
  if (verbose) {
    message(glue::glue("TVAR panel saved to {path}"))
    if (hash) message(glue::glue("SHA-256: {meta$sha256}"))
    message(glue::glue("Rows: {meta$n_obs} | Vars: {meta$n_vars} | Date: {meta$date_range[1]} → {meta$date_range[2]}"))
  }
  
  return(meta)
}
