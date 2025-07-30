#' Log TVAR Metadata to JSON
#'
#' Logs metadata about a cleaned TVAR input dataset including SHA-256 file hash, structure, and NA stats.
#'
#' @param df A tibble with a `date` column.
#' @param path Full file path to the dataset that was saved.
#' @param tag Optional string to describe version/scenario (e.g. "baseline").
#' @param log_dir Directory to write JSON log into. Default: "logs/metadata/"
#' @param verbose Logical, print summary if TRUE.
#'
#' @return Invisible metadata list.
#' @export
log_tvar_metadata_json <- function(df,
                                   path,
                                   tag = NULL,
                                   log_dir = here::here("logs", "metadata"),
                                   verbose = TRUE) {
  # Strong Validation
  stopifnot("date" %in% names(df))
  stopifnot(inherits(df, "tbl_df"))
  stopifnot(inherits(df$date, "Date"))
  stopifnot(file.exists(path))
  
  if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)
  
  # Extract Info
  var_cols <- setdiff(names(df), "date")
  sha256   <- tryCatch(
    digest::digest(file = path, algo = "sha256"),
    error = function(e) digest::digest(df, algo = "sha256")
  )
  timestamp_utc <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  
  meta <- list(
    file_name      = basename(path),
    file_path      = normalizePath(path, mustWork = TRUE),
    tag            = tag,
    timestamp      = timestamp_utc,
    sha256         = sha256,
    n_obs          = nrow(df),
    n_vars         = length(var_cols),
    var_names      = var_cols,
    date_range     = as.character(range(df$date)),
    na_share_table = round(colMeans(is.na(df[var_cols])), 4)
  )
  
  # Write JSON
  base <- tools::file_path_sans_ext(basename(path))
  log_file <- file.path(log_dir, glue::glue("{base}_meta.json"))
  jsonlite::write_json(meta, path = log_file, pretty = TRUE, auto_unbox = TRUE)
  
  if (verbose) {
    message("Saved TVAR metadata JSON: ", basename(log_file))
    message("Rows: ", meta$n_obs, " | Vars: ", meta$n_vars)
    message("Date range: ", meta$date_range[1], " → ", meta$date_range[2])
    message("SHA-256: ", substr(meta$sha256, 1, 16), "...")
  }
  
  invisible(meta)
}