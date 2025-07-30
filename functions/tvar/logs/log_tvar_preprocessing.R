#' Log TVAR Preprocessing Activity
#'
#' Appends a row to the tvar_preprocessing log CSV with timestamp, metadata, and optional hash.
#'
#' @param path Full path to the saved TVAR dataset.
#' @param n_obs Number of rows in the panel.
#' @param n_vars Number of variables (excluding date).
#' @param date_range Vector with start and end date.
#' @param sha256 Optional file hash. If NULL, will be computed.
#' @param tag Optional string to describe the run (e.g., "baseline", "test").
#' @param log_dir Directory where log should be stored. Default: "logs/"
#' @param verbose Logical. Print confirmation message? Default = TRUE.
#'
#' @return Invisible TRUE
#' @export
log_tvar_preprocessing <- function(path, n_obs, n_vars, date_range, sha256 = NULL, tag = NULL,
                                   log_dir = "logs/", verbose = TRUE) {
  stopifnot(file.exists(path))
  
  # Auto-hash if needed
  if (is.null(sha256)) {
    sha256 <- as.character(digest::digest(file = path, algo = "sha256"))
    if (verbose) message("Auto-generated SHA-256: ", sha256)
  }
  
  # Construct log path
  log_path <- file.path(log_dir, "tvar_preprocessing.csv")
  if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)
  
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  
  log_entry <- tibble::tibble(
    timestamp   = timestamp,
    file        = path,
    n_obs       = n_obs,
    n_vars      = n_vars,
    start_date  = as.character(date_range[1]),
    end_date    = as.character(date_range[2]),
    sha256      = as.character(sha256),
    tag         = ifelse(is.null(tag), NA_character_, tag)
  )
  
  # Detect duplicate row
  if (file.exists(log_path)) {
    log_existing <- readr::read_csv(log_path, show_col_types = FALSE)
    if (any(log_existing$timestamp == timestamp & log_existing$file == path)) {
      warning("Duplicate log entry detected (timestamp + path). Skipping append.")
      return(invisible(FALSE))
    }
    readr::write_csv(log_entry, log_path, append = TRUE)
    if (verbose) message("Appended to TVAR log: ", basename(log_path))
  } else {
    readr::write_csv(log_entry, log_path)
    if (verbose) message("Created new TVAR log at: ", log_path)
  }
  
  invisible(TRUE)
}
