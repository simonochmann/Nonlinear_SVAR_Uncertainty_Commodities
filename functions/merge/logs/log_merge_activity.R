#' Log Merge Activity to CSV
#'
#' Records metadata about a merge operation: dataset sizes, matched rows, file source, and timestamp.
#'
#' @param index_name Canonical index name (e.g., "VIX").
#' @param n_rows_main Number of rows in the main panel (before merge).
#' @param n_rows_index Number of rows in the index panel.
#' @param n_rows_matched Number of rows after merge (i.e., matched).
#' @param source_file_index Source path of the uncertainty index file.
#' @param output_path CSV file to log into.
#' @param verbose Logical. Whether to print confirmation to console.
#'
#' @return Invisibly returns the log entry (as a data.frame).
#' @export
log_merge_activity <- function(index_name,
                               n_rows_main,
                               n_rows_index,
                               n_rows_matched,
                               source_file_index,
                               output_path,
                               verbose = FALSE) {
  stopifnot(
    is.character(index_name), length(index_name) == 1,
    is.numeric(n_rows_main), length(n_rows_main) == 1,
    is.numeric(n_rows_index), length(n_rows_index) == 1,
    is.numeric(n_rows_matched), length(n_rows_matched) == 1,
    is.character(source_file_index), length(source_file_index) == 1,
    is.character(output_path), length(output_path) == 1
  )
  
  # Defensive: normalize path for clarity
  source_file_index <- normalizePath(source_file_index, winslash = "/", mustWork = FALSE)
  
  pct_matched <- if (n_rows_main > 0) {
    round(100 * n_rows_matched / n_rows_main, 2)
  } else {
    NA_real_
  }
  
  log_entry <- data.frame(
    timestamp      = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    index_name     = index_name,
    n_rows_main    = n_rows_main,
    n_rows_index   = n_rows_index,
    n_rows_matched = n_rows_matched,
    pct_matched    = pct_matched,
    source_file    = source_file_index,
    stringsAsFactors = FALSE
  )
  
  # Ensure folder exists
  log_dir <- dirname(output_path)
  if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)
  
  tryCatch({
    write.table(
      log_entry,
      file      = output_path,
      sep       = ",",
      row.names = FALSE,
      col.names = !file.exists(output_path) || file.size(output_path) == 0,
      append    = TRUE
    )
    if (verbose) {
      message(sprintf("Merge log written for '%s' to %s", index_name, output_path))
    }
  }, error = function(e) {
    warning(sprintf("Failed to write merge activity log for '%s': %s", index_name, e$message))
  })
  
  invisible(log_entry)
}
