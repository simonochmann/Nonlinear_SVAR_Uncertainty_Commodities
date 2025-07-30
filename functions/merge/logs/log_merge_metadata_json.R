#' Log Structured Merge Metadata to JSON
#'
#' Appends detailed metadata for a merge operation to a persistent JSON audit log.
#'
#' @param index_name Canonical name of the uncertainty index (e.g., "VIX").
#' @param n_rows_main Number of rows in the main dataset before merge.
#' @param n_rows_index Number of rows in the index dataset.
#' @param n_rows_matched Number of successfully matched rows.
#' @param source_file_index File path of the uncertainty index CSV.
#' @param output_path JSON file to append to.
#' @param verbose Logical, print confirmation to console?
#' @param extra Optional named list of additional metadata fields.
#'
#' @return Invisibly returns the metadata entry as a list.
#' @export
log_merge_metadata_json <- function(index_name,
                                    n_rows_main,
                                    n_rows_index,
                                    n_rows_matched,
                                    source_file_index,
                                    output_path,
                                    verbose = FALSE,
                                    extra = list()) {
  stopifnot(
    is.character(index_name), length(index_name) == 1,
    is.numeric(n_rows_main), length(n_rows_main) == 1,
    is.numeric(n_rows_index), length(n_rows_index) == 1,
    is.numeric(n_rows_matched), length(n_rows_matched) == 1,
    is.character(source_file_index), length(source_file_index) == 1,
    is.character(output_path), length(output_path) == 1,
    is.list(extra)
  )
  
  # Safely hash input file
  hash_source <- if (file.exists(source_file_index)) {
    tools::md5sum(source_file_index)[[1]]
  } else {
    NA_character_
  }
  
  # Construct metadata block
  metadata <- c(
    list(
      timestamp      = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      index_name     = index_name,
      rows_main      = n_rows_main,
      rows_index     = n_rows_index,
      rows_matched   = n_rows_matched,
      pct_matched    = if (n_rows_main > 0) round(100 * n_rows_matched / n_rows_main, 2) else NA_real_,
      source_file    = normalizePath(source_file_index, winslash = "/", mustWork = FALSE),
      source_md5     = hash_source
    ),
    extra
  )
  
  # Ensure output directory exists
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  
  # Read + append to existing log
  combined <- tryCatch({
    if (file.exists(output_path)) {
      jsonlite::fromJSON(output_path, simplifyVector = FALSE)
    } else {
      list()
    }
  }, error = function(e) {
    warning(sprintf("Could not parse existing JSON log: %s", e$message))
    list()
  })
  
  combined <- append(combined, list(metadata))
  
  # Write updated log
  tryCatch({
    jsonlite::write_json(
      combined,
      path = output_path,
      pretty = TRUE,
      auto_unbox = TRUE
    )
    if (verbose) {
      message(sprintf("JSON metadata log updated for index: %s", index_name))
    }
  }, error = function(e) {
    warning(sprintf("Failed to write metadata JSON log: %s", e$message))
  })
  
  invisible(metadata)
}