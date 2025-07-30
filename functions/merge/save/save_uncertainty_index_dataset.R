#' Save Uncertainty Index Dataset
#'
#' Saves a uncertainty index dataset as CSV (and optionally as RDS),
#' with integrity hash, timestamped filenames, and structured return metadata.
#'
#' @param df A merged tibble with columns: `date`, `index`, `value`.
#' @param path Folder to save to. Default: "output/data/".
#' @param filename Base name without extension. Default: "uncertainty_index".
#' @param add_timestamp Whether to append YYYYMMDD date. Default: TRUE.
#' @param save_rds Also save RDS version. Default: FALSE.
#' @param verbose Print path and info? Default: TRUE.
#'
#' @return A list with path(s), metadata, and hash.
#' @export
save_uncertainty_index_dataset <- function(df,
                                path = "output/data/",
                                filename = "merged_uncertainty",
                                add_timestamp = TRUE,
                                save_rds = FALSE,
                                verbose = TRUE) {
  stopifnot("date" %in% names(df), "index" %in% names(df), "value" %in% names(df))
  fs::dir_create(path)
  
  # Construct filename
  timestamp <- if (add_timestamp) format(Sys.Date(), "%Y%m%d") else NULL
  fname <- paste(c(filename, timestamp), collapse = "_")
  full_csv_path <- fs::path(path, paste0(fname, ".csv"))
  readr::write_csv(df, full_csv_path)
  
  # Optional RDS
  if (save_rds) {
    full_rds_path <- fs::path(path, paste0(fname, ".rds"))
    saveRDS(df, full_rds_path)
  }
  
  # Compute file hash
  hash <- tools::md5sum(full_csv_path)[[1]]
  
  # Metadata
  meta <- list(
    file_csv = full_csv_path,
    file_rds = if (save_rds) full_rds_path else NULL,
    n_obs = nrow(df),
    start = min(df$date),
    end = max(df$date),
    hash = hash,
    timestamp = timestamp
  )
  
  if (verbose) {
    message(glue::glue("Saved merged dataset to {full_csv_path}"))
    message(glue::glue("Rows: {meta$n_obs} | Date range: {meta$start} → {meta$end}"))
    message(glue::glue("MD5 hash: {meta$hash}"))
  }
  
  return(invisible(meta))
}
