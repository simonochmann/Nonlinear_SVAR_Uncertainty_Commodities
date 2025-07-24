#' Save Long and Wide Commodity CSVs (Robust + Logged)
#'
#' Saves cleaned commodity data in long and wide format with optional logging, overwrite protection, and atomic writes.
#'
#' @param df_long Tibble with columns: `date`, `commodity`, `price`.
#' @param dir_out Output folder (default: "data/processed/").
#' @param filename_base Base name for CSVs (no extension).
#' @param save_long Logical. Save long-format CSV? (default: TRUE)
#' @param save_wide Logical. Save wide-format CSV? (default: TRUE)
#' @param overwrite Logical. Allow overwriting files? (default: TRUE)
#' @param add_timestamp Logical. Append YYYYMMDD to filename? (default: FALSE)
#' @param log Logical. Save metadata log? (default: FALSE)
#' @param verbose Logical. Print messages? (default: TRUE)
#' @param return_paths Logical. Return paths as list? (default: FALSE)
#'
#' @return Invisibly TRUE, or list of paths if return_paths = TRUE
#' @export

save_commodity_csvs <- function(df_long,
                                dir_out = "data/processed/",
                                filename_base = "commodity_prices",
                                save_long = TRUE,
                                save_wide = TRUE,
                                overwrite = TRUE,
                                add_timestamp = FALSE,
                                log = FALSE,
                                verbose = TRUE,
                                return_paths = FALSE) {
  # Check format
  stopifnot(is.data.frame(df_long), all(c("date", "commodity", "price") %in% names(df_long)))
  
  # Create dir
  if (!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE)
  
  # Timestamped names if requested
  suffix <- if (add_timestamp) format(Sys.Date(), "%Y%m%d") else NULL
  fname <- paste(na.omit(c(filename_base, suffix)), collapse = "_")
  
  file_long <- file.path(dir_out, paste0(fname, "_long.csv"))
  file_wide <- file.path(dir_out, paste0(fname, "_wide.csv"))
  
  out_paths <- list()
  
  # Save long
  if (save_long) {
    if (file.exists(file_long) && !overwrite) stop("File exists and overwrite = FALSE: ", file_long)
    tmp_file <- paste0(file_long, ".tmp")
    readr::write_csv(df_long, tmp_file)
    file.rename(tmp_file, file_long)
    if (verbose) cat("Saved long-format CSV:", file_long, "\n")
    out_paths$long <- file_long
  }
  
  # Save wide
  if (save_wide) {
    df_wide <- tidyr::pivot_wider(df_long, names_from = commodity, values_from = price)
    if (file.exists(file_wide) && !overwrite) stop("File exists and overwrite = FALSE: ", file_wide)
    tmp_file <- paste0(file_wide, ".tmp")
    readr::write_csv(df_wide, tmp_file)
    file.rename(tmp_file, file_wide)
    if (verbose) cat("Saved wide-format CSV:", file_wide, "\n")
    out_paths$wide <- file_wide
  }
  
  # Logging (CSV metadata)
  if (log) {
    log_path <- file.path(dir_out, paste0(fname, "_metadata_log.txt"))
    n_rows <- nrow(df_long)
    n_commodities <- length(unique(df_long$commodity))
    date_range <- range(df_long$date, na.rm = TRUE)
    writeLines(c(
      paste("Saved:", Sys.time()),
      paste("Rows:", n_rows),
      paste("Commodities:", n_commodities),
      paste("Date Range:", date_range[1], "to", date_range[2]),
      paste("Files:", paste(names(out_paths), out_paths, sep = ": "))
    ), con = log_path)
    if (verbose) cat("Log saved to:", log_path, "\n")
    out_paths$log <- log_path
  }
  
  if (return_paths) return(out_paths) else invisible(TRUE)
}
