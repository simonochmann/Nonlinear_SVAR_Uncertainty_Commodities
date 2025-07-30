#' Load an Uncertainty Index CSV File 
#'
#' Loads a raw CSV file from disk, handles embedded quotes (e.g. ECB SDW),
#' applies optional column cleaning and logs file metadata.
#'
#' @param filepath Full path to the CSV.
#' @param clean_names Logical. Clean column names using janitor (default = TRUE).
#' @param verbose Logical. Print status message (default = TRUE).
#' @param log_hash Logical. Whether to log SHA256 hash to JSON (default = TRUE).
#'
#' @return A tibble of raw contents.
#' @export
load_uncertainty_index_csv <- function(filepath,
                                       clean_names = TRUE,
                                       verbose = TRUE,
                                       log_hash = TRUE) {
  if (!file.exists(filepath)) stop(glue::glue("File not found: {filepath}"))
  
  file_hash <- digest::digest(file = filepath, algo = "sha256")
  filename <- tolower(basename(filepath))
  is_ciss <- grepl("ciss", filename)
  
  if (is_ciss) {
    raw_lines <- readLines(filepath, warn = FALSE)
    
    parsed <- lapply(raw_lines[-1], function(line) {
      # Remove ALL double quotes, then split by comma
      fields <- gsub('"', '', line) |> strsplit(",", fixed = TRUE) |> unlist()
      return(fields)
    })
    
    parsed_lengths <- lengths(parsed)
    parsed <- parsed[parsed_lengths == 3]
    
    if (length(parsed) == 0) stop("No valid rows with 3 fields found in CISS CSV.")
    
    df <- as.data.frame(do.call(rbind, parsed), stringsAsFactors = FALSE)
    names(df) <- c("date", "time_period", "ciss_value")
    
    df <- df |>
      dplyr::mutate(
        date = as.Date(date),
        ciss_value = as.numeric(ciss_value)
      )
  } else {
    # Standard parser
    df <- readr::read_csv(
      file = filepath,
      show_col_types = FALSE,
      quote = "\"",
      skip_empty_rows = TRUE
    )
  }
  
  if (clean_names) df <- janitor::clean_names(df)
  if (nrow(df) == 0) stop("Loaded file is empty.")
  if (verbose) message(glue::glue("Loaded: {basename(filepath)} [{nrow(df)} rows × {ncol(df)} cols]"))
  
  if (log_hash) {
    metadata <- list(
      filename = basename(filepath),
      full_path = normalizePath(filepath),
      n_rows = nrow(df),
      n_cols = ncol(df),
      sha256 = file_hash,
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )
    log_path <- file.path("logs", "merge", paste0(tools::file_path_sans_ext(basename(filepath)), "_load_metadata.json"))
    log_dir <- dirname(log_path)
    if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)
    jsonlite::write_json(metadata, path = log_path, pretty = TRUE, auto_unbox = TRUE)
  }
  
  return(df)
}