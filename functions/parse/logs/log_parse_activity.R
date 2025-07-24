#' 📝 Log Pink Sheet Parsing Activity (Enhanced v2)
#'
#' Records details of a parsing operation: timestamp, user, session, script, input file, Git hash, file hash,
#' output paths, tags, and freeform notes. Logs to CSV with reproducibility and auditing in mind.
#'
#' @param input_file Path to the raw file parsed (e.g., Pink Sheet Excel).
#' @param output_files Character vector of saved output file paths (relative or absolute).
#' @param log_path Path to the .csv log file (default: "logs/parse_log.csv").
#' @param note Optional freeform annotation for this operation (default: "").
#' @param tags Optional character vector of keywords for classification (e.g., c("raw", "pink_sheet", "initial")).
#' @param verbose Logical. If TRUE, prints log confirmation to console (default: TRUE).
#'
#' @return Invisibly returns the updated log data.frame.
#' @export
#'
#' @examples
#' log_parse_activity("data/external/Monthly_Prices_Pink_Sheet.xlsx",
#'                    c("data/raw/commodity_prices_wide.csv"),
#'                    note = "Initial parse", tags = c("pink_sheet", "parse"))

log_parse_activity <- function(input_file,
                               output_files,
                               log_path = "logs/parse_log.csv",
                               note = "",
                               tags = NULL,
                               verbose = TRUE) {
  # 1. Ensure directory exists
  log_dir <- dirname(log_path)
  if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)
  
  # 2. Infer calling script path
  calling_script <- tryCatch({
    cmd_args <- commandArgs(trailingOnly = FALSE)
    script_path <- sub("^--file=", "", cmd_args[grep("^--file=", cmd_args)])
    if (length(script_path) == 0) NA else normalizePath(script_path, winslash = "/", mustWork = FALSE)
  }, error = function(e) NA)
  
  # 3. Get Git commit hash (for reproducibility)
  git_hash <- tryCatch(
    system("git rev-parse --short HEAD", intern = TRUE),
    error = function(e) NA
  )
  
  # 4. Compute SHA-256 hash of input file (for data integrity)
  input_hash <- tryCatch(
    digest::digest(file = input_file, algo = "sha256"),
    error = function(e) NA
  )
  
  # 5. Build log entry
  entry <- tibble::tibble(
    timestamp     = as.POSIXct(Sys.time(), tz = "UTC"),
    user          = Sys.info()[["user"]],
    session       = Sys.getpid(),
    script        = calling_script,
    input_file    = normalizePath(input_file, winslash = "/", mustWork = FALSE),
    output_files  = paste(normalizePath(gsub("//+", "/", output_files), winslash = "/", mustWork = FALSE), collapse = "; "),
    tags          = if (!is.null(tags)) paste(tags, collapse = ", ") else "",
    note          = note
  )
  
  # 6. Append to existing or new log
  if (file.exists(log_path)) {
    existing_log <- readr::read_csv(log_path, show_col_types = FALSE)
    full_log <- dplyr::bind_rows(existing_log, entry)
  } else {
    full_log <- entry
  }
  
  # 7. Write updated log safely
  readr::write_csv(full_log, log_path)

  # 8. Optional: Console confirmation
  if (verbose) {
    cat("Log Entry Added:", log_path, "\n")
    print(entry, width = 1000)
    cat(strrep("-", 80), "\n")
  }
  
  invisible(full_log)
}
