#' Log Metadata from Pink Sheet Parse to JSON (Enhanced)
#'
#' Captures a complete trace of the data parse, including file hashes, date range,
#' structure, user session, system info, and optional notes. Designed for
#' reproducibility, auditing, and integration into larger data provenance systems.
#'
#' @param df Data.frame in long format (must contain `date`, `commodity`, `price`)
#' @param input_file Path to original Excel input
#' @param output_dir Directory where CSVs were saved
#' @param out_json Path to write .json metadata (optional)
#' @param tags Optional character vector for classification (e.g., c("monthly", "energy"))
#' @param note Optional freeform annotation
#' @param verbose Logical, whether to print summary
#'
#' @return Invisibly returns metadata list
#' @export
log_parse_metadata_json <- function(df,
                                    input_file,
                                    output_dir,
                                    out_json = NULL,
                                    tags = NULL,
                                    note = "",
                                    verbose = TRUE) {
  stopifnot(is.data.frame(df))
  stopifnot(all(c("date", "commodity", "price") %in% names(df)))
  
  requireNamespace("jsonlite")
  requireNamespace("digest")
  requireNamespace("base64enc")
  
  # Compute SHA-256 hashes
  hash_file <- function(f) {
    if (!file.exists(f)) return(NA_character_)
    digest::digest(file = f, algo = "sha256")
  }
  
  long_file <- file.path(output_dir, "commodity_prices_long.csv")
  wide_file <- file.path(output_dir, "commodity_prices_wide.csv")
  
  # Optional: short hash ID (base64-encoded SHA digest prefix)
  short_id <- substr(base64enc::base64encode(charToRaw(digest::digest(df, algo = "sha256"))), 1, 12)
  
  metadata <- list(
    id = short_id,
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    user = Sys.info()[["user"]],
    system = list(
      hostname = Sys.info()[["nodename"]],
      platform = R.version$platform,
      version = R.version$version.string
    ),
    session = list(
      pid = Sys.getpid(),
      working_directory = getwd()
    ),
    input_file = normalizePath(input_file, winslash = "/", mustWork = FALSE),
    input_hash = hash_file(input_file),
    output_dir = normalizePath(output_dir, winslash = "/", mustWork = FALSE),
    output_files = list(
      long_csv = normalizePath(long_file, winslash = "/", mustWork = FALSE),
      wide_csv = normalizePath(wide_file, winslash = "/", mustWork = FALSE)
    ),
    output_hashes = list(
      long_csv = hash_file(long_file),
      wide_csv = hash_file(wide_file)
    ),
    structure = list(
      n_obs = nrow(df),
      n_vars = ncol(df),
      n_commodities = length(unique(df$commodity)),
      date_range = format(range(df$date), "%Y-%m-%d"),
      missing_pct = round(mean(is.na(df$price)) * 100, 3)
    ),
    tags = if (!is.null(tags)) tags else character(0),
    note = note
  )
  
  # Define default path
  if (is.null(out_json)) {
    ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
    out_json <- file.path("logs", "pink_sheet", paste0("parse_metadata_", ts, "_", short_id, ".json"))
  }
  
  # Defensive fix to ensure out_json is a valid character string
  stopifnot(is.character(out_json), length(out_json) == 1, !is.na(out_json), nchar(out_json) > 0)
  
  dir.create(dirname(out_json), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(metadata, out_json, pretty = TRUE, auto_unbox = TRUE)
  
  if (verbose) cat("Metadata saved to:", out_json, "\n")
  
  invisible(metadata)
}
