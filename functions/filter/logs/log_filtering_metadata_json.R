#' Log Metadata for Filtered Commodity Panel (.json format)
#'
#' Captures key metadata from the filtered and volatility-enhanced
#' commodity price panel. Outputs machine-readable `.json` for auditing
#' and reproducibility.
#'
#' @param df Filtered panel in long format (`date`, `commodity`, `price`)
#' @param out_dir Directory where long/wide CSVs were saved
#' @param out_json Optional path to save metadata log (.json)
#' @param vol_method Volatility proxy used ("squared", "abs", etc.)
#' @param min_obs Minimum non-NA observations required per series
#' @param tags Optional character vector of tags (e.g., c("monthly", "energy"))
#' @param note Optional annotation
#' @param verbose Logical; print metadata path if TRUE
#'
#' @return Invisibly returns metadata list
#' @export
log_filtering_metadata_json <- function(df,
                                        out_dir,
                                        out_json = NULL,
                                        vol_method = "squared",
                                        min_obs = NA_integer_,
                                        tags = NULL,
                                        note = "",
                                        verbose = TRUE) {
  stopifnot(is.data.frame(df))
  
  # Accept either lean (date, commodity, price) or canonical (date, commodity_name, price)
  requireNamespace("dplyr"); requireNamespace("rlang")
  if (!all(c("date", "commodity", "price") %in% names(df))) {
    if (all(c("date", "commodity_name", "price") %in% names(df))) {
      df <- dplyr::rename(df, commodity = "commodity_name")
    } else {
      stop("log_filtering_metadata_json(): df must contain either ",
           "(date, commodity, price) or (date, commodity_name, price).")
    }
  }
  
  # Light type guards (do not fail if we can coerce sanely)
  if (!inherits(df$date, "Date")) df$date <- as.Date(df$date)
  df$commodity <- as.character(df$commodity)
  df$price <- suppressWarnings(as.numeric(df$price))
  
  stopifnot(dir.exists(out_dir))
  
  requireNamespace("digest")
  requireNamespace("jsonlite")
  requireNamespace("base64enc")
  
  hash_file <- function(f) {
    if (!file.exists(f)) return(NA_character_)
    digest::digest(file = f, algo = "sha256")
  }
  
  long_file <- file.path(out_dir, "filtered_panel_long.csv")
  wide_file <- file.path(out_dir, "filtered_panel_wide.csv")
  
  short_id <- substr(
    base64enc::base64encode(charToRaw(digest::digest(df, algo = "sha256"))),
    1, 12
  )
  
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
    output_dir = normalizePath(out_dir, winslash = "/", mustWork = FALSE),
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
    filtering = list(
      min_obs = min_obs,
      volatility_method = vol_method
    ),
    tags = if (!is.null(tags)) tags else character(0),
    note = note
  )
  
  if (is.null(out_json)) {
    ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
    out_json <- file.path("logs/filtering", paste0("filter_metadata_", ts, "_", short_id, ".json"))
  }
  
  stopifnot(is.character(out_json), length(out_json) == 1, !is.na(out_json))
  dir.create(dirname(out_json), recursive = TRUE, showWarnings = FALSE)
  
  jsonlite::write_json(metadata, out_json, pretty = TRUE, auto_unbox = TRUE)
  
  if (verbose) cat("Metadata saved to:", out_json, "\n")
  
  invisible(metadata)
}