#' Log Dropped Commodities from Filtering 
#'
#' Logs dropped commodity series during the filtering step to CSV and optionally JSON,
#' including timestamp, system metadata, SHA256 hash of dropped list, and run ID.
#'
#' @param dropped_df Data.frame with columns `commodity` and `n_obs`.
#' @param out_dir Output directory for logs. Default = "logs/filter".
#' @param log_json Logical. Whether to also save a JSON metadata file. Default = TRUE.
#' @param note Optional freeform annotation. Default = "".
#' @param tags Optional character vector for classification (e.g. c("monthly", "TVAR")).
#' @param verbose Logical. Whether to print log paths.
#'
#' @return Invisibly returns a list with paths and metadata.
#' @export
log_dropped_commodities <- function(dropped_df,
                                    out_dir = here::here("logs/filter"),
                                    log_json = TRUE,
                                    note = "",
                                    tags = NULL,
                                    verbose = TRUE) {
  stopifnot(is.data.frame(dropped_df))
  stopifnot(all(c("commodity", "n_obs") %in% colnames(dropped_df)))
  
  # Dependencies 
  requireNamespace("digest")
  requireNamespace("jsonlite")
  requireNamespace("base64enc")
  
  # Metadata Prep 
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  run_id <- substr(base64enc::base64encode(charToRaw(digest::digest(dropped_df, algo = "sha256"))), 1, 12)
  hash_sha <- digest::digest(dropped_df, algo = "sha256")
  
  csv_path  <- file.path(out_dir, paste0("dropped_commodities_", timestamp, "_", run_id, ".csv"))
  json_path <- file.path(out_dir, paste0("dropped_commodities_", timestamp, "_", run_id, ".json"))
  
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(dropped_df, csv_path)
  
  # JSON Log
  if (log_json) {
    meta <- list(
      id = run_id,
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
      structure = list(
        n_dropped = nrow(dropped_df),
        n_unique  = length(unique(dropped_df$commodity))
      ),
      session = list(
        user     = Sys.info()[["user"]],
        pid      = Sys.getpid(),
        platform = R.version$platform,
        version  = R.version$version.string
      ),
      system = list(
        hostname = Sys.info()[["nodename"]],
        wd       = getwd()
      ),
      hash_sha256 = hash_sha,
      tags = tags,
      note = note
    )
    
    jsonlite::write_json(meta, path = json_path, pretty = TRUE, auto_unbox = TRUE)
  }
  
  if (verbose) {
    cat("Dropped list saved to:", csv_path, "\n")
    if (log_json) cat("🧾 Metadata JSON saved to:", json_path, "\n")
  }
  
  invisible(list(
    csv = csv_path,
    json = if (log_json) json_path else NULL,
    id = run_id,
    hash = hash_sha
  ))
}
