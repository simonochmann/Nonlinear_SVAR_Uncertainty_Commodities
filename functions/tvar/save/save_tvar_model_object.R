#' Save Threshold VAR Model Object
#'
#' Serializes the estimated TVAR model and writes metadata including timestamp, hash, and optional input snapshot.
#'
#' @param model_object A list returned from `estimate_tvar_model()`, ideally including metadata.
#' @param prefix Optional prefix tag for filenames (e.g. "vix_").
#' @param verbose If TRUE, prints messages.
#' @return A list with file paths for the saved .rds and .json (and optional .csv) files.
save_tvar_model_object <- function(model_object, prefix = "", verbose = TRUE) {
  stopifnot(!is.null(model_object$metadata))
  
  timestamp   <- format(Sys.time(), "%Y%m%d_%H%M%S")
  dir_models  <- here::here("models", "tvar")
  dir_logs    <- here::here("logs", "tvar")
  
  if (!dir.exists(dir_models)) dir.create(dir_models, recursive = TRUE)
  if (!dir.exists(dir_logs))   dir.create(dir_logs, recursive = TRUE)
  
  # Base filenames
  rds_base   <- paste0(prefix, "tvar_model_", timestamp)
  json_base  <- paste0(prefix, "tvar_model_metadata_", timestamp)
  rds_path   <- file.path(dir_models, paste0(rds_base, ".rds"))
  json_path  <- file.path(dir_logs, paste0(json_base, ".json"))
  
  # Hash fingerprint
  model_hash <- digest::digest(model_object, algo = "sha256")
  
  # Collision-safe filename (batch safe)
  i <- 1
  while (file.exists(rds_path) || file.exists(json_path)) {
    suffix    <- paste0("_v", i)
    rds_path  <- file.path(dir_models, paste0(rds_base, suffix, ".rds"))
    json_path <- file.path(dir_logs, paste0(json_base, suffix, ".json"))
    i <- i + 1
  }
  
  # save model
  saveRDS(model_object, file = rds_path)
  
  # generate metadata
  metadata <- list(
    timestamp      = timestamp,
    prefix         = prefix,
    hash           = model_hash,
    rds_file       = basename(rds_path),
    input_vars     = model_object$metadata$variables,
    lag_order      = model_object$metadata$lag,
    threshold_var  = model_object$regime_variable,
    threshold_val  = model_object$threshold_value,
    tag            = model_object$metadata$threshold_tag,
    obs_total      = model_object$metadata$total_obs,
    regimes        = names(model_object$regimes)
  )
  
  # input snapshot
  if (!is.null(model_object$metadata$input_df)) {
    input_csv <- file.path(dir_logs, paste0(prefix, "tvar_input_", timestamp, ".csv"))
    readr::write_csv(model_object$metadata$input_df, input_csv)
    metadata$input_snapshot <- basename(input_csv)
    if (verbose) message("📊 Input snapshot saved to: ", input_csv)
  }
  
  jsonlite::write_json(metadata, path = json_path, pretty = TRUE, auto_unbox = TRUE)
  
  if (verbose) {
    message("VAR model saved: ", rds_path)
    message("Metadata saved: ", json_path)
    message("SHA256 hash: ", model_hash)
  }
  
  return(list(
    model_rds = rds_path,
    metadata_json = json_path,
    input_snapshot = if (exists("input_csv")) input_csv else NULL
  ))
}
