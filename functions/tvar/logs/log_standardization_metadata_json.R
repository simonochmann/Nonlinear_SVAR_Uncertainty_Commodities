#' Log Standardization Metadata to JSON 
#'
#' Creates a reproducibility-grade JSON log of column cleaning and coercion.
#'
#' @param original_names Character vector of original column names.
#' @param cleaned_names Character vector of cleaned column names.
#' @param non_numeric_vars Character vector of columns that failed coercion.
#' @param output_path Path to the standardized output CSV.
#' @param tag Optional tag for the run (e.g. "vix", "baseline"). Default: NULL.
#'
#' @return Invisible path to metadata JSON log.
#' @export
log_standardization_metadata_json <- function(original_names,
                                              cleaned_names,
                                              non_numeric_vars = character(),
                                              output_path,
                                              tag = NULL) {
  stopifnot(length(original_names) == length(cleaned_names))
  
  rename_map <- purrr::map2(original_names, cleaned_names, ~ list(from = .x, to = .y))
  
  log_data <- list(
    file_name              = basename(output_path),
    file_path              = normalizePath(output_path, winslash = "/"),
    tag                    = tag,
    timestamp              = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    sha256_column_names = as.character(openssl::sha256(paste(cleaned_names, collapse = ","))),
    n_columns              = length(cleaned_names),
    original_column_names  = original_names,
    cleaned_column_names   = cleaned_names,
    renamed_columns        = rename_map,
    failed_numeric_coercion = if (length(non_numeric_vars) > 0) non_numeric_vars else NULL,
    session_info           = list(
      R_version = R.version.string,
      platform  = R.version$platform
    )
  )
  
  # Write to file
  output_file <- fs::path("logs", "metadata", fs::path_ext_set(basename(output_path), "meta.json"))
  jsonlite::write_json(log_data, path = output_file, pretty = TRUE, auto_unbox = TRUE)
  
  message(glue::glue("Saved standardization metadata JSON: {basename(output_file)}"))
  return(invisible(output_file))
}
