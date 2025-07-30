#' Log Standardization Activity
#'
#' Logs the results of column standardization to a .log file.
#'
#' @param original_names Character vector of original column names.
#' @param cleaned_names Character vector of cleaned/standardized column names.
#' @param non_numeric_vars Character vector of variable names that failed numeric coercion.
#' @param tag Optional string label to describe the run (e.g. "vix", "baseline").
#' @param log_dir Directory to store the log file. Default: "logs/"
#'
#' @return Invisible TRUE
#' @export
log_standardization_activity <- function(original_names, cleaned_names, non_numeric_vars = character(),
                                         tag = NULL, log_dir = "logs/") {
  if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)
  log_file <- file.path(log_dir, "standardization.log")
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  
  renamed <- setNames(cleaned_names, original_names)
  renamed_str <- paste(
    purrr::map_chr(names(renamed), ~ glue::glue("{.x} → {renamed[[.x]]}")),
    collapse = "\n"
  )
  
  log_text <- glue::glue(
    "=== TVAR: Standardization Log ===\n",
    "Timestamp: {timestamp}\n",
    "Tag: {tag}\n",
    "Hash (SHA-256) of column names: {digest::digest(cleaned_names, algo = 'sha256')}\n\n",
    "Total columns: {length(cleaned_names)}\n",
    "Renamed columns: {length(renamed)}\n",
    "→ Renamed:\n{renamed_str}\n\n",
    "Columns failed numeric coercion: {length(non_numeric_vars)}\n",
    "{paste(non_numeric_vars, collapse = '\n')}\n",
    strrep("=", 33),
    "\n"
  )
  
  cat(log_text, file = log_file, append = TRUE)
  message("Logged standardization activity → ", basename(log_file))
  invisible(TRUE)
}