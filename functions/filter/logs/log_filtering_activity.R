#' Log Filtering Activity to .log File
#'
#' Appends a timestamped log entry documenting the
#' commodity filtering step. Complements structured .json metadata logging.
#'
#' @param out_log Path to `.log` file (e.g., "logs/filtering/filtering_pipeline.log")
#' @param n_commodities_before Number of total commodities before filtering
#' @param n_commodities_after Number of commodities after filtering
#' @param min_obs Minimum required non-missing observations per commodity
#' @param method_vol_proxy Method used for volatility proxy ("squared", "abs", etc.)
#' @param out_dir Directory where filtered CSVs were saved
#' @param note Optional annotation (free text)
#' @param tags Optional character vector of tags (e.g., c("monthly", "metal", "dry-run"))
#'
#' @return TRUE (invisibly)
#' @export
log_filtering_activity <- function(out_log,
                                   n_commodities_before,
                                   n_commodities_after,
                                   min_obs,
                                   method_vol_proxy,
                                   out_dir,
                                   note = "",
                                   tags = NULL) {
  stopifnot(is.character(out_log), length(out_log) == 1)
  stopifnot(is.numeric(n_commodities_before), is.numeric(n_commodities_after), is.numeric(min_obs))
  stopifnot(is.character(method_vol_proxy), dir.exists(out_dir))
  
  dir.create(dirname(out_log), recursive = TRUE, showWarnings = FALSE)
  
  pct_retained <- round(100 * n_commodities_after / n_commodities_before, 2)
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  
  log_entry <- paste0(
    "── FILTERING PIPELINE LOG ──\n",
    "Timestamp        : ", ts, "\n",
    "Total Commodities: ", n_commodities_before, "\n",
    "Retained         : ", n_commodities_after, " (", pct_retained, "%)\n",
    "Min Obs Required : ", min_obs, "\n",
    "Volatility Method: ", method_vol_proxy, "\n",
    "Output Directory : ", normalizePath(out_dir, winslash = "/", mustWork = FALSE), "\n",
    if (!is.null(tags)) paste0("Tags            : ", paste(tags, collapse = ", "), "\n") else "",
    if (nchar(note) > 0) paste0("Note            : ", note, "\n") else "",
    "────────────────────────────\n\n"
  )
  
  cat(log_entry, file = out_log, append = TRUE)
  
  invisible(TRUE)
}
