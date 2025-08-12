log_eurostat_parse_activity <- function(input, output_files, log_path = here::here("logs","eurostat","parse_log.csv"), verbose = TRUE) {
  suppressPackageStartupMessages({ library(dplyr); library(readr) })
  dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)
  row <- tibble::tibble(timestamp = Sys.time(), input = input, outputs = paste(output_files, collapse="; "))
  if (file.exists(log_path)) readr::write_csv(row, log_path, append = TRUE) else readr::write_csv(row, log_path)
  if (verbose) cat(" Log entry →", log_path, "\n")
}
