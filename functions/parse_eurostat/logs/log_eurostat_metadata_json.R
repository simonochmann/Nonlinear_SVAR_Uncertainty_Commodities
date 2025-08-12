log_eurostat_metadata_json <- function(df, input, out_dir, verbose = TRUE) {
  suppressPackageStartupMessages({ library(jsonlite) })
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  meta <- list(
    timestamp = as.character(Sys.time()),
    input = input,
    date_start = as.character(min(df$date, na.rm=TRUE)),
    date_end   = as.character(max(df$date, na.rm=TRUE)),
    columns = names(df)
  )
  path <- file.path(out_dir, paste0("ciss_metadata_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".json"))
  jsonlite::write_json(meta, path, pretty = TRUE, auto_unbox = TRUE)
  if (verbose) cat(" Metadata →", path, "\n")
  invisible(path)
}
