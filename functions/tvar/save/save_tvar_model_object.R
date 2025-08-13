#' Save a TVAR model object with metadata
#' @param model list returned by estimate_tvar_model()
#' @param path  full .rds path
#' @param meta  named list of provenance (optional)
save_tvar_model_object <- function(model, path, meta = NULL) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  obj <- list(
    model = model,
    meta  = meta,
    saved_at = Sys.time()
  )
  saveRDS(obj, path)
  message("Saved TVAR model -> ", path)
  invisible(path)
}