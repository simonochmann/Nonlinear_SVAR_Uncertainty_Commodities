canonicalize_ordering <- function(ordering, model_vars) {
  if (is.null(ordering) || !length(ordering)) return(model_vars)
  # keep unique user-provided names that are present in model
  ord <- unique(intersect(as.character(ordering), model_vars))
  # append missing vars in model order
  missing <- setdiff(model_vars, ord)
  c(ord, missing)
}
