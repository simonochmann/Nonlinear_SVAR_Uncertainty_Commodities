#' Validate Estimated TVAR Object
#'
#' Performs structural checks on the estimated TVAR object: regime coverage,
#' model dimensionality, residual sanity, and coefficient consistency.
#'
#' @param tvar_model A list returned by `estimate_tvar_model()`.
#' @param verbose Logical. If TRUE, prints validation results.
#' @return Invisible TRUE if all checks pass. Throws error otherwise.
validate_estimated_tvar <- function(tvar_model, verbose = TRUE) {
  stopifnot(is.list(tvar_model), !is.null(tvar_model$regimes))
  
  regimes <- tvar_model$regimes
  metadata <- tvar_model$metadata
  vars <- metadata$variables
  
  # 1. Check both regimes are present
  if (length(regimes) < 2) {
    stop("TVAR validation failed: Less than 2 regimes found.")
  }
  
  # 2. Check regime consistency
  for (regime_name in names(regimes)) {
    regime <- regimes[[regime_name]]
    
    # Dimensions
    Y <- regime$Y
    X <- regime$X
    residuals <- regime$residuals
    coefs <- regime$coefficients
    
    if (ncol(Y) != length(vars)) {
      stop(paste("TVAR validation failed:", regime_name, "Y dims incorrect."))
    }
    if (nrow(Y) != nrow(residuals)) {
      stop(paste("TVAR validation failed:", regime_name, "Residuals rows mismatch."))
    }
    if (any(sapply(coefs, function(x) any(is.na(x))))) {
      stop(paste("TVAR validation failed:", regime_name, "NA coefficients found."))
    }
    if (verbose) {
      message(paste0("✓ Regime ", regime_name, ": passed all checks."))
    }
  }
  
  if (verbose) message("All regimes validated successfully.")
  invisible(TRUE)
}