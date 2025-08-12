#' Validate a TVAR coefficient matrix
#'
#' @param A A matrix of dimensions (k × k × p) = (k × (k × p))
#' @param expected_k Number of endogenous variables
#' @param expected_p Number of lags
#'
#' @return TRUE if valid; otherwise, stops with an informative error
validate_tvar_coefficient_matrix <- function(A, expected_k = NULL, expected_p = NULL) {
  if (!is.matrix(A)) {
    stop("A is not a matrix.")
  }
  
  dims <- dim(A)
  if (length(dims) != 2) {
    stop("A must be a 2-dimensional matrix.")
  }
  
  k <- dims[1]
  kp <- dims[2]
  
  if (!is.null(expected_k) && k != expected_k) {
    stop(glue::glue("Number of rows in A (={k}) does not match expected_k (={expected_k})"))
  }
  
  if (!is.null(expected_p) && kp != expected_k * expected_p) {
    stop(glue::glue("Number of columns in A (={kp}) does not match expected_k × expected_p (={expected_k}×{expected_p}={expected_k * expected_p})"))
  }
  
  return(TRUE)
}