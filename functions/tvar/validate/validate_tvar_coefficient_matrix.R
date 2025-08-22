# functions/tvar/validate/validate_tvar_coefficient_matrix.R
# Extended validation: dimensions, stability (eigenvalues), NA checks

validate_tvar_coefficient_matrix <- function(A, Sigma = NULL, tol = 1 - 1e-6, expected_k = NULL, expected_p = NULL) {
  if (is.null(A)) stop("A is NULL")
  if (!is.matrix(A)) stop("A must be a matrix")
  k <- if (is.null(Sigma)) NA_integer_ else ncol(Sigma)
  # Infer k,p from A if Sigma not given
  if (is.na(k)) {
    # try square-root inference assuming A = [A1 ... Ap] with k rows
    k <- nrow(A)
  }
  p <- ncol(A) / k
  if (p %% 1 != 0) stop("Columns of A not divisible by k — inconsistent dimensions")
  
  if (!is.null(expected_k) && expected_k != k) warning("expected_k != inferred k")
  if (!is.null(expected_p) && expected_p != p) warning("expected_p != inferred p")
  
  if (any(!is.finite(A))) stop("A contains non-finite values")
  
  # Stability: spectral radius of companion matrix
  comp <- matrix(0, nrow = k * p, ncol = k * p)
  comp[1:k, ] <- A
  if (p > 1) comp[(k + 1):(k * p), 1:(k * (p - 1))] <- diag(k * (p - 1))
  rho <- max(Mod(eigen(comp, only.values = TRUE)$values))
  
  if (rho >= 1 - 1e-10) warning(glue::glue("Unstable VAR dynamics: spectral radius ~ {round(rho, 6)}"))
  
  if (!is.null(Sigma)) {
    if (!is.matrix(Sigma) || nrow(Sigma) != ncol(Sigma)) stop("Sigma must be square matrix")
    if (any(!is.finite(Sigma))) stop("Sigma contains non-finite values")
    if (any(abs(Sigma - t(Sigma)) > 1e-10)) warning("Sigma not exactly symmetric")
  }
  
  return(TRUE)
}