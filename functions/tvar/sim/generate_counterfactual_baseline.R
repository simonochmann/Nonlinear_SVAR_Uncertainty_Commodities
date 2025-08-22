generate_counterfactual_baseline <- function(model, H, B, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  Sigma_u <- get_resid_cov(model)
  if (is.null(Sigma_u)) {
    warning("[sim] No residual covariance found; using identity draws.")
    k <- length(model$variables)
    Sigma_u <- diag(k)
    dimnames(Sigma_u) <- list(model$variables, model$variables)
  }
  U <- chol(Sigma_u)
  k <- ncol(Sigma_u)
  arr <- array(NA_real_, dim = c(B, H, k), dimnames = list(NULL, NULL, model$variables))
  for (b in 1:B) {
    Z <- matrix(stats::rnorm(H * k), nrow = H, ncol = k)
    arr[b, , ] <- Z %*% t(U)
  }
  list(U = arr)
}
