compute_tvar_irf <- function(
    model,
    horizon = 10,
    n_draws = 1000,
    shock_type = c("unit", "cholesky"),
    shock_size = 1,
    impulse_variable = NULL,
    parallel = FALSE,
    seed = 42,
    verbose = TRUE
) {
  shock_type <- match.arg(shock_type)
  set.seed(seed)
  
  regimes <- c("low", "high")
  irf_list <- list()
  
  for (regime in regimes) {
    if (verbose) cli::cli_alert_info("Simulating IRFs for regime: {regime}")
    
    A <- model$regimes[[regime]]$A
    Sigma <- model$regimes[[regime]]$Sigma
    k <- nrow(Sigma)
    p <- model$metadata$lag
    
    cat("\n--- DEBUG INFO ---\n")
    cat("Regime:", regime, "\n")
    cat("Dimensions of A:", if (!is.null(A)) paste(dim(A), collapse = " x ") else "NULL", "\n")
    cat("Sigma is NULL? ", is.null(Sigma), "\n")
    cat("k:", k, "\n")
    cat("p:", p, "\n")
    cat("k * p:", k * p, "\n")
    cat("ncol(A):", if (!is.null(A)) ncol(A) else "NULL", "\n")
    cat("------------------\n")
    
    if (is.null(p)) stop("Lag order 'p' not found in model metadata.")
    var_names <- colnames(model$regimes[[regime]]$Y)
    
    # Validate A dimensions
    if (!is.matrix(A)) {
      stop(glue::glue("A matrix in regime '{regime}' is not a matrix."))
    }
    if (ncol(A) != k * p) {
      stop(glue::glue("Coefficient matrix A has {ncol(A)} columns but expected {k*p}."))
    }
    
    # Cholesky decomposition if needed
    if (shock_type == "cholesky") {
      tryCatch({
        chol_decomp <- chol(Sigma)
      }, error = function(e) {
        stop(glue::glue("Cholesky decomposition failed for regime '{regime}': {e$message}"))
      })
    }
    
    # Validate impulse variable
    impulse_vars <- if (is.null(impulse_variable)) var_names else {
      if (!(impulse_variable %in% var_names)) {
        stop(glue::glue("Impulse variable '{impulse_variable}' not found in regime '{regime}'."))
      }
      impulse_variable
    }
    
    shock_list <- list()
    for (imp in impulse_vars) {
      shock_vec <- rep(0, k)
      names(shock_vec) <- var_names
      shock_vec[imp] <- shock_size
      
      if (shock_type == "cholesky") {
        shock_matrix <- chol_decomp %*% matrix(shock_vec, ncol = 1)
      } else {
        shock_matrix <- matrix(shock_vec, ncol = 1)
      }
      shock_list[[imp]] <- shock_matrix
    }
    
    # Simulate IRFs
    for (imp in impulse_vars) {
      shock <- shock_list[[imp]]
      Y_irf <- array(0, dim = c(n_draws, k, horizon))
      
      for (d in seq_len(n_draws)) {
        # Initialize Y matrix with enough rows to hold p lags + horizon
        Y <- matrix(0, nrow = horizon + p, ncol = k)
        
        # Insert shock at t = p + 1
        Y[p + 1, ] <- as.vector(shock)
        
        for (t in (p + 2):(horizon + p)) {
          lags <- as.vector(t(Y[(t - 1):(t - p), , drop = FALSE]))
          if (length(lags) != k * p) {
            stop(glue::glue("Lagged vector at t={t} has length {length(lags)} but expected {k * p}"))
          }
          Y[t, ] <- A %*% lags
        }
        
        Y_irf[d, , ] <- Y[(p + 1):(p + horizon), ]
      }
      
      dimnames(Y_irf) <- list(NULL, var_names, seq_len(horizon))
      irf_list[[regime]][[imp]] <- Y_irf
    }
    
    if (verbose) cli::cli_alert_success("IRFs for regime '{regime}' completed.")
  }
  
  # Initialize model$irf if not already
  if (is.null(model$irf)) {
    model$irf <- list()
  }
  
  # Append new IRFs by regime and impulse
  for (regime in names(irf_list)) {
    if (is.null(model$irf[[regime]])) {
      model$irf[[regime]] <- list()
    }
    model$irf[[regime]] <- c(model$irf[[regime]], irf_list[[regime]])
  }
  
  return(model)
}