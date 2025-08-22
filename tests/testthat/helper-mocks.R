suppressPackageStartupMessages({
  library(glue); library(dplyr); library(fs)
})

# Build a tiny, stable mock TVAR object consistent with your functions’ expectations
build_mock_tvar <- function(k = 2, p = 1) {
  set.seed(123)
  # Simple stable VAR coefficients
  A_block <- matrix(c(0.5, 0.1,
                      0.0, 0.4), nrow = k, byrow = TRUE)
  A_low  <- A_block
  A_high <- A_block * 0.8
  
  Sigma_low  <- diag(c(0.04, 0.09))
  Sigma_high <- diag(c(0.05, 0.07))
  
  n_low  <- 30
  n_high <- 30
  coln <- c("oil","value")
  
  Y_low  <- matrix(rnorm(n_low  * k, 0, 0.1), ncol = k);  colnames(Y_low)  <- coln
  Y_high <- matrix(rnorm(n_high * k, 0, 0.1), ncol = k);  colnames(Y_high) <- coln
  
  X_low  <- cbind(value_L1 = rnorm(n_low))
  X_high <- cbind(value_L1 = rnorm(n_high))
  
  list(
    regimes = list(
      low  = list(Y = Y_low,  X = X_low,  A = A_low,  Sigma = Sigma_low),
      high = list(Y = Y_high, X = X_high, A = A_high, Sigma = Sigma_high)
    ),
    threshold_value = list(0),
    metadata = list(dates = seq_len(n_low + n_high))
  )
}

tmp_outdir <- function(sub = NULL) {
  d <- fs::path_temp("tvar_tests")
  fs::dir_create(d)
  if (!is.null(sub)) { d <- fs::path(d, sub); fs::dir_create(d) }
  d
}

# Ensure sources are loaded (use your project structure)
root <- getwd()
source(file.path(root, "functions/tvar/simulate/compute_tvar_irf.R"))
source(file.path(root, "functions/tvar/diagnostics/plot_tvar_irf_regimes.R"))
source(file.path(root, "functions/tvar/validate/validate_tvar_coefficient_matrix.R"))
source(file.path(root, "functions/tvar/run/run_tvar_irf_analysis.R"))
source(file.path(root, "functions/tvar/logs/log_tvar_irf_metadata.R"))
