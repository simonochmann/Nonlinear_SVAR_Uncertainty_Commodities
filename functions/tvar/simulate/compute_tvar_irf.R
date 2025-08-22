# functions/tvar/simulate/compute_tvar_irf.R
# Computes regime-specific IRFs for a fitted TVAR model.
# Returns: model with model$irf attached + a structured irf object

compute_tvar_irf <- function(
    model,
    horizon = 12L,
    n_draws = 1000L,
    ci_level = 0.90,
    shock_type = c("unit", "sd"),
    shock_size = 1,
    impulse_variable = NULL,   
    response_variable = NULL,  
    seed = 42L,
    progress = TRUE,
    verbose = TRUE,
    save_dir = NULL            
) {
  stopifnot(is.list(model))
  shock_type <- match.arg(shock_type)
  if (!is.null(seed)) set.seed(seed)
  if (!is.numeric(horizon) || horizon < 1) stop("horizon must be >= 1")
  if (!is.numeric(n_draws) || n_draws < 100) warning("n_draws is small for CI stability")
  if (!is.numeric(ci_level) || ci_level <= 0 || ci_level >= 1) stop("ci_level in (0,1)")
  
  # Extract regimes, coefficients, and names
  regimes <- model$regimes
  if (is.null(regimes$low) || is.null(regimes$high)) stop("model$regimes must contain $low and $high")
  
  Y_low   <- regimes$low$Y
  Y_high  <- regimes$high$Y
  A_low   <- regimes$low$A
  A_high  <- regimes$high$A
  Sigma_l <- regimes$low$Sigma
  Sigma_h <- regimes$high$Sigma
  
  if (is.null(A_low) || is.null(A_high)) stop("coefficient matrices A missing in regimes")
  
  var_names <- colnames(Y_low)
  if (is.null(var_names)) var_names <- paste0("y", seq_len(ncol(Y_low)))
  
  # Select impulses/responses
  impulses <- if (is.null(impulse_variable)) var_names else intersect(impulse_variable, var_names)
  responses <- if (is.null(response_variable)) var_names else intersect(response_variable, var_names)
  if (length(impulses) == 0 || length(responses) == 0) stop("No matching impulse/response variables.")
  
  # Helper to get Cholesky shock scaling
  chol_scale <- function(Sigma, type, size) {
    if (type == "unit") return(diag(ncol(Sigma)) * size)
    if (type == "sd")   return(diag(sqrt(diag(Sigma))) * size)
  }
  
  scale_low  <- chol_scale(Sigma_l, shock_type, shock_size)
  scale_high <- chol_scale(Sigma_h, shock_type, shock_size)
  
  # Core simulator for a given regime
  simulate_irf_regime <- function(A, Sigma, scale_mat, impulses, responses, horizon, n_draws, var_names) {
    k <- ncol(Sigma)
    p <- ncol(A) / k
    # Companion form
    A1 <- matrix(0, nrow = k * p, ncol = k * p)
    A1[1:k, ] <- A
    if (p > 1) A1[(k + 1):(k * p), 1:(k * (p - 1))] <- diag(k * (p - 1))
    
    # Pre-allocate
    res <- array(NA_real_, dim = c(horizon + 1L, length(responses), length(impulses), n_draws),
                 dimnames = list(h = 0:horizon, response = responses, impulse = impulses, draw = NULL))
    
    # Index helpers
    response_idx <- match(responses, var_names)
    impulse_idx  <- match(impulses, var_names)
    
    for (j in seq_along(impulse_idx)) {
      e_j <- rep(0, k)
      e_j[impulse_idx[j]] <- 1
      shock_vec <- as.numeric(scale_mat %*% e_j)
      
      # State starts at zero; IRF measured as deviations
      for (d in seq_len(n_draws)) {
        x <- rep(0, k * p)
        y_path <- matrix(0, nrow = horizon + 1L, ncol = k)
        y_path[1, ] <- rep(0, k)
        
        for (h in 2:(horizon + 1L)) {
          # One-time shock at h = 2 (i.e., period 1)
          eps <- if (h == 2) shock_vec else rep(0, k)
          x <- A1 %*% x + c(eps, rep(0, k * (p - 1)))
          y_path[h, ] <- x[1:k]
        }
        res[, , j, d] <- y_path[, response_idx, drop = FALSE]
      }
    }
    res
  }
  
  if (verbose) cat("── Simulating IRFs (low regime) ──")
  arr_low  <- simulate_irf_regime(A_low,  Sigma_l, scale_low,  impulses, responses, horizon, n_draws, var_names)
  if (verbose) cat("── Simulating IRFs (high regime) ──")
  arr_high <- simulate_irf_regime(A_high, Sigma_h, scale_high, impulses, responses, horizon, n_draws, var_names)
  
  # Summarize draws to median and CI bands
  qlo <- (1 - ci_level) / 2
  qhi <- 1 - qlo
  qfun <- function(x) quantile(x, probs = c(qlo, 0.5, qhi), na.rm = TRUE)
  
  summarize_array <- function(arr) {
    apply(arr, c(1, 2, 3), qfun)  # dims: 3 (q) x (h) x resp x imp
  }
  
  sm_low  <- summarize_array(arr_low)
  sm_high <- summarize_array(arr_high)
  
  irf_obj <- list(
    settings = list(horizon = horizon, n_draws = n_draws, ci_level = ci_level,
                    shock_type = shock_type, shock_size = shock_size, seed = seed,
                    impulses = impulses, responses = responses),
    regimes = list(
      low  = list(draws = arr_low,  summary = sm_low),
      high = list(draws = arr_high, summary = sm_high)
    ),
    var_names = var_names,
    created_at = Sys.time()
  )
  
  # Attach to model as well
  model$irf <- irf_obj
  
  # Optional: write tidy CSVs per impulse-response
  if (!is.null(save_dir)) {
    fs::dir_create(save_dir)
    tidy_write <- function(sm, regime, impulses, responses) {
      for (imp in impulses) for (resp in responses) {
        imp_i <- match(imp, impulses)
        resp_i <- match(resp, responses)
        M <- sm[, , resp_i, imp_i, drop = FALSE]  # q x h x 1 x 1
        df <- data.frame(
          h = 0:horizon,
          q_lo = M[1, , 1, 1],
          q_med = M[2, , 1, 1],
          q_hi = M[3, , 1, 1],
          impulse = imp,
          response = resp,
          regime = regime
        )
        readr::write_csv(df, file.path(save_dir, glue::glue("irf_{regime}_{imp}_to_{resp}.csv")))
      }
    }
    tidy_write(sm_low,  "low",  impulses, responses)
    tidy_write(sm_high, "high", impulses, responses)
  }
  
  return(model)
}